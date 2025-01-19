open Core
open Fragment

let html_encode_char c = sprintf "&#%04i;" (Char.to_int c)

(* TODO: This won't work if there's a unicode sequences that happens to collide with these matches in the HTML fragment. *)
let needs_escape c =
  match c with
  | c when Char.to_int c > 0x20 && Char.to_int c <= 0x2F -> true
  | c when Char.to_int c >= 0x3A && Char.to_int c <= 0x40 -> true
  | c when Char.to_int c >= 0x5B && Char.to_int c <= 0x60 -> true
  | c when Char.to_int c > 0x7B && Char.to_int c <= 0x7E -> true
  | _ -> false
;;

let html_encode_string s =
  let rec encode_list = function
    | [] -> []
    | x :: xs when needs_escape x ->
      let encoded = html_encode_char x in
      let encoded = String.to_list encoded in
      List.join [ encoded; encode_list xs ]
    | x :: xs -> x :: encode_list xs
  in
  String.of_char_list (encode_list (String.to_list s))
;;

let list_order_as_html = function
  | Fragment.List_style.Unordered -> "ul"
  | Ordered -> "ol"
;;

let rec to_html (markdown : Fragment.t) : string =
  let to_html_list_of_fragments list_items =
    (* Turn a list of fragments into a HTML source text by calling to_html recursively on all of them and then joining them as a string *)
    String.concat (List.map ~f:(fun f -> to_html f) list_items)
  in
  let to_html_list list_type list_items =
    let outer_type = list_order_as_html list_type in
    (* Map the to_html of each list item with wrapped in an li and then join into a single string *)
    let li_parts =
      String.concat
        ~sep:"\n"
        (List.map ~f:(fun f -> sprintf "<li>%s</li>\n" (to_html f)) list_items)
    in
    sprintf "<%s>\n%s\n</%s>\n" outer_type li_parts outer_type
  in
  match markdown with
  | Fragments parts -> to_html_list_of_fragments parts
  | Paragraph parts ->
    sprintf "<p>%s</p>\n" (String.strip (to_html_list_of_fragments parts))
  | Code { style; code } ->
    (match style with
     | Inline -> sprintf "<code>%s</code>" (html_encode_string (String.strip code))
     | Block ->
       sprintf "<pre><code>%s</code></pre>" (html_encode_string (String.strip code)))
  | Text text -> text
  | List { style; items } -> to_html_list style items
  | Bold t -> sprintf "<b>%s</b>" (to_html t)
  | Italic t -> sprintf "<i>%s</i>" (to_html t)
  | Image { dimensions; description; path } ->
    let dimension_string =
      match dimensions with
      | Original_dimensions -> ""
      | Width width -> sprintf "width=\"%s\" " width
      | Width_and_height (width, height) ->
        sprintf "width=\"%s\" height=\"%s\" " width height
    in
    sprintf "<img %salt=\"%s\" src=\"%s\" />" dimension_string description path
  | Heading { depth; contents } -> sprintf "<h%i>%s</h%i>\n" depth (to_html_list_of_fragments contents) depth
  | Link { description; path } -> sprintf "<a href=\"%s\">%s</a>" path description
  | Blockquote t -> sprintf "<blockquote>%s</blockquote>\n" (to_html t)
  | HorizontalRule -> sprintf "<hr/>\n"
;;

let test markdown = print_endline (Parse.parse markdown |> to_html)

let%expect_test "basic_text" =
  test "hello";
  [%expect {|
    <p>hello</p> |}]
;;

let%expect_test "basic_texts_split_over_two_lines" =
  test "hello\nworld";
  [%expect {|
    <p>hello world</p> |}]
;;

let%expect_test "bold_text" =
  test "he**ll**o";
  [%expect {|
    <p>he<b>ll</b>o</p> |}]
;;

let%expect_test "italic_text" =
  test "he*ll*o";
  [%expect {|
    <p>he<i>ll</i>o</p> |}]
;;

let%expect_test "heading" =
  test "## Heading\n";
  [%expect {|
    <h2><p>Heading</p>
    </h2>
    |}]
;;

let%expect_test "link" =
  test "[Hello](https://example.com)\n";
  [%expect {|
    <p><a href="https://example.com">Hello</a></p> |}]
;;

let%expect_test "basic_blockquote" =
  test ">Hello\n>World";
  [%expect {|
    <blockquote><p>Hello World</p>
    </blockquote> |}]
;;

let%expect_test "multiple paragraphs" =
  test
    "This is the first paragraph. There are multiple sentences.\n\n\
    \  This is the second paragraph. It is split by a newline.\n\n\
    \  This is the third paragraph.";
  [%expect
    {|
    <p>This is the first paragraph. There are multiple sentences.</p>
    <p>This is the second paragraph. It is split by a newline.</p>
    <p>This is the third paragraph.</p> |}]
;;

let%expect_test "image render" =
  test "![desc](test.png)";
  [%expect {| <p><img alt="desc" src="test.png" /></p> |}]
;;

let%expect_test "code_escape" =
  test "``<Hello>``";
  [%expect {| <p><code>&#0060;Hello&#0062;</code></p> |}]
;;

let%expect_test "multi-line code" =
  test
    "This is not code.\n\
    \  ```\n\
     let this_is_code = true in\n\
     print_endline (Bool.to_string this_is_code)\n\
    \  ```\n\
    \  This is not code but is in the same section.\n\
    \ \n\
    \  This is a new section.";
  [%expect
    {|
    <p>This is not code. <pre><code>let this&#0095;is&#0095;code &#0061; true in
    print&#0095;endline &#0040;Bool&#0046;to&#0095;string this&#0095;is&#0095;code&#0041;</code></pre> This is not code but is in the same section.</p>
    <p>This is a new section.</p> |}]
;;

(* TODO: to_html tests are insufficient, missing important cases like Code*)
