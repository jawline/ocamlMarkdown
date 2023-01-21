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
  | Heading { depth; text } -> sprintf "<h%i>%s</h%i>\n" depth text depth
  | Link { description; path } -> sprintf "<a href=\"%s\">%s</a>" description path
  | Blockquote t -> sprintf "<blockquote>%s</blockquote>\n" (to_html t)
  | HorizontalRule -> sprintf "<hr/>\n"
;;

exception ToHtmlTestingError of string

let%test "basic_text" =
  let parse = Parse.parse in
  let output = to_html (parse "hello") in
  if String.( = ) output "<p>hello</p>\n" then true else raise (ToHtmlTestingError output)
;;

let%test "basic_texts_split_over_two_lines" =
  let parse = Parse.parse in
  let output = to_html (parse "hello\nworld") in
  if String.( = ) output "<p>hello world</p>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

let%test "bold_text" =
  let parse = Parse.parse in
  let output = to_html (parse "he**ll**o") in
  if String.( = ) output "<p>he<b>ll</b>o</p>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

let%test "italic_text" =
  let parse = Parse.parse in
  let output = to_html (parse "he*ll*o") in
  if String.( = ) output "<p>he<i>ll</i>o</p>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

let%test "heading" =
  let parse = Parse.parse in
  let output = to_html (parse "## Heading\n") in
  if String.( = ) output "<h2>Heading</h2>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

let%test "link" =
  let parse = Parse.parse in
  let output = to_html (parse "[Hello](https://example.com)\n") in
  if String.( = ) output "<p><a href=\"https://example.com\">Hello</a></p>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

let%test "basic_blockquote" =
  let parse = Parse.parse in
  let output = to_html (parse ">Hello\n>World") in
  if String.( = ) output "<blockquote><p>Hello World</p>\n</blockquote>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

let%test "two_paragraphs" =
  let parse = Parse.parse in
  let output = to_html (parse "# Heading\nHello World\n\nGoodbye World") in
  if String.( = ) output "<h1>Heading</h1>\n<p>Hello World</p>\n<p>Goodbye World</p>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

let%test "image render" =
  let parse = Parse.parse in
  let output = to_html (parse "![desc](test.png)") in
  if String.( = ) output "<p><img alt=\"desc\" src=\"test.png\" /></p>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

let%test "code_escape" =
  let parse = Parse.parse in
  let output = to_html (parse "``<Hello>``") in
  if String.( = ) output "<p><code>&#0060;Hello&#0062;</code></p>\n"
  then true
  else raise (ToHtmlTestingError output)
;;

(* TODO: to_html tests are insufficient, missing important cases like Code*)
