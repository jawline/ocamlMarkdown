open Core
open Fragment

let list_order_as_html = function
  | Unordered -> "ul"
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
  | Paragraph parts -> sprintf "<p>%s</p>\n" (to_html_list_of_fragments parts)
  | Code code_str -> sprintf "<code><pre>%s</pre></code>" code_str
  | Text text -> text
  | List (list_type, list_items) -> to_html_list list_type list_items
  | Bold t -> sprintf "<b>%s<b>" (to_html t)
  | Italic t -> sprintf "<i>%s</i>" (to_html t)
  | Heading (depth, title) -> sprintf "<h%i>%s</h%i>" depth title depth
  | Link (title, url) -> sprintf "<a href=\"%s\">%s</a>" url title
  | Blockquote t -> sprintf "<blockquote>%s</blockquote>\n" (to_html t)
  | HorizontalRule -> sprintf "<hr/>\n"
;;

(* TODO: to_html tests *)
