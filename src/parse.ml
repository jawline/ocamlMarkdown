open Core
open Fragment
open Util

let rec parse (markdown : string) : t =
  let parse_fragment =
    bind_parsers
      [ Header_parser.parse
      ; List_parser.parse
      ; Horizontal_rule_parser.parse
      ; Blockquotes_parser.parse parse
      ; Paragraph_parser.parse
      ]
  in
  let rec parse_fragments xs =
    match skip xs with
    | [] -> []
    | xs ->
      (match parse_fragment xs with
      | Some (fragment, follows) -> fragment :: parse_fragments follows
      | None ->
        raise
          (ParsingError
             (sprintf
                "Parsing error: we did not reach the end of the file but cannot find a \
                 matching rule at '%s'"
                (String.of_char_list xs))))
  in
  Fragments (parse_fragments (String.to_list markdown))
;;
