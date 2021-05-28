open Core
open Fragment
open Util

let sprintf = Printf.sprintf
let skip = Util.skip

type t = Fragment.t

let parse_fragment =
  bind_parsers [ Header.parse_header; List_parser.parse; Paragraph.parse ]
;;

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
;;

let parse (json : string) : t = Fragments (parse_fragments (String.to_list json))
let test_with_header = "\n\n# Hello World\n\n\nThis is a paragraph"
let test_with_deeper_header = "\n\n### Hello World\n\n\nThis is a paragraph"
let test_with_italic = "*Italic Test*"
let test_with_bold = "**Bold Test**"
let test_with_incorrect_bold = "***Bold Te*st**"
let bold_part_of_word = "Su**pe**r"
let unclosed_bold = "Hel*lo"

let test_with_paragraph_and_eol =
  "Hello World. This is a test paragraph that does not end in a newline."
;;

let test_two_paragraphs =
  "This is paragraph one. It's a great paragraph. Many things happen.\n\n\
   This is paragraph two. Ditto."
;;

let test_two_paragraphs_with_bold =
  "This is paragraph one. It's a **great** paragraph. Many things happen.\n\n\
   This is paragraph two. Ditto."
;;

let test_multiline_header = "Hello World\n===========\nWhat's up?"
let test_deeper_multiline_header = "Hello World\n---\nWhat's up?"
let test_basic_list = "\n- Hello\n- World\n- What\n"

let%test "test_paragraph" =
  match parse test_with_paragraph_and_eol with
  | Fragments [ Paragraph [ Text x ] ] when String.( = ) x test_with_paragraph_and_eol ->
    true
  | _ -> false
;;

let%test "two_paragraphs" =
  match parse test_two_paragraphs with
  | Fragments [ Paragraph [ Text x ]; Paragraph [ Text y ] ]
    when String.( = )
        x
        "This is paragraph one. It's a great paragraph. Many things happen."
      && String.( = ) y "This is paragraph two. Ditto." -> true
  | _ -> false
;;

let%test "two_paragraphs_bold" =
  match parse test_two_paragraphs_with_bold with
  | Fragments
      [ Paragraph [ Text x; Bold (Fragments [ Text y ]); Text z ]; Paragraph [ Text q ] ]
    when String.( = ) x "This is paragraph one. It's a "
      && String.( = ) y "great"
      && String.( = ) z " paragraph. Many things happen."
      && String.( = ) q "This is paragraph two. Ditto." -> true
  | _ -> false
;;

let%test "basic_list" =
  let parsed_list = parse test_basic_list in
  match parsed_list with
  | Fragments
      [ List [ Fragments [ Text x ]; Fragments [ Text y ]; Fragments [ Text z ] ] ]
    when String.( = ) x "Hello" && String.( = ) y "World" && String.( = ) z "What" -> true
  | _ -> false
;;

let%test "header" =
  let parsed_header = parse test_with_header in
  match parsed_header with
  | Fragments [ Heading (depth, x); Paragraph [ Text y ] ]
    when depth = 1 && String.( = ) x "Hello World" && String.( = ) y "This is a paragraph"
    -> true
  | _ -> false
;;

let%test "simple_code_and_text" =
  let parsed_code = parse "Goodbye ``Hello``" in
  match parsed_code with
  | Fragments [ Paragraph [ Text x; Code y ] ]
    when String.( = ) x "Goodbye " && String.( = ) y "Hello" -> true
  | _ -> false
;;

let%test "simple_code" =
  let parsed_code = parse "``Hello``" in
  match parsed_code with
  | Fragments [ Paragraph [ Code y ] ] when String.( = ) y "Hello" -> true
  | _ -> false
;;

let%test "deeper_header" =
  let parsed_header = parse test_with_deeper_header in
  match parsed_header with
  | Fragments [ Heading (depth, x); Paragraph [ Text y ] ]
    when depth = 3 && String.( = ) x "Hello World" && String.( = ) y "This is a paragraph"
    -> true
  | _ -> false
;;

let%test "mutliline_header" =
  let header = parse test_multiline_header in
  match header with
  | Fragments [ Heading (depth, x); Paragraph [ Text y ] ]
    when depth = 1 && String.( = ) x "Hello World" && String.( = ) y "What's up?" -> true
  | _ -> false
;;

let%test "deeper_mutliline_header" =
  let header = parse test_deeper_multiline_header in
  match header with
  | Fragments [ Heading (depth, x); Paragraph [ Text y ] ]
    when depth = 2 && String.( = ) x "Hello World" && String.( = ) y "What's up?" -> true
  | _ -> false
;;

let%test "test_simple_bold" =
  let bold_block = parse test_with_bold in
  match bold_block with
  | Fragments [ Paragraph [ Bold (Fragments [ Text x ]) ] ]
    when String.( = ) x "Bold Test" -> true
  | _ -> false
;;

let%test "unclosed_bold" =
  let bold_block = parse unclosed_bold in
  match bold_block with
  (* TODO: This test fragments the Text up into two sections but we could actually merge any contiguous Text(...) blocks together, this is just an artifact of the way we force forward progress in the parser. *)
  | Fragments [ Paragraph [ Text x; Text y ] ]
    when String.( = ) x "Hel" && String.( = ) y "*lo" -> true
  | _ -> false
;;

let%test "simple_italic" =
  let italic_block = parse test_with_italic in
  match italic_block with
  | Fragments [ Paragraph [ Italic (Fragments [ Text x ]) ] ]
    when String.( = ) x "Italic Test" -> true
  | _ -> false
;;

let%test "test_weirdly_formatted_italics_in_bold" =
  let bold_block = parse test_with_incorrect_bold in
  match bold_block with
  | Fragments [ Paragraph [ Bold (Fragments [ Italic (Fragments [ Text x ]); Text y ]) ] ]
    when String.( = ) x "Bold Te" && String.( = ) y "st" -> true
  | _ -> false
;;

let%test "bold_within_word" =
  let bold_block = parse bold_part_of_word in
  match bold_block with
  | Fragments [ Paragraph [ Text start; Bold (Fragments [ Text middle ]); Text endw ] ]
    when String.( = ) start "Su" && String.( = ) middle "pe" && String.( = ) endw "r" ->
    true
  | _ -> false
;;
