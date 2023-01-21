open Core
open Parse
open Fragment

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

let multiline_block_quotes =
  "> # Hello World\n\
   > This is still the block quotes\n\
   >\n\
   >\n\
   > This is another paragraph in it"
;;

let test_multiline_header = "Hello World\n===========\nWhat's up?"
let test_deeper_multiline_header = "Hello World\n---\nWhat's up?"

let test_basic_ordered_list =
  "1. Hello world\n2. What's up?\n3. I don't know\n3. This is still valid"
;;

let test_horizontal_rules =
  "\nThis is some text\n\n---------------------\n\nThis is other text"
;;

let test_not_hr = "--"

let%test "test_paragraph" =
  match parse test_with_paragraph_and_eol with
  | Fragments [ Paragraph [ Text x ] ] when String.( = ) x test_with_paragraph_and_eol ->
    true
  | _ -> false
;;

let%test "two_paragraphs" =
  match parse test_two_paragraphs with
  | Fragments
      [ Paragraph
          [ Text "This is paragraph one. It's a great paragraph. Many things happen." ]
      ; Paragraph [ Text "This is paragraph two. Ditto." ]
      ] -> true
  | _ -> false
;;

let%test "two_paragraphs_bold" =
  let test_two_paragraphs_with_bold =
    "This is paragraph one. It's a **great** paragraph. Many things happen.\n\n\
     This is paragraph two. Ditto."
  in
  let parsed = parse test_two_paragraphs_with_bold in
  match parsed with
  | Fragments
      [ Paragraph
          [ Text "This is paragraph one. It's a "
          ; Bold (Fragments [ Text "great" ])
          ; Text " paragraph. Many things happen."
          ]
      ; Paragraph [ Text "This is paragraph two. Ditto." ]
      ] -> true
  | _ -> false
;;

let%test "basic_list" =
  let test_basic_list = "Test\n- Hello\n- World\n- What\n" in
  let parsed_list = parse test_basic_list in
  match parsed_list with
  | Fragments
      [ Paragraph [ Text "Test" ]
      ; List
          { style = Unordered
          ; items =
              [ Fragments [ Text "Hello" ]
              ; Fragments [ Text "World" ]
              ; Fragments [ Text "What" ]
              ]
          }
      ] -> true
  | _ -> false
;;

let%test "basic_ordered_list" =
  let parsed_list = parse test_basic_ordered_list in
  match parsed_list with
  | Fragments
      [ List
          { style = Ordered
          ; items =
              [ Fragments [ Text "Hello world" ]
              ; Fragments [ Text "What's up?" ]
              ; Fragments [ Text "I don't know" ]
              ; Fragments [ Text "This is still valid" ]
              ]
          }
      ] -> true
  | _ -> false
;;

let%test "header" =
  let parsed_header = parse test_with_header in
  match parsed_header with
  | Fragments
      [ Heading { depth = 1; text = "Hello World" }
      ; Paragraph [ Text "This is a paragraph" ]
      ] -> true
  | _ -> false
;;

let%test "simple_code_and_text" =
  let parsed_code = parse "Goodbye ``Hello``" in
  match parsed_code with
  | Fragments [ Paragraph [ Text "Goodbye "; Code { style = Inline; code = "Hello" } ] ]
    -> true
  | _ -> false
;;

let%test "simple_code" =
  let parsed_code = parse "``Hello``" in
  match parsed_code with
  | Fragments [ Paragraph [ Code { style = Inline; code = "Hello" } ] ] -> true
  | _ -> false
;;

let%test "block_of_code" =
  let parsed_code = parse "```Hello```" in
  match parsed_code with
  | Fragments [ Paragraph [ Code { style = Block; code = "Hello" } ] ] -> true
  | _ -> false
;;

let%test "deeper_header" =
  let parsed_header = parse test_with_deeper_header in
  match parsed_header with
  | Fragments
      [ Heading { depth = 3; text = "Hello World" }
      ; Paragraph [ Text "This is a paragraph" ]
      ] -> true
  | _ -> false
;;

let%test "mutliline_header" =
  let header = parse test_multiline_header in
  match header with
  | Fragments
      [ Heading { depth = 1; text = "Hello World" }; Paragraph [ Text "What's up?" ] ] ->
    true
  | _ -> false
;;

let%test "deeper_mutliline_header" =
  let header = parse test_deeper_multiline_header in
  match header with
  | Fragments
      [ Heading { depth = 2; text = "Hello World" }; Paragraph [ Text "What's up?" ] ] ->
    true
  | _ -> false
;;

let%test "test_simple_bold" =
  let bold_block = parse test_with_bold in
  match bold_block with
  | Fragments [ Paragraph [ Bold (Fragments [ Text "Bold Test" ]) ] ] -> true
  | _ -> false
;;

let%test "unclosed_bold" =
  let bold_block = parse unclosed_bold in
  match bold_block with
  (* TODO: This test fragments the Text up into two sections but we could actually merge any contiguous Text(...) blocks together, this is just an artifact of the way we force forward progress in the parser. *)
  | Fragments [ Paragraph [ Text "Hel"; Text "*lo" ] ] -> true
  | _ -> false
;;

let%test "simple_italic" =
  let italic_block = parse test_with_italic in
  match italic_block with
  | Fragments [ Paragraph [ Italic (Fragments [ Text "Italic Test" ]) ] ] -> true
  | _ -> false
;;

let%test "simple_escape" =
  let escaped_block = parse "\\- Hello World" in
  match escaped_block with
  | Fragments [ Paragraph [ Text "- Hello World" ] ] -> true
  | _ -> false
;;

let%test "test_weirdly_formatted_italics_in_bold" =
  let bold_block = parse test_with_incorrect_bold in
  match bold_block with
  | Fragments
      [ Paragraph
          [ Bold (Fragments [ Italic (Fragments [ Text "Bold Te" ]); Text "st" ]) ]
      ] -> true
  | _ -> false
;;

let%test "bold_within_word" =
  let bold_block = parse bold_part_of_word in
  match bold_block with
  | Fragments [ Paragraph [ Text "Su"; Bold (Fragments [ Text "pe" ]); Text "r" ] ] ->
    true
  | _ -> false
;;

let%test "horizontal_rule" =
  let hr_block = parse test_horizontal_rules in
  match hr_block with
  | Fragments
      [ Paragraph [ Text "This is some text" ]
      ; HorizontalRule
      ; Paragraph [ Text "This is other text" ]
      ] -> true
  | _ -> false
;;

let%test "not_horizontal_rule" =
  let hr_block = parse test_not_hr in
  match hr_block with
  | Fragments [ Paragraph [ Text "--" ] ] -> true
  | _ -> false
;;

let%test "link" =
  let link_block = parse "[Website](http://url.com)" in
  match link_block with
  | Fragments [ Paragraph [ Link { description = "Website"; path = "http://url.com" } ] ]
    -> true
  | _ -> false
;;

let%test "blockquote" =
  let block_quote = parse "> Hello World" in
  match block_quote with
  (* We expect a trailing space since newlines get substituted for spaces (and stripped in to_html) *)
  | Fragments [ Blockquote (Fragments [ Paragraph [ Text "Hello World " ] ]) ] -> true
  | _ -> false
;;

let%test "multiline_blockquote" =
  let block_quote = parse multiline_block_quotes in
  match block_quote with
  | Fragments
      [ Blockquote
          (Fragments
            [ Heading { depth = 1; text = "Hello World" }
            ; Paragraph [ Text "This is still the block quotes" ]
            ; Paragraph [ Text "This is another paragraph in it " ]
            ])
      ] -> true
  | _ -> false
;;

let%test "simple_image" =
  let image_md = parse "![Description](image.png)" in
  match image_md with
  | Fragments
      [ Paragraph
          [ Image
              { dimensions = Original_dimensions
              ; description = "Description"
              ; path = "image.png"
              }
          ]
      ] -> true
  | _ -> false
;;

let%test "stacked_images" =
  let image_md = parse "![Description](image.png)\n![Other](other.png)\n" in
  match image_md with
  | Fragments
      [ Paragraph
          [ Image
              { dimensions = Original_dimensions
              ; description = "Description"
              ; path = "image.png"
              }
          ; Text " "
          ; Image
              { dimensions = Original_dimensions
              ; description = "Other"
              ; path = "other.png"
              }
          ; Text " "
          ]
      ] -> true
  | _ -> false
;;

let%test "image with width" =
  let image_md = parse "![Description](image.png =500x)" in
  match image_md with
  | Fragments
      [ Paragraph
          [ Image
              { dimensions = Width "500"
              ; description = "Description"
              ; path = "image.png"
              }
          ]
      ] -> true
  | _ -> false
;;

let%test "image with width and height" =
  let image_md = parse "![Description](image.png =500x400)" in
  match image_md with
  | Fragments
      [ Paragraph
          [ Image
              { dimensions = Width_and_height ("500", "400")
              ; description = "Description"
              ; path = "image.png"
              }
          ]
      ] -> true
  | _ -> false
;;

let%test "link_in_text" =
  let link_block = parse "Hello, I'm contacting you from [Website](http://url.com)" in
  match link_block with
  | Fragments
      [ Paragraph
          [ Text "Hello, I'm contacting you from "
          ; Link { description = "Website"; path = "http://url.com" }
          ]
      ] -> true
  | _ -> false
;;
