open Core
open Util

(* Pull out a list of lines beginning with the character '>' but removing the '>' from each line, stops at the end of the file or the first line that doesn't start with a > *)
let rec extract_blockquotes_lines (xs : char list) : string list * char list =
  match xs with
  | '>' :: xs ->
    let line, xs = parse_characters_until (fun xs -> is_some (ends_line xs)) xs in
    let line = String.of_char_list line in
    let lines, xs = extract_blockquotes_lines (skip_newline xs) in
    line :: lines, xs
  | _ -> [], xs
;;

(* The blockquotes parser works by pulling out all of the block-quoted lines (beginning with a >) and then running the markdown parser recursively on them.
   To do this we pass a reference to the markdown parser function to the blockquotes parser when we bind it, allowing it to recurse on itself (otherwise there would be a disallowed cyclical reference between methods)
 *)
let parse markdown_parser xs =
  match xs with
  | '>' :: xs ->
    let blockquotes_section, xs = extract_blockquotes_lines xs in
    Some (Fragment.Blockquote (markdown_parser (String.concat blockquotes_section)), xs)
  | _ -> None
;;
