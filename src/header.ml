open Core
open Fragment

let skip = Util.skip

let rec parse_header_count_depth xs =
  match xs with
  | '#' :: xs ->
    let count, rest = parse_header_count_depth xs in
    count + 1, rest
  | _ -> 0, xs
;;

let rec parse_header_title_text xs =
  match xs with
  | [] | '\n' :: _ -> [], xs
  | x :: xs ->
    let rest, follows = parse_header_title_text xs in
    x :: rest, follows
;;

let parse_header xs =
  match xs with
  | '#' :: _ ->
    let depth, rest = parse_header_count_depth xs in
    let text_list, follows = parse_header_title_text (skip rest) in
    Some (Heading (depth, String.of_char_list text_list), follows)
  | _ -> None
;;
