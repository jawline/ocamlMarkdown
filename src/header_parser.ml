open Core
open Fragment
open Util

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

let parse_header_starting_with_hash xs =
  match xs with
  | '#' :: _ ->
    let depth, rest = parse_header_count_depth xs in
    let text_list, follows = parse_header_title_text (skip rest) in
    Some (Heading (depth, String.of_char_list text_list), follows)
  | _ -> None
;;

let rec read_entire_line xs =
  match xs with
  | [] -> [], []
  | '\n' :: xs -> [], xs
  | x :: xs ->
    let rest, follows = read_entire_line xs in
    x :: rest, follows
;;

let rec is_header_line count chr xs =
  match xs with
  | [] -> Some (count, [])
  | '\n' :: xs -> Some (count, xs)
  | x :: xs when Char.( = ) x chr -> is_header_line (count + 1) chr xs
  | _ -> None
;;

let parse_header_two_lines chr depth xs =
  let first_line, xs = read_entire_line xs in
  match is_header_line 0 chr xs with
  | Some (count, follows) when count > 1 ->
    Some (Heading (depth, String.of_char_list first_line), follows)
  | _ -> None
;;

let two_line_parser_equals = parse_header_two_lines '=' 1
let two_line_parser_dash = parse_header_two_lines '-' 2

let parse =
  two_line_parser_dash
  |> bind_parser two_line_parser_equals
  |> bind_parser parse_header_starting_with_hash
;;
