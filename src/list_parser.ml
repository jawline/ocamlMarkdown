open Core
open Util

let list_line_parser = parse_characters_until (fun x -> is_some (ends_line x))

(* When parsing list items we parse as a paragraph (to support specials like italics) but then swap out Paragraph for Header *)
let paragraph_inner paragraph =
  match paragraph with
  | Fragment.Paragraph inside -> inside
  | _ -> raise_s (sexp_of_string "Paragraph parser did not parse a paragraph")
;;

(* This returns a list of list items where each item is a Fragment.t Fragments *)
let rec parse_list_inner line_parser chr_pred xs =
  match chr_pred xs with
  | Some xs ->
    let rest_of_line, xs = list_line_parser (skip xs) in
    (match line_parser (skip rest_of_line) with
     | Some (paragraph, _) ->
       let this_fragment = Fragment.Fragments (paragraph_inner paragraph) in
       (* The list_line_parser stops at the terminating character, we now need to recurse only after the terminating character *)
       (match ends_line xs with
        | Some xs ->
          let rest_of_fragments, xs = parse_list_inner line_parser chr_pred xs in
          this_fragment :: rest_of_fragments, xs
        (* Something went wrong, terminate the list before this list item *)
        | _ -> [], xs)
     | None -> [], xs)
  | None -> [], xs
;;

let character_predicate chr = function
  | x :: ' ' :: xs when Char.( = ) x chr -> Some xs
  | _ -> None
;;

let digit_predicate = function
  | '0' .. '9' :: '.' :: ' ' :: xs -> Some xs
  | _ -> None
;;

let parse_list line_parser list_type chr_pred xs =
  match parse_list_inner line_parser chr_pred xs with
  | list_items, follows when List.length list_items > 0 ->
    Some (Fragment.List (list_type, list_items), follows)
  | _ -> None
;;

(* TODO: Nested lists *)

(* If at the start of a list, parse it from the markdown, otherwise None *)
let parse =
  bind_parsers
    [ parse_list Paragraph_parser.parse Unordered (character_predicate '-')
    ; parse_list Paragraph_parser.parse Unordered (character_predicate '*')
    ; parse_list Paragraph_parser.parse Unordered (character_predicate '+')
    ; parse_list Paragraph_parser.parse Ordered digit_predicate
    ]
;;
