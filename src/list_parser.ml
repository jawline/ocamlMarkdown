open Core
open Util

(* TODO: Nested lists *)

(* Unordered lists are lines prefixed by a single character - + or * and a space *)
let unordered_list_predicate chr = function
  | x :: ' ' :: xs when Char.( = ) x chr -> Some xs
  | _ -> None
;;

(* Unordered lists are a series of lines prefixed by a digit 0..9 a period (.) and then a space. The digits themselves don't matter - a list of 1. 1. 1. Will be rendered as 1. 2. 3. *)
let ordered_list_predicate = function
  | '0' .. '9' :: '.' :: ' ' :: xs -> Some xs
  | _ -> None
;;

(* This method is called after the text that begins the list item (e.g, the 1. or * ).
   This reads the characters until the end of the line or EOF *)
let list_line_parser = parse_characters_until (fun x -> is_some (ends_line x))

(* When parsing list items we parse as a paragraph (to support specials like italics) but then swap out Paragraph for Header *)
let paragraph_inner = function
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

let parse_list line_parser list_type chr_pred xs =
  match parse_list_inner line_parser chr_pred xs with
  | list_items, follows when List.length list_items > 0 ->
    Some (Fragment.List (list_type, list_items), follows)
  | _ -> None
;;

(* If at the start of a list, parse it from the markdown, otherwise None *)
let parse =
  bind_parsers
    [ parse_list Paragraph_parser.parse Unordered (unordered_list_predicate '-')
    ; parse_list Paragraph_parser.parse Unordered (unordered_list_predicate '*')
    ; parse_list Paragraph_parser.parse Unordered (unordered_list_predicate '+')
    ; parse_list Paragraph_parser.parse Ordered ordered_list_predicate
    ]
;;
