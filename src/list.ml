open Core
open Fragment
open Util

let ends_list_line xs =
  match xs with
  | [] -> Some []
  | ('\n'::rest) -> Some rest
  | _ -> None
;;

let list_line_parser = parse_characters_until (fun x -> is_some ends_list_line);;

(* When parsing list items we parse as a paragraph (to support specials like italics) but then swap out Paragraph for Header *)
let paragraph_inner paragraph = match paragraph with
  | Paragraph inside -> inside
  | _ -> raise_s (sexp_of_string "Paragraph parser did not parse a paragraph")
;;

(* This returns a list of list items where each item is a Fragment.t Fragments *)
let parse_list_inner xs =
  match xs with
  | ('-'::xs) ->
    let rest_of_line, follows = list_line_parser (skip xs) in
    let this_fragment = Fragments (paragraph_inner (Paragraph.parse rest_of_line)) in

    (* The list_line_parser stops at the terminating character, we now need to recurse only after the terminating character *)
    match ends_list_line xs with rest_of_line
                               | Some after -> let rest_of_fragments, follows = parse_list_inner after in (this_fragments::rest_of_fragments, after)
                               | None -> [] (* Something went wrong, terminate the list before this list item *)
                               | _ -> []

(* If at the start of a list, parse it from the markdown, otherwise None *)
let parse xs =
  match parse_list_inner xs with
  | list_items, follows when List.length list_items > 0 -> Some (List list_items, follows)
  | _ -> None
;;
