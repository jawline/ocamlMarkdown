open Core
open Util
open Fragment

(* TODO: Escaping *)

(* Bold text is started and ended by '**' *)
let bold_predicate xs =
  match xs with
  | '*' :: '*' :: after_end -> Some after_end
  | _ -> None
;;

(* Italtics are started and ended by a *. Italics should be checked only after bold since the _great_ language of markdown overloads the symbol *)
let italic_predicate xs =
  match xs with
  | '*' :: after_end -> Some after_end
  | _ -> None
;;

(* If we are at a paragraph ending sequence or any of the bold / italic / code predicates are satisfied then the text parsing is terminated and the next fragment (potentially code, bold, new paragraph) is parsed *)
let terminates_text xs =
  ends_paragraph xs || is_some (bold_predicate xs) || is_some (italic_predicate xs)
;;

(* this reads characters from the stream until a sequence that terminates a contiguous text block is reached *)
let parse_text xs =
  let rec parse_text_inner xs =
    match xs with
    | [] -> [], xs
    | xs when terminates_text xs -> [], xs
    | x :: xs ->
      let rest, follows = parse_text_inner xs in
      x :: rest, follows
  in
  let parsed_text, follows = parse_text_inner xs in
  Some (Text (String.of_char_list parsed_text), follows)
;;

let rec parse_paragraph_contents ends_predicate fragment_parser xs =
  let recurse new_thing follows =
    let rest_of_paragraph, follows =
      parse_paragraph_contents ends_predicate fragment_parser follows
    in
    new_thing :: rest_of_paragraph, follows
  in
  if ends_predicate xs
  then [], xs
  else (
    match fragment_parser xs with
    | Some (fragment, follows) -> recurse fragment follows
    | None -> [], xs)
;;

(*
    This method generates parsing methods for bold and italics using a predicate to decide if we have hit the end.
    start_predicate and end_predicate are functions that take a string and return Some with the remaining string after the terminating characters if satisfied, or None if they are not satisfied.
    wrap is a function that takes
    *)
let paragraph_format_parser
    (start_predicate : char list -> char list option)
    (end_predicate : char list -> char list option)
    parse_method
    (wrap : Fragment.t list -> Fragment.t)
  = function
    | xs ->
      (match start_predicate xs with
       | Some beginning_of_content ->
         (* Check if we can parse the content, if that does not fail then wrap it in Fragment.t using the wrap predicate *)
         let content, rest = parse_method beginning_of_content in
         (match end_predicate rest with
          | Some rest -> Some (wrap content, rest)
          | None -> None)
       | None -> None)
;;

let rec parse_paragraph_fragment xs =
  let parse_bold_inner xs =
    parse_paragraph_contents
      (function
        | test_input -> is_some (bold_predicate test_input) || ends_paragraph test_input)
      parse_paragraph_fragment
      xs
  in
  let parse_bold =
    paragraph_format_parser bold_predicate bold_predicate parse_bold_inner (function
        | xs -> Bold (Fragments xs))
  in
  let parse_italic_inner xs =
    parse_paragraph_contents
      (function
        | test_input -> is_some (italic_predicate test_input) || ends_paragraph test_input)
      parse_paragraph_fragment
      xs
  in
  let parse_italic =
    paragraph_format_parser italic_predicate italic_predicate parse_italic_inner (function
        | xs -> Italic (Fragments xs))
  in
  let special_text_parser = bind_parser parse_bold parse_italic in
  let combined_parser = bind_parser special_text_parser parse_text in
  combined_parser xs
;;

let parse_paragraph xs =
  let contents, follows =
    parse_paragraph_contents ends_paragraph parse_paragraph_fragment xs
  in
  Some (Paragraph contents, follows)
;;
