open Core

(*
 * We construct the fragment parser by glueing together separate parsers that may either consume
 * a portion of the string and return a fragment + the rest of the source, or return None indicating that
 * they aren't applicable for this part of the source
 *
 * This type signature must be adhered to by any method which is used in this step.
 *)
type parse_method = char list -> (Fragment.t * char list) option

(* This exception is raised if the source cannot be parsed for any reason, with string explaining the failure *)
exception ParsingError of string

let rec skip xs =
  match xs with
  | ' ' :: xs | '\t' :: xs | '\n' :: xs -> skip xs
  | _ -> xs
;;

(* Returns true of the characters at the start of the list end the current paragraph. This happens either when we reach the end of the document or there are two newlines in a row. *)
let ends_paragraph xs =
  match xs with
  | [] | '\n' :: '\n' :: _ ->
    true (* TODO: skip the whitespace between the first and second newline *)
  | _ -> false
;;

(* Returns a new function combining the parsing methods f_a and f_b.
 * The returned function glues f_a and f_b together by executing f_a first and executing f_b only if f_a returns None.
 * If f_a returns Some, then return the result of f_a, otherwise return the result of f_b
*)
let bind_parser (f_a : parse_method) (f_b : parse_method) = function
  | xs ->
    (match f_a xs with
     | Some x -> Some x
     | None -> f_b xs)
;;

(* Calls bind parser on a list of parsers, with precedence being given to the first item in the list *)
let rec bind_parsers xs =
  match xs with
  | [ x ] -> x
  | x :: xs -> bind_parser x (bind_parsers xs)
  (* The case below should only happen if initially invoked incorrectly *)
  | _ -> raise_s (sexp_of_string "bind_parser cannot be called with an empty list")
;;

(* Returns true if a character is escapable, false otherwise *)
let escape_char chr =
  match chr with
  | '\\'
  | '*'
  | '+'
  | '-'
  | '`'
  | '{'
  | '}'
  | '<'
  | '>'
  | '#'
  | '('
  | ')'
  | '_'
  | '!'
  | '|' -> true
  | _ -> false
;;

(* Takes a text character, deals with escaping *)
let take_character xs =
  match xs with
  | [] -> None
  | ('\\'::x::xs) when escape_char x -> Some (x, xs)
  | (x::xs) -> Some (x, xs)
;;

(* Parse a list of characters until a stopping predicate is satisfied, forms the foundation of parse_text and parse_code *)
let rec parse_characters_until stop_predicate xs =
  if stop_predicate xs then [], xs else
  match take_character xs with
  | Some (character, xs) ->
    let following_text, xs = parse_characters_until stop_predicate xs in
    character::following_text, xs
  (* None implies no more characters, stop here *)
  | None -> [], xs
;;
