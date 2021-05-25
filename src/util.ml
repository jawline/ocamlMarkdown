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
