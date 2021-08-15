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

(* Skip until the next non-whitespace or newline character *)
let rec skip = function
  | ' ' :: xs | '\t' :: xs | '\n' :: xs -> skip xs
  | xs -> xs
;;

(* Skip until the next non-newline character *)
let rec skip_newline = function
  | '\n' :: xs -> skip_newline xs
  | xs -> xs
;;

(* Remove newlines from a list of characters *)
let rec trim_newlines = function
  | [] -> []
  | '\n' :: xs -> trim_newlines xs
  | x :: xs -> x :: trim_newlines xs
;;

(* Replace newlines in a fragment with spaces *)
let rec replace_newlines_with_spaces = function
  | [] -> []
  | '\n' :: xs -> ' ' :: replace_newlines_with_spaces xs
  | x :: xs -> x :: replace_newlines_with_spaces xs
;;

(* Replace sequences of spaces with a single space *)
let rec remove_duplicate_spaces = function
  | [] -> []
  | ' ' :: ' ' :: xs -> remove_duplicate_spaces (' ' :: xs)
  | x :: xs -> x :: remove_duplicate_spaces xs
;;

(* Strip spaces from the beginning of a list of characters *)
let rec strip_beginning chr = function
  | [] -> []
  | x :: xs when Char.( = ) x chr -> strip_beginning chr xs
  | xs -> xs
;;

(* Strip spaces at the end works by reversing the list and then calling strip spaces beginning, reversing the final output.
 * TODO: This is really slow, maybe improve to just an O(n) scan
 *)
let strip_end chr xs = List.rev (strip_beginning chr (List.rev xs))
let strip chr xs = strip_end chr (strip_beginning chr xs)

(* Replace newlines with spaces and then remove duplicate spaces in a paragraph then remove all the spaces toward the beginning and end of the input *)
let sanitize_paragraph xs = remove_duplicate_spaces (replace_newlines_with_spaces xs)

(* Lists are started on * or - or a 0-9 digit. *)
let starts_list = function
 | '-' :: ' ' :: _ -> true
 | '*' :: ' ' :: _ -> true
 | '0' .. '9' :: '.' :: ' ' :: _ -> true
 | _ -> false
;;

(* Returns true of the characters at the start of the list end the current paragraph. This happens either when we reach the end of the document or there are two newlines in a row. *)
let ends_paragraph = function
  (* TODO: skip the whitespace between the first and second newline *)
  | [] | '\n' :: '\n' :: _ -> true
  | '\n' :: xs when starts_list xs -> true
  | _ -> false
;;

(* Returns a new function combining the parsing methods f_a and f_b.
 * The returned function glues f_a and f_b together by executing f_a first and executing f_b only if f_a returns None.
 * If f_a returns Some, then return the result of f_a, otherwise return the result of f_b
 *)
let bind_parser (f_a : parse_method) (f_b : parse_method) xs =
  match f_a xs with
  | Some x -> Some x
  | None -> f_b xs
;;

(* Calls bind parser on a list of parsers, with precedence being given to the first item in the list *)
let rec bind_parsers = function
  (* If it's the last item in the list then just return it *)
  | [ x ] -> x
  (* Bind the next item in the list with the binding of all subsequent items *)
  | x :: xs -> bind_parser x (bind_parsers xs)
  (* The case below should only happen if initially invoked incorrectly *)
  | _ -> raise_s (sexp_of_string "bind_parser cannot be called with an empty list")
;;

(* Returns true if a character is escapable, false otherwise *)
let escape_char = function
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
let take_character = function
  | [] -> None
  | '\\' :: x :: xs when escape_char x -> Some (x, xs)
  | x :: xs -> Some (x, xs)
;;

(* Parse a list of characters until a stopping predicate is satisfied, forms the foundation of parse_text and parse_code *)
let rec parse_characters_until stop_predicate xs =
  if stop_predicate xs
  then [], xs
  else (
    match take_character xs with
    | Some (character, xs) ->
      let following_text, xs = parse_characters_until stop_predicate xs in
      character :: following_text, xs
    (* None implies no more characters, stop here *)
    | None -> [], xs)
;;

(* Predicate evaluations to Some rest if xs is the end of a line *)
let ends_line = function
  | [] -> Some []
  | '\n' :: rest -> Some rest
  | _ -> None
;;
