open Core
open Util
open Fragment

(* TODO: Escaping *)

(* This method generates predicates for the special text sections like ** for bold and _ for italics. It will generate a method that returns true if the same character is repeated count times *)
let repeated_character (expected : char) (count : int) =
  let rec verify xs i =
    if i = count
    then Some xs
    else (
      match xs with
      | x :: xs when Char.( = ) x expected -> verify xs (i + 1)
      | _ -> None)
  in
  function
  | xs -> verify xs 0
;;

(* Bold text is started and ended by '**' *)
let bold_predicate = repeated_character '*' 2
let bold_predicate_underscore = repeated_character '-' 2

(* Italics are started and ended by a *. Italics should be checked only after bold since the _great_ language of markdown overloads the symbol *)
let italic_predicate = repeated_character '*' 1
let italic_predicate_underscore = repeated_character '_' 1

(* Code is started by `Hello` ``Helllo`` or ```Hello``` *)
let code_predicate = repeated_character '`' 1
let code_predicate_double = repeated_character '`' 2
let code_predicate_triple = repeated_character '`' 3

(* If we are at a paragraph ending sequence or any of the bold / italic / code predicates are satisfied then the text parsing is terminated and the next fragment (potentially code, bold, new paragraph) is parsed *)
let terminates_text xs =
  ends_paragraph xs || is_some (bold_predicate xs) || is_some (italic_predicate xs) || is_some (code_predicate xs)
;;

(* Parse a list of characters until a stopping predicate is satisfied, forms the foundation of parse_text and parse_code *)
let rec parse_text_method stop_predicate xs = match xs with
    | [] -> [], xs
    | xs when stop_predicate xs -> [], xs
    | x :: xs ->
      let rest, follows = parse_text_method stop_predicate xs in
      x :: rest, follows
;;

(* this reads characters from the stream until a sequence that terminates a contiguous text block is reached *)
let parse_text xs =
  let parse_text_inner = parse_text_method terminates_text
  in
  (*
    We force forward progress in the parser by always taking at least one character if we fall through to parse_text
    Otherwise we might get stuck trying to parse an unclosed bold, italic, code block etc *)
  match xs with
  | [] -> None
  | x :: xs ->
    let parsed_text, follows = parse_text_inner xs in
    Some (Text (String.of_char_list (x :: parsed_text)), follows)
;;

(* this reads out a series of characters between ` `` and ``` opening blocks as code segments *)
let parse_code_core predicate xs =
  let parse_code_inner = parse_text_method (fun xs -> is_some (predicate xs)) in
  match predicate xs with
  | Some xs -> (
    let (code, rest) = parse_code_inner xs in
    match predicate rest with
    | Some after_code -> Some (Code (String.of_char_list code), after_code)
    | None -> None
  )
  | None -> None
;;

(* Generate a parsing method for each of the three code start predicates. They are added in reverse order so the longer options take precedence, otherwise ```Hello``` would evaluate to `` `Hello` ``*)
let code_parser = bind_parser (bind_parser (parse_code_core code_predicate_triple) (parse_code_core code_predicate_double)) (parse_code_core code_predicate);;

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

(* parse_formatted_section forms the foundation of the special text parsers (Like Bold, Italic) where the paragraph parser needs to recurse on itself but with an additional terminating predicate (I.e, now terminate at __ rather than just \n\n). We split this logic out into a series of functions with their recursors or continuations as methods to reduce repeated code.
  predicate is the predicate that opens and close this special block (e.g, ** for Bold or * for Italic)
  recursor is the method to be used to take the inner xs and return a list of inner fragments (i.e, given a list of characters after the opening ** recursor should process all characters until the closing ** and return as a Fragment.t list)
  wrap takes the list of fragments returned by parser and wraps it in a single Fragment.t, i.e, wrap x for the bold parser will wrap the list as (Bold x)
*)
let parse_formatted_section predicate recurser wrap =
  let parse_formatted_inner xs =
    parse_paragraph_contents
      (function
        | test_input -> is_some (predicate test_input) || ends_paragraph test_input)
      recurser
      xs
  in
  paragraph_format_parser predicate predicate parse_formatted_inner wrap
;;

let rec parse_paragraph_fragment xs =
  (* There are two situations where we parse text as Bold, '__' or '**', in this case we recurse (since bold text can be additionally formatted) using our parse_formatted_section generator
      We generate an individual bold parsing function for __ and ** and then bind them together *)
  let bold_wrap = function
    | xs -> Bold (Fragments xs)
  in
  let assembled_bold_parser =
    bind_parser
      (parse_formatted_section bold_predicate parse_paragraph_fragment bold_wrap)
      (parse_formatted_section
         bold_predicate_underscore
         parse_paragraph_fragment
         bold_wrap)
  in
  (* We do the same for italics, which are begun by a single * or _.
   * Bold takes precedence (so ** will begin a bold, not open and close an italic)
   * If three control characters appear in a row, the inner content will be both bold and italic, with bold wrapping the italic. For example ***Hello*** or __*Hello*__.
  *)
  let italic_wrap = function
    | xs -> Italic (Fragments xs)
  in
  let assembled_italic_parser =
    bind_parser
      (parse_formatted_section italic_predicate parse_paragraph_fragment italic_wrap)
      (parse_formatted_section
         italic_predicate_underscore
         parse_paragraph_fragment
         italic_wrap)
  in

  (* We combine the bold and italics parsers with bold taking precedence *)
  let special_text_parser = bind_parser assembled_bold_parser assembled_italic_parser in
  let special_text_parser = bind_parser special_text_parser code_parser in

  (* Finally this is combined with the fallback text parser that forces forward progress (if no rule can be satisfied, we assume it is just normal text and consume at least one character as text *)
  let combined_parser = bind_parser special_text_parser parse_text in
  combined_parser xs
;;

let parse_paragraph xs =
  let contents, follows =
    parse_paragraph_contents ends_paragraph parse_paragraph_fragment xs
  in
  Some (Paragraph contents, follows)
;;
