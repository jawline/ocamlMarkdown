open Core
open Util

(* TODO: I'm pretty sure I already wrote this function elsewhere, eliminate the duplication *)
let is_character chr xs =
  match xs with
  | x :: rest when Char.( = ) x chr -> Some rest
  | _ -> None
;;

let starts_link_title = is_character '['
let ends_link_title = is_character ']'
let starts_link_url = is_character '('
let ends_link_url = is_character ')'
let starts_link xs = starts_link_title xs

let fetch_between start_predicate end_predicate xs =
  let inside_reader = parse_characters_until (fun xs -> is_some (end_predicate xs)) in
  match start_predicate xs with
  | Some xs ->
    let inner, xs = inside_reader xs in
    (match end_predicate xs with
    | Some xs -> Some (inner, xs)
    | None -> None)
  | None -> None
;;

let fetch_link_title = fetch_between starts_link_title ends_link_title
let fetch_link_url = fetch_between starts_link_url ends_link_url

let parse_link xs =
  match fetch_link_title xs with
  | Some (title_contents, xs) ->
    (match fetch_link_url xs with
    | Some (url_contents, xs) ->
      Some
        ( Fragment.Link
            (String.of_char_list title_contents, String.of_char_list url_contents)
        , xs )
    | None -> None)
  | None -> None
;;

(* Images have almost identical schema to links so we hijack the link parser and convert it to an image fragment instead *)
let parse_image xs =
  match xs with
  | '!' :: xs ->
    (match parse_link xs with
    | Some (Fragment.Link (alt_text, url), xs) -> Some (Fragment.Image (alt_text, url), xs)
    | _ -> None)
  | _ -> None
;;

let parse = parse_image |> bind_parser parse_link
