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
let starts_image_title = is_character '!'
let starts_link xs = Option.first_some (starts_link_title xs) (starts_image_title xs)

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
             { description = String.of_char_list title_contents
             ; path = String.of_char_list url_contents
             }
         , xs )
     | None -> None)
  | None -> None
;;

let convert_to_image description url_section =
  match String.split_on_chars url_section ~on:[ ' ' ] with
  | path :: width_part :: _ ->
    let dimensions =
      if String.is_prefix width_part ~prefix:"="
      then (
        match String.split_on_chars ~on:[ 'x' ] (String.drop_prefix width_part 1) with
        | [ width; height ] ->
          if String.length height = 0
          then Fragment.Image_dimensions.Width width
          else Width_and_height (width, height)
        | _ -> Original_dimensions)
      else Original_dimensions
    in
    Fragment.Image { dimensions; description; path }
  | _ ->
    Fragment.Image { dimensions = Original_dimensions; description; path = url_section }
;;

(* Images have almost identical schema to links so we hijack the link parser and convert it to an image fragment instead *)
let parse_image xs =
  match xs with
  | '!' :: xs ->
    (match parse_link xs with
     | Some (Fragment.Link { description; path }, xs) ->
       Some (convert_to_image description path, xs)
     | _ -> None)
  | _ -> None
;;

let parse = parse_image |> bind_parser parse_link
