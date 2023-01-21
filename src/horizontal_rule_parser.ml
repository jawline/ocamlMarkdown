open Core

(* If a line contains only a given character return the first character after the end of the line, otherwise return None *)
let rec line_contains_only chr count = function
  (* We only accept it as a HR if there are three or more in a row *)
  | '\n' :: xs -> if count >= 3 then Some xs else None
  | x :: xs when Char.( = ) x chr -> line_contains_only chr (count + 1) xs
  | _ -> None
;;

(* Checks a series of possible horizontal rule methods and returns a horizontal rule if it's a match.
   If any rules return Some then the first to return Some is returned, otherwise None is returned *)
let rec check_hr hr_list xs =
  match hr_list with
  | [] -> None
  | f :: hr_list ->
    (match f 0 xs with
     | Some xs -> Some (Fragment.HorizontalRule, xs)
     | None -> check_hr hr_list xs)
;;

let parse =
  check_hr [ line_contains_only '*'; line_contains_only '-'; line_contains_only '_' ]
;;
