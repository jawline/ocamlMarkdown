type t =
  | Fragments of t list
  | Paragraph of t list
  | Text of string
  | Bold of t
  | Italic of t
  | Heading of (int * string)
[@@deriving show]
