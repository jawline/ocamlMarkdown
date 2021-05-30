type t =
  | Fragments of t list
  | Paragraph of t list
  | Code of string
  | Text of string
  | List of t list
  | Bold of t
  | Italic of t
  | Heading of (int * string)
  | Link of (string * string)
  | Blockquote of t
  | HorizontalRule
[@@deriving show]
