type list_type =
  | Ordered
  | Unordered
[@@deriving show]

type t =
  | Fragments of t list
  | Paragraph of t list
  | Code of string
  | Text of string
  | List of (list_type * t list)
  | Bold of t
  | Italic of t
  | Heading of (int * string)
  | Link of (string * string)
  | Blockquote of t
  | HorizontalRule
[@@deriving show]
