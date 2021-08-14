open Core

type list_type =
  | Ordered
  | Unordered
[@@deriving show, sexp]

type code_type =
  | Inline
  | Block
[@@deriving show, sexp]

type image_dimensions =
  | OriginalDimension
  | Width of string
  | WidthHeight of (string * string)
[@@deriving show, sexp]

type t =
  | Fragments of t list
  | Paragraph of t list
  | Code of (code_type * string)
  | Text of string
  | List of (list_type * t list)
  | Bold of t
  | Italic of t
  | Heading of (int * string)
  | Link of (string * string)
  | Image of (image_dimensions * string * string)
  | Blockquote of t
  | HorizontalRule
[@@deriving show, sexp]
