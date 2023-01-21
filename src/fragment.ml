open Core

module List_style = struct
  type t =
    | Ordered
    | Unordered
  [@@deriving show, sexp]
end

module Code_style = struct
  type t =
    | Inline
    | Block
  [@@deriving show, sexp]
end

module Image_dimensions = struct
  type t =
    | Original_dimensions
    | Width of string
    | Width_and_height of (string * string)
  [@@deriving show, sexp]
end

type t =
  | Fragments of t list
  | Paragraph of t list
  | Code of
      { style : Code_style.t
      ; code : string
      }
  | Text of string
  | List of
      { style : List_style.t
      ; items : t list
      }
  | Bold of t
  | Italic of t
  | Heading of (int * string)
  | Link of (string * string)
  | Image of
      { dimensions : Image_dimensions.t
      ; description : string
      ; path : string
      }
  | Blockquote of t
  | HorizontalRule
[@@deriving show, sexp]
