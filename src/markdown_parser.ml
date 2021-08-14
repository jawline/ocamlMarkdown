type t = Fragment.t [@@deriving show, sexp]

let parse = Parse.parse
let to_html = To_html.to_html
