type t = Fragment.t [@@deriving show]

let parse = Parse.parse
let to_html = To_html.to_html
