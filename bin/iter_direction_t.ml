
let sp = Format.sprintf

type t = [
  | `Up
  | `Down
]

let all = [ `Up; `Down ]

let parse : string -> (t, string) result = fun str ->
  match CCString.lowercase_ascii str with
  | "up" -> Ok `Up
  | "down" -> Ok `Down
  | _ -> Error (sp "Unknown iteration direction '%s'" str)

let to_string : t -> string = function
  | `Up -> "up"
  | `Down -> "down"

let pp fmt v = Format.fprintf fmt "%s" (to_string v)
