
let sp = Format.sprintf

type t = [
  | `Blobmap
  | `Gcode
]

let all = [ `Blobmap; `Gcode ]

let parse : string -> (t, string) result = fun str ->
  match CCString.lowercase_ascii str with
  | "blobmap" -> Ok `Blobmap
  | "gcode" -> Ok `Gcode
  | _ -> Error (sp "Unknown output format '%s'" str)

let to_string : t -> string = function
  | `Blobmap -> "blobmap"
  | `Gcode -> "gcode"

let pp fmt v = Format.fprintf fmt "%s" (to_string v)
