
let sp = Format.sprintf

type t = [
  | `Blobmap
  | `Centers
  | `Gcode
]

let all = [ `Blobmap; `Centers; `Gcode ]

let parse : string -> (t, string) result = fun str ->
  match CCString.lowercase_ascii str with
  | "blobmap" -> Ok `Blobmap
  | "centers" -> Ok `Centers
  | "gcode" -> Ok `Gcode
  | _ -> Error (sp "Unknown output format '%s'" str)

let to_string : t -> string = function
  | `Blobmap -> "blobmap"
  | `Centers -> "centers"
  | `Gcode -> "gcode"

let pp fmt v = Format.fprintf fmt "%s" (to_string v)
