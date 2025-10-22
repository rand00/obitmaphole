open Cmdliner

let sp = Format.sprintf

let image_file = 
  let doc = "The image to analyze" in
  let docv = "FILE" in
  Arg.(value & opt (some file) None & info ["image"] ~docv ~doc)

let x_range = 
  let doc = "Range of x-values output" in
  let docv = "FLOAT,FLOAT" in
  Arg.(value & opt (some (list float)) None & info ["x-range"] ~docv ~doc)

let y_range = 
  let doc = "Range of y-values output" in
  let docv = "FLOAT,FLOAT" in
  Arg.(value & opt (some (list float)) None & info ["y-range"] ~docv ~doc)

let output =
  (*> goto support GCODE too*)
  (*> goto change this when gcode is implemented*)
  let default_output = `Blobmap in
  let all_variants =
    Output_t.all
    |> CCList.map Output_t.to_string
    |> CCList.map CCString.uppercase_ascii
    |> CCString.concat "|"
  in
  let doc = sp "What to output (one of '%s')" all_variants in
  let docv = "STRING" in
  let format_conv = Arg.conv' (
    Output_t.parse,
    Output_t.pp
  ) in
  Arg.(value & opt format_conv default_output & info ["output"] ~docv ~doc)

(*> goto add param to map center-pct's to x/y ranges (which could e.g. be in mm's)*)
let apply f = 
  let doc = "Find white holes in image and export G-code for CNC milling" in
  let cmd =
    Cmd.v
      Cmd.(info "bitmapholes" ~doc)
      Term.(const f $ image_file $ output $ x_range $ y_range)
  in
  Cmd.(eval cmd |> exit)
