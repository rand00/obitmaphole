open Cmdliner

let sp = Format.sprintf

let image_file = 
  let doc = "The image to analyze" in
  let docv = "FILE" in
  let format_conv = Arg.file in
  Arg.(value & opt (some file) None & info ["socket"] ~docv ~doc)

let apply f = 
  let doc = "Find white holes in image and export G-code for CNC milling" in
  let cmd =
    Cmd.v
      Cmd.(info "bitmapholes" ~doc)
      Term.(const f $ image_file)
  in
  Cmd.(eval cmd |> exit)
