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

let min_pct_brightness = 
  let doc = "The brightness cutoff for distinguishing blobs from background. \
             Note that currently it's only the red channel of RGB images that is \
             checked." in
  let docv = "FLOAT" in
  let min_pct_bright = 0.4 in
  Arg.(value & opt float min_pct_bright & info ["min-pct-brightness"] ~docv ~doc)

let dont_filter_outliers = 
  let doc = "Toggle filtering of outliers" in
  Arg.(value & flag & info ["dont-filter-outliers"] ~doc)

let no_blobmap_crosses = 
  let doc = "Toggle adding crosses to blobmap for visualizing boundingbox/center \
             of blobs" in
  Arg.(value & flag & info ["no-blobmap-crosses"] ~doc)

let output =
  let default_output = `Gcode in
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

let apply f = 
  let doc = "Find light holes in dark image and export G-code for CNC milling" in
  let cmd =
    Cmd.v
      Cmd.(info "bitmapholes" ~doc)
      Term.(const f
            $ image_file
            $ output
            $ x_range
            $ y_range
            $ dont_filter_outliers
            $ no_blobmap_crosses
            $ min_pct_brightness
      )
  in
  Cmd.(eval cmd |> exit)
