open Cmdliner

let sp = Format.sprintf

let image_file = 
  let doc = "The image to analyze - this input is required." in
  let docv = "FILE" in
  Arg.(value & opt (some file) None & info ["image"] ~docv ~doc)

let x_range = 
  let doc = "Range of x-values output. This is e.g. relevant for gcode output." in
  let docv = "FLOAT,FLOAT" in
  Arg.(value & opt (some (list float)) None & info ["x-range"] ~docv ~doc)

let y_range = 
  let doc = "Range of y-values output. This is e.g. relevant for gcode output." in
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

let glitch_mode = 
  let doc = "Toggle glitch-mode for blobmap generation (: You'll probably only \
             find this creatively useful, and is intended for blobmaps - see \
             --output parameter." in 
  Arg.(value & flag & info ["glitch-mode"] ~doc)

let output =
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

let y_dir =
  let default = `Up in
  let all_variants =
    Iter_direction_t.all
    |> CCList.map Iter_direction_t.to_string
    |> CCList.map CCString.uppercase_ascii
    |> CCString.concat "|"
  in
  let doc = sp "What direction to iterate y in (one of '%s').\
                This is only relevant for glitch-mode."
      all_variants
  in
  let docv = "STRING" in
  let format_conv = Arg.conv' (
    Iter_direction_t.parse,
    Iter_direction_t.pp
  ) in
  Arg.(value & opt format_conv default & info ["y-dir"] ~docv ~doc)

let x_dir =
  let default = `Up in
  let all_variants =
    Iter_direction_t.all
    |> CCList.map Iter_direction_t.to_string
    |> CCList.map CCString.uppercase_ascii
    |> CCString.concat "|"
  in
  let doc = sp "What direction to iterate x in (one of '%s').\
                This is only relevant for glitch-mode."
      all_variants
  in
  let docv = "STRING" in
  let format_conv = Arg.conv' (
    Iter_direction_t.parse,
    Iter_direction_t.pp
  ) in
  Arg.(value & opt format_conv default & info ["x-dir"] ~docv ~doc)

let iter_y_before_x = 
  let doc = "Toggle the iteration-order when finding blobs. \
             This is only relevant for glitch-mode." in
  Arg.(value & flag & info ["iter-y-before-x"] ~doc)
    
let blob_dir_weights =
  let default = [ 1.; 1.; 1.; 1. ] in
  let doc = "Blob iteration direction weights for calculating \
             probabilities, in the order: x,-x,y,-y. Total of weights \
             doesn't have to add up to a specific number - they are relative.
             This is only relevant for glitch-mode." in
  let docv = "FLOAT,FLOAT,FLOAT,FLOAT" in
  Arg.(value & opt (list float) default & info ["blob-direction-weights"] ~docv ~doc)

let blob_stop_chance =
  let default = 0. in
  let doc = "The chance of blob stopping expanding. It always stops when \
             hitting edges of image or other blobs in glitch-mode. \
             This is only relevant for glitch-mode." in
  let docv = "FLOAT" in
  Arg.(value & opt float default & info ["blob-stop-chance"] ~docv ~doc)

let colour_init =
  let doc = "Sets the initial RGB colour for automatic colour-generation. \
             These values wrap when getting > 255." in
  let docv = "INT,INT,INT" in
  Arg.(value & opt (some (list int)) None & info ["colour-init"] ~docv ~doc)

let colour_step =
  let doc = "Sets the RGB colour step for automatic colour-generation. \
             Big steps lead to more variation in colour-channel." in
  let docv = "INT,INT,INT" in
  Arg.(value & opt (some (list int)) None & info ["colour-step"] ~docv ~doc)

let colour_div =
  let doc = "Sets the divisor per RGB channel for automatic colour-generation. \
             This makes the respective channel change more slowly. As we work with \
             integer division, this parameter doesn't have the same semantics as \
             '--colour-step'." in
  let docv = "INT,INT,INT" in
  Arg.(value & opt (some (list int)) None & info ["colour-div"] ~docv ~doc)

let colour_min =
  let doc = "Sets the minimum values per RGB channel for automatic colour-generation, \
             i.e. this limits the possible output-value of the generated colour." in
  let docv = "INT,INT,INT" in
  Arg.(value & opt (some (list int)) None & info ["colour-min"] ~docv ~doc)

let colour_max =
  let doc = "Sets the maximum values per RGB channel for automatic colour-generation, \
             i.e. this limits the possible output-value of the generated colour." in
  let docv = "INT,INT,INT" in
  Arg.(value & opt (some (list int)) None & info ["colour-max"] ~docv ~doc)

let apply f = 
  let doc = "Find light holes in dark images and export G-code for CNC milling... \
             or exploit the blob-finding algorithm creatively in glitch-mode!" in
  let cmd =
    Cmd.v
      Cmd.(info "obitmaphole" ~doc)
      Term.(const f
            $ image_file
            $ output
            $ x_range
            $ y_range
            $ dont_filter_outliers
            $ no_blobmap_crosses
            $ min_pct_brightness
            $ colour_init
            $ colour_step
            $ colour_div
            $ colour_min
            $ colour_max
            $ glitch_mode
            $ x_dir
            $ y_dir
            $ blob_dir_weights
            $ blob_stop_chance
            $ iter_y_before_x
      )
  in
  Cmd.(eval cmd |> exit)
