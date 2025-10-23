open Blob.T

let min_pct_bright = 0.4

let expand_blob ~x ~y ~w ~h ~max_val ~pixels ~blobmap ~identity =
  let rec aux acc_blob (x, y) =
    if x < 0 || x >= w || y < 0 || y >= h then acc_blob else begin
      let is_checked = CCOption.is_some blobmap.(x).(y) in
      if is_checked then acc_blob else begin 
        let pct_bright = float (Image.Pixmap.get pixels x y) /. max_val in
        if pct_bright < min_pct_bright then begin
          blobmap.(x).(y) <- Some `No_blob;
          acc_blob
        end else begin
          let acc_blob = 
            let x_range =
              CCInt.min x (fst acc_blob.x_range),
              CCInt.max x (snd acc_blob.x_range) in
            let y_range =
              CCInt.min y (fst acc_blob.y_range),
              CCInt.max y (snd acc_blob.y_range) in
            {
              acc_blob with
              x_range;
              y_range;
            }
          in 
          blobmap.(x).(y) <- Some (`Blob identity);
          [
            x+1, y;
            x-1, y;
            x  , y+1;
            x  , y-1;
          ] |> CCList.fold_left aux acc_blob
        end
      end
    end
  in
  let init = {
    identity;
    x_range = x, x;
    y_range = y, y;
  }
  in
  aux init (x, y) 

let find_holes ~w ~h ~max_val ~pixels ~blobmap =
  let max_val = float max_val in
  let holes = ref [] in
  for x = 0 to w-1 do
    for y = 0 to h-1 do
      let pct_bright = float (Image.Pixmap.get pixels x y) /. max_val in
      let is_checked = CCOption.is_some blobmap.(x).(y) in
      if pct_bright > min_pct_bright && not is_checked then
        let identity = CCList.length !holes in
        let hole = expand_blob ~x ~y ~w ~h ~max_val ~pixels ~blobmap ~identity in
        holes := hole :: !holes
    done
  done;
  !holes

let filter_holes ~filter_outliers holes =
  let len_holes = CCList.length holes in
  let avg_xdiff, avg_ydiff =
    holes
    |> CCList.fold_left (fun (sum_xdiff, sum_ydiff) hole ->
      let xdiff = snd hole.x_range - fst hole.x_range in
      let ydiff = snd hole.y_range - fst hole.y_range in
      sum_xdiff + xdiff, sum_ydiff + ydiff
    ) (0, 0)
    |> (fun (sum_xdiff, sum_ydiff) ->
      float sum_xdiff /. float len_holes,
      float sum_ydiff /. float len_holes
    )
  in
  holes |> CCList.filter (fun hole ->
    let xdiff = snd hole.x_range - fst hole.x_range in
    let ydiff = snd hole.y_range - fst hole.y_range in
    let xdiff_outlying = float xdiff /. avg_xdiff in
    let ydiff_outlying = float ydiff /. avg_ydiff in
    xdiff_outlying > 0.3 && xdiff_outlying < 3.0 &&
    ydiff_outlying > 0.3 && ydiff_outlying < 3.0
  )

let extract_single_pixmap image =
  match image.Image.pixels with
  | Grey pixmap
  | GreyA (pixmap, _)
  | RGB (pixmap, _, _)
  | RGBA (pixmap, _, _, _) -> pixmap

let main image_file output x_range y_range filter_outliers =
  let xmin_out, xmax_out = match x_range with
    | None -> 0., 1.
    | Some (xmin :: xmax :: []) -> xmin, xmax
    | _ -> failwith "You can only pass two values to x-range"
  in
  let ymin_out, ymax_out = match y_range with
    | None -> 0., 1.
    | Some (ymin :: ymax :: []) -> ymin, ymax
    | _ -> failwith "You can only pass two values to y-range"
  in
  match image_file with
  | None -> failwith "You need to pass an image to analyze"
  | Some image_file -> 
    let image = ImageLib_unix.openfile image_file in
    (*> Note: as we only analyze b/w images, the red channel is enough for
        non-greytone bitmaps*)
    let pixels = extract_single_pixmap image in
    let blobmap =
      CCArray.init image.width (fun _x ->
        CCArray.init image.height (fun _y -> None))
    in
    let holes =
      find_holes
        ~w:image.width
        ~h:image.height
        ~max_val:image.max_val
        ~pixels
        ~blobmap
      |> filter_holes ~filter_outliers
    in
    match output with
    | `Blobmap ->
      Format.eprintf ".. writing blobmap (if file doesn't exist already)\n%!";
      Blobmap.to_image blobmap
      |> Blob.Image.add_centers ~holes
      |> ImageLib_unix.writefile "blobmap.png"
    | `Centers ->
      Format.eprintf ".. printing centers\n%!";
      holes |> CCList.iter (fun hole ->
        let xmin, xmax = hole.x_range in
        let xdiff = xmax - xmin in
        let center_x_pct =
          (float xmin +. float xdiff /. 2.) /. float image.width in
        let center_x =
          Gg.Float.remap ~x0:0. ~x1:1. ~y0:xmin_out ~y1:xmax_out center_x_pct in
        let ymin, ymax = hole.y_range in
        let ydiff = ymax - ymin in
        let center_y_pct =
          (float ymin +. float ydiff /. 2.) /. float image.height in
        let center_y =
          Gg.Float.remap ~x0:0. ~x1:1. ~y0:ymin_out ~y1:ymax_out center_y_pct in
        Format.printf "hole-%d: %f, %f\n%!" hole.identity center_x center_y;
      )
    | `Gcode ->
      failwith "todo"

let () = Cli.apply main
  
