
type blob = {
  identity : int;
  x_range : int * int;
  y_range : int * int;
}

let min_pct_bright = 0.4

let expand_blob ~x ~y ~w ~h ~max_val ~pixels ~blobmap ~identity =
  let rec aux ~x ~y acc_blob =
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
          ] |> CCList.fold_left (fun acc_blob (x, y) ->
            aux ~x ~y acc_blob
          ) acc_blob
        end
      end
    end
  in
  aux ~x ~y {
    identity;
    x_range = x, x;
    y_range = y, y;
  }

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

let extract_single_pixmap image =
  match image.Image.pixels with
  | Grey pixmap
  | GreyA (pixmap, _)
  | RGB (pixmap, _, _)
  | RGBA (pixmap, _, _, _) -> pixmap

let main image_file output =
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
    in
    match output with
    | `Blobmap ->
      Format.eprintf ".. writing blobmap (if file doesn't exist already)\n%!";
      Blobmap.to_image blobmap |> ImageLib_unix.writefile "blobmap.png"
    | `Centers ->
      Format.eprintf ".. printing centers\n%!";
      holes |> CCList.iter (fun hole ->
        let xmin, xmax = hole.x_range in
        let xdiff = xmax - xmin in
        let center_x_pct = (float xmin +. float xdiff /. 2.) /. float image.width in
        let ymin, ymax = hole.y_range in
        let ydiff = ymax - ymin in
        let center_y_pct = (float ymin +. float ydiff /. 2.) /. float image.height in
        Format.printf "hole-%d: %f, %f\n%!" hole.identity center_x_pct center_y_pct;
      )
    | `Gcode ->
      failwith "todo"

let () = Cli.apply main
  
