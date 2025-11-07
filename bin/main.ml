open Blob.T

let ncons vs acc = CCList.fold_right CCList.cons vs acc

let shuffle_head_by_weight ~total_weight list =
  let random_v = Random.float total_weight in
  let rec aux acc_weight acc_list = function
    | [] -> CCList.rev acc_list
    | (weight, _v as v_wrap) :: rest ->
      let acc_weight' = acc_weight +. weight in
      if CCFloat.Infix.(acc_weight < random_v && random_v <= acc_weight') then
        v_wrap :: (CCList.rev acc_list @ rest)
      else
        let acc_list = v_wrap :: acc_list in
        aux acc_weight' acc_list rest
  in
  aux 0. [] list

let shuffle_by_weight list =
  let total_weight =
    list |> CCList.fold_left (fun acc (w, _) -> acc +. w) 0.
  in
  let rec shuffle list = match shuffle_head_by_weight ~total_weight list with
    | [] 
    | _ :: [] as l -> l
    | head :: tail -> head :: shuffle tail
  in
  shuffle list

module Mode = struct

  module Normal = struct

    let expand_blob
        ~min_pct_bright
        ~x ~y ~w ~h
        ~max_val
        ~pixels
        ~blobmap
        ~identity
      = 
      let rec aux acc_blob = function
        | [] -> acc_blob
        | (x, y) :: pixel_queue ->
          if x < 0 || x >= w || y < 0 || y >= h then
            aux acc_blob pixel_queue
          else begin
            let is_checked = CCOption.is_some blobmap.(x).(y) in
            if is_checked then
              aux acc_blob pixel_queue
            else begin 
              let pct_bright = float (Image.Pixmap.get pixels x y) /. max_val in
              if pct_bright < min_pct_bright then begin
                blobmap.(x).(y) <- Some `No_blob;
                aux acc_blob pixel_queue
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
                let pixel_queue =
                  (x+1, y) ::
                  (x-1, y) ::
                  (x  , y+1) ::
                  (x  , y-1) ::
                  pixel_queue
                in
                aux acc_blob pixel_queue
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
      aux init [ x, y ]

    let find_holes
        ~min_pct_bright
        ~w ~h
        ~max_val
        ~pixels
        ~blobmap
      =
      let max_val = float max_val in
      let holes = ref [] in
      for x = 0 to w-1 do
        for y = 0 to h-1 do
          let pct_bright = float (Image.Pixmap.get pixels x y) /. max_val in
          let is_checked = CCOption.is_some blobmap.(x).(y) in
          if pct_bright > min_pct_bright && not is_checked then
            let identity = CCList.length !holes in
            let hole = expand_blob
                ~min_pct_bright
                ~x ~y ~w ~h
                ~max_val
                ~pixels
                ~blobmap
                ~identity
            in
            holes := hole :: !holes
        done
      done;
      !holes
        
  end

  module Glitch = struct
    
    let expand_blob
        ~min_pct_bright
        ~x ~y ~w ~h
        ~max_val
        ~pixels
        ~blobmap
        ~identity 
        ~blob_dir_weights
      = 
      let rec aux acc_blob = function
        | [] -> acc_blob
        | (x, y) :: pixel_queue ->
          if x < 0 || x >= w || y < 0 || y >= h then
            (*> Note: interesting glitch-art from  this (instead of rec call) (:
                .. combine with different orders of pixels added to pixel_queue
            *)
            acc_blob 
          else begin
            let is_checked = CCOption.is_some blobmap.(x).(y) in
            if is_checked then
              aux acc_blob pixel_queue
            else begin 
              let pct_bright = float (Image.Pixmap.get pixels x y) /. max_val in
              if pct_bright < min_pct_bright then begin
                blobmap.(x).(y) <- Some `No_blob;
                aux acc_blob pixel_queue
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
                let pixel_queue =
                  let points_to_add =
                    [
                      x+1, y;
                      x-1, y;
                      x  , y+1;
                      x  , y-1;
                    ]
                    |> CCList.combine blob_dir_weights
                    |> shuffle_by_weight
                    |> CCList.map (fun (_weight, move) -> move)
                  in
                  ncons points_to_add pixel_queue
                in
                aux acc_blob pixel_queue
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
      aux init [ x, y ]

    let find_holes
        ~min_pct_bright
        ~w ~h
        ~max_val
        ~pixels
        ~blobmap
        ~x_dir
        ~y_dir
        ~blob_dir_weights
      =
      let max_val = float max_val in
      let holes = ref [] in
      let iterate ~x ~y =
        let pct_bright = float (Image.Pixmap.get pixels x y) /. max_val in
        let is_checked = CCOption.is_some blobmap.(x).(y) in
        if pct_bright > min_pct_bright && not is_checked then
          let identity = CCList.length !holes in
          let hole = expand_blob
              ~min_pct_bright
              ~x ~y ~w ~h
              ~max_val
              ~pixels
              ~blobmap
              ~identity
              ~blob_dir_weights
          in
          holes := hole :: !holes
      in
      begin match x_dir, y_dir with
        | `Up, `Up -> 
          for x = 0 to w-1 do
            for y = 0 to h-1 do
              iterate ~x ~y
            done
          done
        | `Down, `Up -> 
          for x = w-1 downto 0 do
            for y = 0 to h-1 do
              iterate ~x ~y
            done
          done
        | `Down, `Down -> 
          for x = w-1 downto 0 do
            for y = h-1 downto 0 do
              iterate ~x ~y
            done
          done
        | `Up, `Down -> 
          for x = 0 to w-1 do
            for y = h-1 downto 0 do
              iterate ~x ~y
            done
          done
      end;
      !holes

  end

end

let filter_holes ~height holes =
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
    let test = 
      xdiff_outlying > 0.3 && xdiff_outlying < 3.0 &&
      ydiff_outlying > 0.3 && ydiff_outlying < 3.0
    in
    if not test then begin
      Format.eprintf "\
Warning: excluded hole-%d because of being an outlier 
    bounding-box (in pixels):
        lower left corner: (%d, %d)
        width x height: %d x %d\
\n%!"
        hole.identity
        (fst hole.x_range)
        (*> Note: remapping reverse y-dimension*)
        (height - snd hole.y_range)
        xdiff
        ydiff
    end;
    test
  )

let extract_single_pixmap image =
  match image.Image.pixels with
  | Grey pixmap
  | GreyA (pixmap, _)
  | RGB (pixmap, _, _)
  | RGBA (pixmap, _, _, _) -> pixmap

let main
    image_file
    output
    x_range
    y_range
    dont_filter_outliers
    no_blobmap_crosses
    min_pct_brightness
    glitch_mode
    x_dir
    y_dir
    blob_dir_weights
  =
  (* Printexc.record_backtrace true; *)
  let xmin_out, xmax_out = match x_range with
    | None -> begin match output with
      | `Gcode -> failwith "For G-code output you will need to specify x/y-range - see --help"
      | _ -> 0., 1.
    end 
    | Some (xmin :: xmax :: []) -> xmin, xmax
    | _ -> failwith "You can only pass two values to x-range"
  in
  let ymin_out, ymax_out = match y_range with
    | None -> begin match output with
      | `Gcode -> failwith "For G-code output you will need to specify x/y-range - see --help"
      | _ -> 0., 1.
    end 
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
    Format.eprintf ".. finding holes\n%!";
    let holes = match glitch_mode with
      | false ->
        Mode.Normal.find_holes
          ~w:image.width
          ~h:image.height
          ~max_val:image.max_val
          ~min_pct_bright:min_pct_brightness
          ~pixels
          ~blobmap
      | true -> 
        Mode.Glitch.find_holes
          ~w:image.width
          ~h:image.height
          ~max_val:image.max_val
          ~min_pct_bright:min_pct_brightness
          ~pixels
          ~blobmap
          ~x_dir
          ~y_dir
          ~blob_dir_weights
    in
    let holes =
      if dont_filter_outliers then holes else begin
        Format.eprintf ".. filtering outlier-holes\n%!";
        holes |> filter_holes ~height:image.height
      end
    in
    match output with
    | `Blobmap ->
      Format.eprintf ".. writing blobmap\n%!";
      Blobmap.to_image blobmap
      |> (fun img ->
        if no_blobmap_crosses then img else Blob.Image.add_centers ~holes img)
      |> ImageLib_unix.writefile "blobmap.png"
    | `Centers ->
      Format.eprintf ".. printing centers\n%!";
      holes |> CCList.iter (fun hole ->
        let center_x, center_y =
          hole |> Blob.to_center
            ~image
            ~xmin_out ~xmax_out
            ~ymin_out ~ymax_out
        in
        Format.printf "hole-%d: %f, %f\n%!" hole.identity center_x center_y;
      )
    | `Gcode ->
      Format.eprintf ".. generating G-code\n%!";
      let hole_centers = holes |> CCList.map (
        Blob.to_center
          ~image
          ~xmin_out ~xmax_out
          ~ymin_out ~ymax_out
      ) in
      Gcode.print ~hole_centers

let () = Cli.apply main
  
