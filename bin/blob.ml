
module T = struct

  type t = {
    identity : int;
    x_range : int * int;
    y_range : int * int;
  }

end
include T

let to_center ~image ~xmin_out ~xmax_out ~ymin_out ~ymax_out hole =
  let xmin, xmax = hole.x_range in
  let xdiff = xmax - xmin in
  let center_x = float xmin +. float xdiff /. 2. in
  let center_x_remapped =
    center_x |> Gg.Float.remap
      ~x0:0.
      ~x1:(float @@ image.Image.width -1)
      ~y0:xmin_out
      ~y1:xmax_out 
  in
  let ymin, ymax = hole.y_range in
  let ydiff = ymax - ymin in
  let center_y = float ymin +. float ydiff /. 2. in
  let center_y_remapped =
    center_y |> Gg.Float.remap
      ~x0:0.
      ~x1:(float @@ image.Image.height -1)
      ~y0:ymin_out
      ~y1:ymax_out 
  in
  center_x_remapped, center_y_remapped

module Image = struct

  let add_centers ~holes image =
    let image = Image.copy image in
    holes |> CCList.iter (fun hole ->
      let xmin, xmax = hole.x_range in
      let xdiff = xmax - xmin in
      let x_center =
        (float xmin +. float xdiff /. 2.) |> truncate
      in
      let ymin, ymax = hole.y_range in
      let ydiff = ymax - ymin in
      let y_center =
        (float ymin +. float ydiff /. 2.) |> truncate
      in
      for x = xmin to xmax do
        Image.write_rgb image x y_center 255 255 255;
      done;
      for y = ymin to ymax do
        Image.write_rgb image x_center y 255 255 255;
      done;
    );
    image

end
