
module T = struct

  type t = {
    identity : int;
    x_range : int * int;
    y_range : int * int;
  }

end
include T

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
