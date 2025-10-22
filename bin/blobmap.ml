
type checked_pixel = [
  | `No_blob
  | `Blob of int (*identity*)
]

type t = checked_pixel option array array

let rgb_of_blob_id id =
  let step = 80 in
  let i = (id + 1) * step in
  let b = i mod 255 in
  let g = ((i / 255) * step) mod 255 in
  let r = (((i * step) / 255) * step) mod 255 in
  r, g , b

let to_image (blobmap:t) =
  let w, h = CCArray.length blobmap, CCArray.length blobmap.(0) in
  let image = Image.create_rgb ~alpha:false ~max_val:255 w h in
  Image.fill_rgb image 0 0 0;
  for x = 0 to w-1 do
    for y = 0 to h-1 do
      match blobmap.(x).(y) with
      | None 
      | Some `No_blob -> ()
      | Some (`Blob blob_id) ->
        let r, g, b = rgb_of_blob_id blob_id in
        (* Format.eprintf "DEBUG: writing pixel (%d,%d,%d) for blob-id (%d)\n%!" *)
        (*   r g b *)
        (*   blob_id; *)
        Image.write_rgb image x y r g b
    done;
  done;
  image
