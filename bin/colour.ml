
type rgb = int * int * int

type config = {
  init : rgb;
  step : rgb;
  div  : rgb;
  min  : rgb;
  max  : rgb;
}

let default = {
  init = 0, 0, 20;
  step = 9, 1, 49;
  div  = 40, 89, 1;
  (* div  = 200, 255*2, 1; *)
  min  = 10, 10, 20;
  max  = 80, 30, 100;
}

let r (r, _, _) = r
let g (_, g, _) = g
let b (_, _, b) = b

let rgb_of_blob_id ~config id =
  let c = config in
  let def_i get = (get c.init) + id * (get c.step) in
  let i_r, i_g, i_b = def_i r, def_i g, def_i b in
  let def_channel i get =
    ((get c.min) + (i_r / (get c.div))) mod CCInt.(min 255 (get c.max))
  in
  let r = def_channel i_r r in
  let g = def_channel i_g g in
  let b = def_channel i_b b in
  r, g , b



