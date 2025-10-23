
let sp = Format.sprintf

let safe_height = 15.
let safe_height_for_movement = 2.
let cutting_depth = -2.4

let print ~hole_centers =
  let cnc_setup = sp {|
G21                 ; Set units to millimeters
G90                 ; Use absolute positioning (coordinates are absolute, not relative)
G94                 ; Feedrate is in units per minute

G01 F120.00         ; Set linear feedrate to 120 mm/min

M5                  ; Ensure spindle is stopped
G00 Z%f             ; Rapid move to Z = 15 mm (raise tool to safe height)
G00 X0.0000 Y0.0000 ; Rapid move to origin (X0, Y0)
      
T1                  ; Select tool #1
M6                  ; Execute tool change
M0                  ; Pause program until operator resumes (e.g., after tool change)
G00 Z%f             ; Raise tool again to safe height
      
M03                 ; Start spindle clockwise

G01 F120.00         ; Set feedrate again to 120 mm/min
|} safe_height safe_height
  in
  let cnc_holes =
    hole_centers |> CCList.map (fun (x_center, y_center) ->
      sp {|
G00 X%f Y%f
G01 F60.00
G01 Z%f
G01 F120.00
G00 Z%f
|}
        x_center y_center
        cutting_depth
        safe_height_for_movement
    )
  in
  cnc_setup :: cnc_holes
  |> CCString.concat ""
  |> print_string
