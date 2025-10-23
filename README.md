# `obitmaphole`: find the holes, export the gcode 

At [Copenhagen Music Maker Space](https://musicmakerspace.dk/) we are working with an
[Roland SRM-20 CNC mill](https://musicmakerspace.dk/wiki/doku.php?id=cnc) to e.g.
mill PCB's.

Apparantly one often has a regular bitmap image one wants to mill - so this program finds all the
holes and exports G-code for the CNC mill.

## Hacky image-to-Gcode Workflow &trade;

### PCB paths 
* edit your PCB bitmap in [Gimp](https://www.gimp.org/downloads/)
* use [Inkscape](https://inkscape.org/) to map path-bitmap into an SVG
* export as a compatible format for your CNC software
  * [STL](https://en.wikipedia.org/wiki/STL_(file_format)) ?  

### PCB holes (using `obitmaphole`)
* use the black bucket tool in Gimp to fill out everything but the lightly coloured holes of the PCB bitmap
* use `obitmaphole` to generate the G-code:
  * `obitmaphole --image <image-file> --output blobmap`
    * check the generated `blobmap.png` for if your holes are recognized properly - they will be colourized and marked with a cross
    * check the terminal for potentially ignored outliers
  * `obitmaphole --image <image-file> --x-range 0,<width-in-mm> --y-range 0,<height-in-mm> --output gcode`
    * you specify the same width and height in mm, as the SVG and bitmap has
    * the G-code is printed to `stdout`

## Features

* export formats:
  * `blobmap`: check what blobs are recognized in the image, and their corresponding bounding-box and center
    * the centers are used for the G-code holes
  * `centers`: print the hole centers on `stdout` - these are mapped with `--x-range` and `--y-range`
  * `gcode`: print the gcode on `stdout` 
* filter outlier blobs away; blobs that are too big/small vs the average are ignored by default (see `--help` for toggling)
* see `--help` for more info

## Installation

[Install](https://opam.ocaml.org/doc/Install.html) `opam`, the OCaml package manager.
E.g. on Arch Linux:
```bash
pacman -S opam
opam init
```

Install OCaml 4.14.1:
```bash
opam switch create 4.14.2+options
```

Then clone this repository and install dependencies and `mtag`:
```bash
git clone https://github.com/rand00/obitmaphole
cd obitmaphole
opam install dune cmdliner containers gg imagelib 
dune build
cp bin/main.exe ~/bin/obitmaphole
```



