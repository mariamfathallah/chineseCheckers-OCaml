# Les Dames Chinoises (OCaml)

Implementation of a Chinese Checkers (Halma) variant in OCaml.
University group project (INF201 – Algorithmique et Programmation Fonctionnelle, Université Grenoble Alpes).

## Features
- Board representation using cube coordinates (i, j, k) with i + j + k = 0
- Validation of positions (star board) and “North–South diamond” end zone
- Player rotation by board rotation
- Move validation:
  - Single-step moves (adjacent cells)
  - Jumps with pivot rules (no capture)
  - Multi-jump sequences

## Rules (project variant)
This implementation follows the INF201 project specification:
- Players move one piece per turn
- A move is either:
  - Unit move to a neighboring cell
  - Jump(s) over a pivot piece, landing on a free valid cell
- Final landing must be inside the North–South diamond area

## How to run
### Requirements
- OCaml (tested with OCaml >= 4.x)

### Run in the toplevel
```ocaml
#use "dames_chinoises.ml";;
```

## Project structure
-```src/dames_chinoises.ml``` : main implementation

## Authors
- FATHALLAH Mariam
- DESFONDS Antoine
- LAUSSAC Guillaume
- DIALLO Alpha

## Notes
This is a first-year functional programming project. The code includes assert-based tests.