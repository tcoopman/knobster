open Tea.App

open Tea.Html

module Coordinate = struct
  type t = int * int
end

type rotation = North | East | South | West
module Knob : sig
  type t

  val create : string -> rotation -> Coordinate.t -> t
  val connected : t list -> t -> t list
  val rotate : t -> t
  val location: t -> Coordinate.t
  val color: t -> string
  val onLocation : t -> Coordinate.t -> bool
  val rotationInDegrees: t -> int
end = struct
  type t = {
    color: string;
    rotation: rotation;
    location: Coordinate.t;
  }

  let create color rotation location = {color; rotation; location}

  let rotate knob = 
    let newRotation = match knob.rotation with
    | North -> East
    | East -> South
    | South -> West
    | West -> North
    in
    {knob with rotation = newRotation}

  let location knob = knob.location
  let color knob = knob.color
  let onLocation knob coordinate = knob.location = coordinate
  let rotationInDegrees knob = 
    match knob.rotation with
    | North -> 0
    | East -> 90
    | South -> 180
    | West -> 270

  let transformToDirections = function
  | North -> (North, East)
  | East -> (East, South)
  | South -> (South, West)
  | West -> (West, North)

  let mapCoordinate (x, y) = function
  | North -> (x, y+1)
  | East -> (x+1, y)
  | South -> (x, y-1)
  | West -> (x-1, y)

  let oppositeRotation r1 r2 =
    match (r1, r2) with
    | (North, South) ->  true
    | (East, West) ->  true
    | (South, North) ->  true
    | (West, East) ->  true
    | _ ->  false

  let connected board knob =
    let (one, two) = transformToDirections knob.rotation in
    let coordinateOne = mapCoordinate knob.location one in
    let coordinateTwo = mapCoordinate knob.location two in
    List.filter (fun k -> (
      (k.location = coordinateOne && (oppositeRotation one k.rotation)) ||
      (k.location = coordinateTwo && (oppositeRotation two k.rotation)))
    ) board
end

type model = {
  board: Knob.t list
}

type msg =
  | KnobClicked of Coordinate.t
  [@@bs.deriving {accessors}]


let init () = {
  board = [
    Knob.create "green" North (0, 0);
    Knob.create "blue" West (0, 1);
    Knob.create "red" East (1, 0);
    Knob.create "yellow" South (1, 1);
  ]
}

let update model = function 
  | KnobClicked coordinate -> 
    let knob = List.find (fun knob -> Knob.onLocation knob coordinate) model.board in
    {board = model.board
    |> List.filter (fun knob -> not (Knob.onLocation knob coordinate))
    |> List.append [Knob.rotate knob]
    }

let viewKnob knob = 
  let module Svg = Tea.Svg in
  let module SvgA = Tea.Svg.Attributes in
  let (x,y) = Knob.location knob in
  let size = 50 in
  let toPx i = (string_of_int i) ^ "px" in
  let translate = "translate(" ^ string_of_int (x * size * 2) ^ ", " ^ string_of_int (y * size * 2) ^ ")" in
  let rotate = 
    let degrees = Knob.rotationInDegrees knob in
    "rotate(" ^ string_of_int (degrees) ^ ", " ^ string_of_int (x * size * 2 + size) ^ ", " ^ string_of_int (y * size * 2 + size) ^ ")" 
  in
  let transform = rotate ^ " " ^ translate in
  Svg.g [onClick (knobClicked (x, y)); SvgA.transform transform] [
    Svg.circle [SvgA.cx (toPx size); SvgA.cy (toPx size); SvgA.r (toPx size); SvgA.fill (Knob.color knob)] [];
    Svg.path [SvgA.d "M 50 0 V 50 H 100"; SvgA.stroke "black"; SvgA.strokeWidth "5"; SvgA.fill "transparent"] []
  ]

let viewBoard board =
  let module Svg = Tea.Svg in
  let module SvgA = Tea.Svg.Attributes in
  Svg.svg [SvgA.width "500px"; SvgA.height "500px"] (
    List.map viewKnob board
  )

let view model =
  div
    []
    [
      viewBoard model.board
    ]

let main =
  beginnerProgram { 
    model = init (); 
    update;
    view;
  }