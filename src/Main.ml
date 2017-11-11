open Tea.App

open Tea.Html

module Coordinate = struct
  type t = int * int
end

type rotation = North | East | South | West
module Knob : sig
  type t
  type color = Yellow | Red | Green | Blue | Orange

  val create : color -> rotation -> Coordinate.t -> t
  val connected : t list -> t -> t list
  val rotate : t -> t
  val location: t -> Coordinate.t
  val color: t -> string
  val transferColor : t -> t -> t
  val sameLocation : t -> t -> bool
  val rotationInDegrees: t -> int
end = struct
  type color = Yellow | Red | Green | Blue | Orange
  type t = {
    color: color;
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

  let color knob = 
    match knob.color with
    | Yellow -> "yellow"
    | Red -> "red"
    | Green -> "green"
    | Blue -> "blue"
    | Orange -> "orange"

  let transferColor from to_ = {to_ with color = from.color}

  let sameLocation k1 k2 = k1.location = k2.location
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
  | North -> (x, y-1)
  | East -> (x+1, y)
  | South -> (x, y+1)
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
      let (oneOther, twoOther) = transformToDirections k.rotation in
      (k.location = coordinateOne && (oppositeRotation one oneOther)) ||
      (k.location = coordinateOne && (oppositeRotation one twoOther)) ||
      (k.location = coordinateTwo && (oppositeRotation two oneOther)) ||
      (k.location = coordinateTwo && (oppositeRotation two twoOther)))
    ) board
end

type model = {
  board: Knob.t list
}

type msg =
  | KnobClicked of Knob.t
  [@@bs.deriving {accessors}]


let init () = ({
  board = [
    Knob.create Green North (0, 0);
    Knob.create Blue West (0, 1);
    Knob.create Red East (1, 0);
    Knob.create Yellow South (1, 1);
  ]
}, Tea.Cmd.none)

let update model = function 
  | KnobClicked knob -> 
    let knob = Knob.rotate knob in
    let connected = 
      Knob.connected model.board knob
      |> List.map (Knob.transferColor knob)
    in
    let newCmds = 
      List.map knobClicked connected
      |> List.map Tea.Cmd.msg
    in
    let newKnobs = (knob :: connected) in
    Js.log "knob clicked";
    ({board = model.board
    |> List.filter (fun k1 -> 
      List.for_all (fun k2 -> not (Knob.sameLocation k1 k2)) newKnobs)
    |> List.append newKnobs
    }, Tea.Cmd.batch newCmds)

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
  Svg.g [onClick (knobClicked knob); SvgA.transform transform] [
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
  Tea.App.standardProgram { 
    init;
    update;
    view;
    subscriptions = fun _ -> Tea.Sub.none;
  }