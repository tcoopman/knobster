open Tea.App

open Tea.Html

module Coordinate = struct
  type t = int * int
end

type rotation = North | East | South | West
type knob = {
  color: string;
  rotation: rotation;
  location: Coordinate.t;
}

type model = {
  board: knob list
}

type msg =
  | KnobClicked of Coordinate.t
  [@@bs.deriving {accessors}]


let init () = {
  board = [
    {color = "green"; rotation = North; location = (0, 0)};
    {color = "blue"; rotation = West; location = (0, 1)};
    {color = "red"; rotation = West; location = (1, 0)};
    {color = "yellow"; rotation = West; location = (1, 1)}
  ]
}

let update model = function 
  | KnobClicked coordinate -> 
    let findNeighbours (x, y) =
      let neighbourCoordinates = [(x, y -1); (x+1, y); (x, y +1); (x -1, y)] in
      neighbourCoordinates
      |> List.map (fun coo -> List.filter (fun knob -> knob.location = coo) model.board) 
      |> List.flatten
    in
    let isConnected knob1 knob2 =
      match (knob1.location, knob2.location, knob1.rotation, knob2.rotation) with
      | ((x1, y1), (x2, y2), East, North) when x1 = x2 && y1 +1 = y2 -> true
      | ((x1, y1), (x2, y2), East, West) when x1 = x2 && y1 +1 = y2 -> true
      | ((x1, y1), (x2, y2), South, North) when x1 = x2 && y1 +1 = y2 -> true
      | ((x1, y1), (x2, y2), South, West) when x1 = x2 && y1 +1 = y2 -> true
      | _ -> false
    in
    let knob = List.find (fun knob -> knob.location = coordinate) model.board in
    let newRotation = match knob.rotation with
    | North -> East
    | East -> South
    | South -> West
    | West -> North
    in
    let newKnob = {knob with rotation = newRotation} in
    Js.log (findNeighbours coordinate |> List.map (fun other -> isConnected newKnob other));
    {board = model.board
    |> List.filter (fun knob -> knob.location <> coordinate)
    |> List.append [newKnob]
    }

let viewKnob knob = 
  let module Svg = Tea.Svg in
  let module SvgA = Tea.Svg.Attributes in
  let (x,y) = knob.location in
  let size = 50 in
  let toPx i = (string_of_int i) ^ "px" in
  let translate = "translate(" ^ string_of_int (x * size * 2) ^ ", " ^ string_of_int (y * size * 2) ^ ")" in
  let rotate = 
    let degrees = match knob.rotation with
    | North -> 0
    | East -> 90
    | South -> 180
    | West -> 270
    in
    "rotate(" ^ string_of_int (degrees) ^ ", " ^ string_of_int (x * size * 2 + size) ^ ", " ^ string_of_int (y * size * 2 + size) ^ ")" 
  in
  let transform = rotate ^ " " ^ translate in
  Svg.g [onClick (knobClicked (x, y)); SvgA.transform transform] [
    Svg.circle [SvgA.cx (toPx size); SvgA.cy (toPx size); SvgA.r (toPx size); SvgA.fill knob.color] [];
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