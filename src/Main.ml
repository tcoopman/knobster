open Tea.App

open Tea.Html

module Coordinate = struct
  type t = int * int
end

module Knob : sig
  type t
  type color = Yellow | Red | Green | Blue | Orange
  type rotation = North | East | South | West

  val create : color -> rotation -> Coordinate.t -> t
  val connected : t list -> t -> t list
  val rotate : t -> t
  val location: t -> Coordinate.t
  val color: t -> string
  val transferColor : t -> t -> t
  val sameColor : t -> t -> bool
  val sameLocation : t -> t -> bool
  val rotationInDegrees: t -> int
  val compare : t -> t -> int
end = struct
  type color = Yellow | Red | Green | Blue | Orange
  type rotation = North | East | South | West
  type t = {
    color: color;
    degrees: int;
    location: Coordinate.t;
  }

  let rotationInDegrees rotation =
    match rotation with
    | North -> 0
    | East -> 90
    | South -> 180
    | West -> 270
  let degreesToRotation degrees =
    match (degrees mod 360) with
    | 0 -> North
    | 90 -> East
    | 180 -> South
    | 270 -> West
    | _ -> invalid_arg "degrees not modulo 90"

  let create color rotation location = {color; degrees =(rotationInDegrees rotation); location}

  let rotate knob = 
    {knob with degrees = knob.degrees + 90}

  let location knob = knob.location

  let color knob = 
    match knob.color with
    | Yellow -> "yellow"
    | Red -> "red"
    | Green -> "green"
    | Blue -> "blue"
    | Orange -> "orange"

  let transferColor from to_ = {to_ with color = from.color}
  let sameColor k1 k2 = k1.color = k2.color

  let sameLocation k1 k2 = k1.location = k2.location
  let rotationInDegrees knob = knob.degrees

  let transformToDirections degrees =
    let rotation = degreesToRotation degrees in
    match rotation with
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
    let (one, two) = transformToDirections knob.degrees in
    let coordinateOne = mapCoordinate knob.location one in
    let coordinateTwo = mapCoordinate knob.location two in
    List.filter (fun k -> (
      let (oneOther, twoOther) = transformToDirections k.degrees in
      (k.location = coordinateOne && (oppositeRotation one oneOther)) ||
      (k.location = coordinateOne && (oppositeRotation one twoOther)) ||
      (k.location = coordinateTwo && (oppositeRotation two oneOther)) ||
      (k.location = coordinateTwo && (oppositeRotation two twoOther)))
    ) board

  let compare knobA knobB =
    let (x1, y1) = knobA.location in
    let (x2, y2) = knobB.location in
    let compareY = compare y1 y2 in
    if compareY == 0 then compare x1 x2 else compareY
end

type state = WaitingForInput | Resolving | Won
type model = {
  board: Knob.t list;
  state: state
}

type msg =
  | KnobClicked of Knob.t
  | KnobRotationPropagated of Knob.t
  [@@bs.deriving {accessors}]


let init () = ({
  board = [
    Knob.create Green North (0, 0);
    Knob.create Blue West (0, 1);
    Knob.create Red East (1, 0);
    Knob.create Yellow South (1, 1);
    Knob.create Red South (2, 0);
    Knob.create Orange South (2, 1);
    Knob.create Blue South (2, 2);
    Knob.create Green South (0, 2);
    Knob.create Green South (1, 2);
  ];
  state = WaitingForInput
}, Tea.Cmd.none)


let propagateKnobRotation board knob =
  let knob = Knob.rotate knob in
let updateBoard board newKnobs =
  board
  |> List.filter (fun k1 -> 
    List.for_all (fun k2 -> not (Knob.sameLocation k1 k2)) newKnobs)
  |> List.append newKnobs
  in
  let connected = 
    Knob.connected board knob
    |> List.map (Knob.transferColor knob)
  in
  let newCmds = 
    match connected with
    | [] -> None
    | _ -> Some (Tea.Cmd.call (fun callbacks ->
      Js.Global.setTimeout (fun () -> 
        (List.iter (fun knob ->
          !callbacks.enqueue (knobRotationPropagated knob)
        ) connected)
      ) 200 |> ignore
    ))
  in
  let newKnobs = (knob :: connected) in
  let newBoard =updateBoard board newKnobs in
  let gameWon = 
    match board with
    | [] -> true
    | (hd::rest) -> List.for_all (fun knob -> Knob.sameColor hd knob) rest
  in
  begin match (gameWon, newCmds) with
  | (true, _) -> 
    (
      {board = newBoard; state = Won}, 
      Tea.Cmd.none)
  | (false, None) -> 
    (
      {board = newBoard; state = WaitingForInput}, 
      Tea.Cmd.none)
  | (false, Some cmd) -> 
    (
      {board = newBoard; state = Resolving}, 
      Tea.Cmd.batch [cmd])
  end


let update model = function 
  | KnobClicked knob -> 
    begin match model.state with
    | WaitingForInput -> propagateKnobRotation model.board knob
    | Won -> (model, Tea.Cmd.none)
    | Resolving -> (model, Tea.Cmd.none)
    end
  | KnobRotationPropagated knob ->
    match model.state with
    | WaitingForInput -> (model, Tea.Cmd.none)
    | Won -> (model, Tea.Cmd.none)
    | Resolving -> propagateKnobRotation model.board knob

let viewKnob knob = 
  let module Svg = Tea.Svg in
  let module SvgA = Tea.Svg.Attributes in
  let size = 50 in
  let toPx i = (string_of_int i) ^ "px" in
  let style = "transform:rotate(" ^ (string_of_int (Knob.rotationInDegrees knob)) ^ "deg);transition:200ms" in
  Svg.svg [SvgA.width (toPx (size *2)); SvgA.height (toPx (size * 2)); SvgA.style style; onClick (knobClicked knob)] [
    Svg.circle [SvgA.cx (toPx size); SvgA.cy (toPx size); SvgA.r (toPx size); SvgA.fill (Knob.color knob)] [];
    Svg.path [SvgA.d "M 50 0 V 50 H 100"; SvgA.stroke "black"; SvgA.strokeWidth "5"; SvgA.fill "transparent"] [];
  ]


let viewBoard board =
  let module Svg = Tea.Svg in
  let module SvgA = Tea.Svg.Attributes in
  div [class' "board"] (
    board
    |> List.sort Knob.compare
    |> List.map viewKnob 
  )

let view model =
  let state = match model.state with
  | WaitingForInput -> "waiting for input"
  | Won -> "won"
  | Resolving -> "resolving"
  in
  main
    []
    [
      div [] [ text state];
      viewBoard model.board
    ]

let main =
  Tea.App.standardProgram { 
    init;
    update;
    view;
    subscriptions = fun _ -> Tea.Sub.none;
  }