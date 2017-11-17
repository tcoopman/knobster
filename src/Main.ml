open Tea.App

open Tea.Html

let pick (t:'a list) : 'a = 
  let length = List.length t in
  let i = Random.int length in
  List.nth t i

module Coordinate = struct
  type t = int * int
end

module Knob : sig
  type t
  type color = Yellow | Red | Green | Blue | Orange
  type rotation = North | East | South | West

  val create : color -> rotation -> Coordinate.t -> t
  val random : Coordinate.t -> t
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
  let random location =
    let rotation = pick [North; East; South; West] in
    let color = pick [Yellow; Red; Green; Blue; Orange] in
    create color rotation location

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


let init () = 
  let coordinates = 
    let rec zippedRange xS xE yS yE = 
      match (xS = xE, yS = yE) with
      | true, true -> [(xS, yS)]
      | true, false -> (xS, yS) :: (zippedRange 0 xE (yS+1) yE)
      | false, true -> (xS, yS) :: (zippedRange (xS+1) xE yS yE)
      | false, false -> (xS, yS) :: (zippedRange (xS+1) xE yS yE)
    in
    zippedRange 0 9 0 9
  in
  let board = List.map Knob.random coordinates in
  ({
    board;
    state = WaitingForInput
  }, Tea.Cmd.none
  )


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
  let (x,y) = Knob.location knob in
  let toPx i = (string_of_int i) ^ "px" in
  let style = "transform:rotate(" ^ (string_of_int (Knob.rotationInDegrees knob)) ^ "deg);transition:200ms" in
  Svg.svg [SvgA.width (toPx (size *2)); SvgA.height (toPx (size * 2)); SvgA.style style; onClick (knobClicked knob)] [
    Svg.circle [SvgA.cx (toPx size); SvgA.cy (toPx size); SvgA.r (toPx size); SvgA.fill (Knob.color knob)] [];
    Svg.path [SvgA.d "M 50 0 V 50 H 100"; SvgA.stroke "black"; SvgA.strokeWidth "5"; SvgA.fill "transparent"] [];
    Svg.text' [SvgA.x (toPx size); SvgA.y (toPx size); SvgA.fontSize "20"] [Svg.text (string_of_int x ^ ", " ^ string_of_int y)]
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