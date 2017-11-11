open Jest
open Main

let _ =

describe "connected" (fun () -> 
  let open Expect in

  test "with an empty board" (fun () ->
    let board = [] in
    let knob = Knob.create Knob.Green Main.North (0, 0) in
    let connected = Knob.connected board knob in
    expect connected |> toBe []
  );

  test "only connected" (fun () ->
    let board = [
      Knob.create Knob.Green Main.South (0, -1);
      Knob.create Knob.Green Main.West (1, 0)
    ] in
    let knob = Knob.create Knob.Green Main.North (0, 0) in
    let connected = Knob.connected board knob in
    expect connected |> toEqual board
  );

  test "only some connected" (fun () ->
    let board = [
      Knob.create Knob.Green Main.South (0, -1);
      Knob.create Knob.Green Main.West (1, 0);
      Knob.create Knob.Green Main.North (0, 0);
    ] in
    let knob = Knob.create Knob.Green Main.North (0, 0) in
    let connected = Knob.connected board knob in
    expect connected |> toEqual [
      Knob.create Knob.Green Main.South (0, -1);
      Knob.create Knob.Green Main.West (1, 0);
    ]
  );

  test "east and west" (fun () ->
    let board = [
      Knob.create Knob.Green Main.West (0, 1);
    ] in
    let knob = Knob.create Knob.Green Main.East (0, 0) in
    let connected = Knob.connected board knob in
    expect connected |> toEqual board
  );
);