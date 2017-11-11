open Jest

let _ =

describe "Expect" (fun () -> 
  let open Expect in
  let module Knob = Main.Knob in

  test "color" (fun () ->
    let color = Knob.create "green" Main.North (0, 0) |> Knob.color in
    expect color |> toBe "green"
  );
);