module Kobalt.Core.ProgressPage

open System
open Elmish
open Elmish.WPF


type Model = 
  { Text: string }

type Msg =
  | RequestLoad of string list
  | LoadSuccess of string
  | Update
  | GoBack

let init () =
  { Text = "Processing..." }, 
  Cmd.none


let update msg m =
  match msg with
  | RequestLoad items -> 
    let load (dispatch: Msg -> unit) =
      async {
        for item in items do
          dispatch (LoadSuccess item)
          do! Async.Sleep(TimeSpan.FromSeconds(1))

        dispatch (LoadSuccess "Done")
      }
      |> Async.StartImmediate 
    m, Cmd.ofSub load
  | LoadSuccess t -> { m with Text = sprintf "%s\r\n%s" m.Text t }, Cmd.none
  | Update -> { m with Text = "Updated" }, Cmd.none
  | GoBack -> m, Cmd.none

let bindings () =
  [ "Text" |> Binding.oneWay (fun m -> m.Text)
    "Update" |> Binding.cmd Update
    "GoBack" |> Binding.cmd GoBack ]
