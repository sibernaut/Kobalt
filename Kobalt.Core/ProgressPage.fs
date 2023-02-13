module Kobalt.Core.ProgressPage

open System
open Elmish
open Elmish.WPF


let init () =
  { Items = List.Empty
    Text = "Processing..." }, 
  Cmd.none

let update msg m =
  match msg with
  | RequestLoad  -> 
    let load (dispatch: ProgressPageMsg -> unit) =
      async {
        for item in m.Items do
          dispatch (LoadSuccess (Helpers.save item))
          do! Async.Sleep(TimeSpan.FromSeconds(1))

        dispatch (LoadSuccess "Done")
      }
      |> Async.StartImmediate 
    m, Cmd.ofSub load
  | LoadSuccess t -> { m with Text = sprintf "%s\r\n%s" m.Text t }, Cmd.none
  | Update -> { m with Text = "Updated" }, Cmd.none
  | ProgressPageMsg.GoBack -> m, Cmd.none

let bindings () =
  [ "Text" |> Binding.oneWay (fun m -> m.Text)
    "Update" |> Binding.cmd Update
    "GoBack" |> Binding.cmd ProgressPageMsg.GoBack ]
