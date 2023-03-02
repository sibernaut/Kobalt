module Kobalt.Core.ProgressPage

open Elmish
open Elmish.WPF

type Model = 
  { Items: Video list
    Config: Config
    Text: string }

type Msg =
  | RequestLoad
  | LoadSuccess of string
  | GoBack

let init config items =
  { Items = items
    Config = config
    Text = "Processing..." }, 
  Cmd.ofMsg RequestLoad

let load m (dispatch: Msg -> unit) =
  async {
    for item in m.Items do
      dispatch (LoadSuccess (Video.save m.Config.Rules item))
      // do! Async.Sleep(TimeSpan.FromSeconds(1))

    dispatch (LoadSuccess "Done")
  }
  |> Async.StartImmediate 

let update msg m =
  match msg with
  | RequestLoad  -> m, Cmd.ofSub (load m)
  | LoadSuccess t -> { m with Text = sprintf "%s\r\n%s" m.Text t }, Cmd.none
  | GoBack -> m, Cmd.none // managed by parent model

let bindings () =
  [ "Text" |> Binding.oneWay (fun m -> m.Text)
    "GoBack" |> Binding.cmd GoBack ]
