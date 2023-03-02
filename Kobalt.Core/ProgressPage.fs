//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
  | Exit

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
  | Exit -> m, Cmd.none // managed by parent model

let bindings () =
  [ "Text" |> Binding.oneWay (fun m -> m.Text)
    "GoBack" |> Binding.cmd GoBack
    "Exit" |> Binding.cmd Exit ]
