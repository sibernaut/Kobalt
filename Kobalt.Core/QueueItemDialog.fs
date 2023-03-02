//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kobalt.Core.QueueItemDialog

open System
open Elmish
open Elmish.WPF


type Model =
  { Id: Guid
    Title: string }

type Msg =
  | Title of string
  | Submit
  | Cancel

let create  title itemId =
  { Id = itemId
    Title = title }

let update msg (m: Model) =
  match msg with
  | Title t -> { m with Title = t }, Cmd.none
  | Submit -> m, Cmd.none
  | Cancel -> m, Cmd.none

let bindings () =
  [ "TextInput" |> Binding.twoWay ((fun m -> m.Title), Title)
    "Submit" |> Binding.cmd Submit // managed by parent
    "Cancel" |> Binding.cmd Cancel ] // managed by parent
