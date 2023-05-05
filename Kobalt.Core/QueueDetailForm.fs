//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kobalt.Core.QueueDetailForm

open System
open Elmish
open Elmish.WPF


type Model =
  { Title: string 
    Filename: string 
    ProcessOutput: string }

type Msg =
  | SetTitle of string
  | Rename
  | Reset

let create rules item =
  let title = Video.getTitle rules item

  { Title = title
    Filename = item.FileName
    ProcessOutput = item.ProcessOutput }

let update msg (m: Model) =
  match msg with
  | SetTitle t -> { m with Title = t }, Cmd.none
  | Rename -> m, Cmd.none
  | Reset -> m, Cmd.none

let bindings () =
  [ "Title" |> Binding.twoWay ((fun m -> m.Title), SetTitle)
    "Filename" |> Binding.oneWay(fun m -> m.Filename)
    "ProcessOutput" |> Binding.oneWay(fun m -> m.ProcessOutput)
    "Rename" |> Binding.cmd Rename // managed by parent
    "Reset" |> Binding.cmd Reset ] // managed by parent
