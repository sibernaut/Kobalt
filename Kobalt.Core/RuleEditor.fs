//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kobalt.Core.RuleEditor

open System
open Elmish
open Elmish.WPF


type Model = 
  { Id: Guid
    SearchFor: string
    ReplaceWith: string
    IsRegex: bool }

type Msg =
  | SetSearchFor of string
  | SetReplaceWith of string
  | SetRegex of bool
  | Submit
  | Cancel

let empty =
  { Id = Guid.NewGuid()
    SearchFor = String.Empty
    ReplaceWith = String.Empty
    IsRegex = false }

let init search replace isregex guid =
  { Id = guid
    SearchFor = search
    ReplaceWith = replace
    IsRegex = isregex }

let update msg m =
  match msg with
  | SetSearchFor t -> { m with SearchFor = t }, Cmd.none
  | SetReplaceWith t -> { m with ReplaceWith = t }, Cmd.none
  | SetRegex b -> { m with IsRegex = b }, Cmd.none
  | Submit -> m, Cmd.none
  | Cancel -> m, Cmd.none

let bindings () =
  [ "SearchFor" |> Binding.twoWay((fun m -> m.SearchFor), SetSearchFor)
    "ReplaceWith" |> Binding.twoWay((fun m -> m.ReplaceWith), SetReplaceWith) 
    "IsRegex" |> Binding.twoWay((fun m -> m.IsRegex), SetRegex) 
    "Submit" |> Binding.cmd Submit // managed by parent
    "Cancel" |> Binding.cmd Cancel ] // managed by parent
