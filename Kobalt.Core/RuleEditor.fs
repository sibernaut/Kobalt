module Kobalt.Core.RuleEditor

open System
open Elmish
open Elmish.WPF


type Model = 
  { Id: Guid
    Item: Rule }

type Msg =
  | SetSearchFor of string
  | SetReplaceWith of string
  | SetRegex of bool
  | Submit
  | Cancel

let init rule guid =
  { Id = guid
    Item = rule }

let updateSearch rule input =
  match rule with
  | Regular p -> Regular { p with Search = input }
  | Regex p -> Regex { p with Search = input }

let updateReplace rule input =
  match rule with
  | Regular p -> Regular { p with Replace = input }
  | Regex p -> Regex { p with Replace = input }

let updateType rule input =
  match input with
  | true -> rule |> Rule.setRegex
  | false -> rule |> Rule.setRegular

let update msg m =
  match msg with
  | SetSearchFor t -> { m with Item = updateSearch m.Item t }, Cmd.none
  | SetReplaceWith t -> { m with Item = updateReplace m.Item t }, Cmd.none
  | SetRegex b -> { m with Item = updateType m.Item b }, Cmd.none
  | Submit -> m, Cmd.none
  | Cancel -> m, Cmd.none

let isRegex rule =
  match rule with
  | Regular _ -> false
  | Regex _ -> true

let bindings () =
  [ "SearchFor" |> Binding.twoWay ((fun m -> Rule.getField m.Item |> fst), SetSearchFor)
    "ReplaceWith" |> Binding.twoWay ((fun m -> Rule.getField m.Item |> snd), SetReplaceWith) 
    "IsRegex" |> Binding.twoWay ((fun m -> isRegex m.Item), SetRegex) 
    "Submit" |> Binding.cmd Submit // managed by parent
    "Cancel" |> Binding.cmd Cancel ] // managed by parent
