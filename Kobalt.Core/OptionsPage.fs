module Kobalt.Core.OptionsPage

open System.Threading
open System.Windows
open Elmish
open Elmish.WPF


type Model =
  { FavPath: string }

type Msg =
  | GoBack
  | Save
  | Browse
  | SetAutoScanPath of string

let init favpath =
  { FavPath = favpath }

let update msg (m: Model) =
  match msg with
  | GoBack -> m, Cmd.none // managed by parent
  | Save -> m, Cmd.none // managed by parent
  | Browse -> m, Cmd.none // UNDONE: need implementation
  | SetAutoScanPath t -> { m with FavPath = t }, Cmd.none

let bindings () =
  [ "FavPath" |> Binding.twoWay((fun m -> m.FavPath), SetAutoScanPath)
    "Browse" |> Binding.cmd Browse
    "Save" |> Binding.cmd Save
    "GoBack" |> Binding.cmd GoBack ]

