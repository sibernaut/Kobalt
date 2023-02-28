module Kobalt.Core.OptionsPage

open System.Threading
open System.Windows
open Elmish
open Elmish.WPF


type Model =
  { AutoScanPath: string }

type Msg =
  | GoBack
  | Save
  | Browse
  | SetAutoScanPath of string

let init autoscan =
  { AutoScanPath = autoscan }

let update msg (m: Model) =
  match msg with
  | GoBack -> m, Cmd.none // managed by parent
  | Save -> m, Cmd.none // managed by parent
  | Browse -> m, Cmd.none // UNDONE: need implementation
  | SetAutoScanPath t -> { m with AutoScanPath = t }, Cmd.none

let bindings () =
  [ "AutoScanPath" |> Binding.twoWay((fun m -> m.AutoScanPath), SetAutoScanPath)
    "Browse" |> Binding.cmd Browse
    "Save" |> Binding.cmd Save
    "GoBack" |> Binding.cmd GoBack ]

