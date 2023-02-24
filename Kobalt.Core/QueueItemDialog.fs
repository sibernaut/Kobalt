module Kobalt.Core.QueueItemDialog

open Elmish
open Elmish.WPF


type Model =
  { Id: int
    Item: Video
    Title: string }

type Msg =
  | TextInput of string
  | Submit
  | Cancel

let create video title itemId =
  { Id = itemId
    Item = video 
    Title = title }

let update msg (m: Model) =
  match msg with
  | TextInput t -> { m with Title = t }, Cmd.none
  | Submit -> m, Cmd.none
  | Cancel -> m, Cmd.none

let bindings () =
  [ "TextInput" |> Binding.twoWay ((fun m -> m.Title), TextInput)
    "Submit" |> Binding.cmd Submit // managed by parent
    "Cancel" |> Binding.cmd Cancel ] // managed by parent
