module Kobalt.Core.QueueItemDialog

open Elmish
open Elmish.WPF


type Model =
  { Id: int
    Item: Video }

type Msg =
  | TextInput of string
  | Submit
  | Cancel

let create path itemId =
  { Id = itemId
    Item = Video.create path }

let setTitle t m = { m with Item = Video.updateTitle t m.Item }

let update msg m =
  match msg with
  | TextInput t -> setTitle t m, Cmd.none
  | Submit -> m, Cmd.none
  | Cancel -> m, Cmd.none

let bindings () =
  [ "TextInput" |> Binding.twoWay ((fun m -> Video.getTitle m.Item), TextInput)
    "Submit" |> Binding.cmd Submit // managed by parent
    "Cancel" |> Binding.cmd Cancel ] // managed by parent
