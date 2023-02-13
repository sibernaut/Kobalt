module Kobalt.Core.QueuePage

open System
open System.Windows
open System.Threading
open Elmish
open Elmish.WPF


type Queue =
  { Id: Guid
    FileName: string
    IsSelected: bool }

type Model =
  { QueueItems: Queue list
    StatusMsg: string
    Selected: int option }

let init () =
  { QueueItems = List.Empty
    StatusMsg = ""
    Selected = None },
  Cmd.none

type Msg =
  | RequestLoad
  | LoadSuccess of string[]
  | LoadCancelled
  | LoadFailed of exn
  | Select of int option
  | SetIsSelected of Guid * bool
  | RemoveItem
  | ClearList
  | GoNext


let load () =
  Application.Current.Dispatcher.Invoke(fun () ->
    let guiCtx = SynchronizationContext.Current

    async {
      do! Async.SwitchToContext guiCtx
      let dlg = Microsoft.Win32.OpenFileDialog()
      dlg.Multiselect <- true
      dlg.Filter <- "Video files (*.mp4;*.mkv)|*.mp4;*.mkv|MP4 files (*.mp4)|*.mp4|Matroska files(*.mkv)|*.mkv"
      let result = dlg.ShowDialog()

      if result.HasValue && result.Value then
        return LoadSuccess dlg.FileNames
      else
        return LoadCancelled
    })


let loadSuccess files m =
  let items =
    files
    |> Array.map (fun item ->
      { Id = Guid.NewGuid()
        FileName = item
        IsSelected = false })
    |> Array.toList

  { m with
      QueueItems = items |> List.append m.QueueItems
      StatusMsg = "File loaded" }


let setIsSelected id value m =
  let setValue item =
    if item.Id = id then
      { item with IsSelected = value }
    else
      item

  { m with
      QueueItems = m.QueueItems |> List.map (fun e -> setValue e) }


let removeItem m =
  { m with
      QueueItems = m.QueueItems |> List.filter (fun e -> e.IsSelected = false)
      StatusMsg = "Selected item(s) removed" }


let update msg m =
  match msg with
  | RequestLoad -> m, Cmd.OfAsync.either load () id LoadFailed
  | LoadSuccess f -> loadSuccess f m, Cmd.none
  | LoadCancelled ->
    { m with
        StatusMsg = "File load cancelled" },
    Cmd.none
  | LoadFailed ex ->
    { m with
        StatusMsg = sprintf "File failed to load with exception %s: %s" (ex.GetType().Name) ex.Message },
    Cmd.none
  | Select itemId -> { m with Selected = itemId }, Cmd.none
  | SetIsSelected(itemId, isSelected) -> setIsSelected itemId isSelected m, Cmd.none
  | RemoveItem -> removeItem m, Cmd.none
  | ClearList ->
    { m with
        QueueItems = List.empty
        Selected = None
        StatusMsg = "List cleared" },
    Cmd.none
  | GoNext -> m, Cmd.none


let bindings () =
  [ "QueueItems"
    |> Binding.subModelSeq (
      (fun m -> m.QueueItems),
      (fun e -> e.Id),
      (fun () ->
        [ "ID" |> Binding.oneWay (fun (_, e) -> e.Id)
          "FileName" |> Binding.oneWay (fun (_, e) -> e.FileName)
          "IsSelected"
          |> Binding.twoWay ((fun (_, e) -> e.IsSelected), (fun isSelected (_, e) -> SetIsSelected(e.Id, isSelected)))
          "SelectedLabel"
          |> Binding.oneWay (fun (_, e) -> if e.IsSelected then "SELECTED" else "") ])
    )
    "StatusMsg" |> Binding.oneWay (fun m -> m.StatusMsg)
    "Load" |> Binding.cmd RequestLoad
    "SelectedItem"
    |> Binding.subModelSelectedItem ("QueueItems", (fun m -> m.Selected), Select)
    "RemoveItem" |> Binding.cmd RemoveItem
    "ClearList" |> Binding.cmd ClearList
    "GoNext" |> Binding.cmd GoNext ]
