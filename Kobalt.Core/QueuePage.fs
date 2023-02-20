module Kobalt.Core.QueuePage

open System.Windows
open System.Threading
open Elmish
open Elmish.WPF


type Dialog =
  | ItemEditor of QueueItemDialog.Model

type Model =
  { Items: QueueItemDialog.Model list
    Dialog: Dialog option
    StatusMsg: string }

type Msg =
  | RequestLoad
  | LoadSuccess of string[]
  | LoadCancelled
  | LoadFailed of exn
  | Remove of int
  | Modify of int
  | ResetChange of int
  | ClearList
  | GoNext
  | ItemEditor of QueueItemDialog.Msg

let init () =
  { Items = List.Empty
    Dialog = None
    StatusMsg = "" },
  Cmd.none

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
        return Msg.LoadSuccess dlg.FileNames
      else
        return LoadCancelled
    })


let update msg m =
  match msg with
  | Msg.RequestLoad -> m, Cmd.OfAsync.either load () id Msg.LoadFailed
  | Msg.LoadSuccess f -> 
    { m with
        Items = 
          f
          |> Array.mapi (fun i x -> QueueItemDialog.create x i)
          |> Array.toList
          |> List.append m.Items
        StatusMsg = "File(s) loaded" },
    Cmd.none
  | LoadCancelled ->
    { m with
        StatusMsg = "File load cancelled" },
    Cmd.none
  | LoadFailed ex ->
    { m with
        StatusMsg = sprintf "File failed to load with exception %s: %s" (ex.GetType().Name) ex.Message },
    Cmd.none
  | Remove itemId ->
    { m with
        Items = 
          m.Items 
          |> List.filter (fun e -> e.Id <> itemId)
        StatusMsg = "Item removed" },
    Cmd.none
  | Modify itemId ->
    let item = m.Items |> List.find (fun e -> e.Id = itemId)

    { m with Dialog = Some(Dialog.ItemEditor item) },
    Cmd.none
  | ResetChange itemId ->
    let reset (item: QueueItemDialog.Model) =
      { item with Item = Video.resetTitle item.Item }      

    { m with
        Items =
          m.Items
          |> List.map (fun e -> if e.Id = itemId then reset e else e) },
    Cmd.none
  | ClearList ->
    { m with
        Items = List.empty
        StatusMsg = "List cleared" },
    Cmd.none
  | ItemEditor QueueItemDialog.Cancel -> { m with Dialog = None }, Cmd.none
  | ItemEditor QueueItemDialog.Submit ->
    let setTitle (e: QueueItemDialog.Model) i =
      match m.Dialog with
      | Some(Dialog.ItemEditor m') -> 
        if m'.Id = i then 
          { e with Item = m'.Item }
        else
          e
      | None -> e

    { m with
        Items = 
          m.Items 
          |> List.mapi (fun i e -> setTitle e i)
        Dialog = None },
    Cmd.none
  | ItemEditor msg' ->
    match m.Dialog with
    | Some(Dialog.ItemEditor m') ->
      let detailModel, detailMsg = QueueItemDialog.update msg' m'

      { m with
          Dialog = detailModel |> Dialog.ItemEditor |> Some },
      detailMsg
    | _ -> m, Cmd.none
  | GoNext -> m, Cmd.none // managed by parent model


let bindings () =
  [ "QueueItems"
    |> Binding.subModelSeq (
      (fun m -> m.Items),
      (fun e -> e.Id),
      (fun () ->
        [ "Title" |> Binding.oneWay (fun (_, e) -> Video.getTitle e.Item)
          "Remove" |> Binding.cmd (fun (_, (e: QueueItemDialog.Model)) -> Remove e.Id)
          "Modify" |> Binding.cmd (fun (_, (e: QueueItemDialog.Model)) -> Modify e.Id)
          "Reset"
          |> Binding.cmdIf (fun (_, (e: QueueItemDialog.Model)) ->
            match e.Item.Title with
            | Some _ -> Some(ResetChange e.Id)
            | None -> None )])
    )
    "DetailFormVisible"
    |> Binding.oneWay (fun m ->
      match m.Dialog with
      | Some(Dialog.ItemEditor _) -> true
      | _ -> false)
    "DetailForm"
    |> Binding.subModelOpt (
      (fun m ->
        match m.Dialog with
        | Some(Dialog.ItemEditor m') -> Some m'
        | _ -> None),
      snd,
      ItemEditor,
      QueueItemDialog.bindings
    )
    "StatusMsg" |> Binding.oneWay (fun m -> m.StatusMsg)
    "Load" |> Binding.cmd Msg.RequestLoad
    "ClearList" |> Binding.cmd ClearList
    "GoNext" |> Binding.cmd GoNext ]
