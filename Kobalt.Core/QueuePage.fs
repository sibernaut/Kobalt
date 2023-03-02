//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kobalt.Core.QueuePage

open System
open System.IO
open System.Windows
open System.Threading
open Elmish
open Elmish.WPF


type Dialog =
  | ItemEditor of QueueItemDialog.Model

type Item =
  { Id: Guid
    Video: Video }

type Model =
  { Items: Item list
    Dialog: Dialog option
    Config: Config
    StatusMsg: string }

type Msg =
  | RequestLoad
  | LoadFavorite
  | LoadSuccess of string[]
  | LoadCancelled
  | LoadFailed of exn
  | Remove of Guid
  | Modify of Guid
  | ResetChange of Guid
  | ClearList
  | CloseDialog
  | GoNext
  | GoRules
  | GoOptions
  | ItemEditor of QueueItemDialog.Msg

let init config =
  { Items = List.Empty
    Dialog = None
    Config = config
    StatusMsg = "" },
  Cmd.ofMsg LoadFavorite

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

let loadFav m =
  match m.Config.FavoritePath with
  | None -> LoadCancelled
  | Some p ->
    let isVideo (path: string) =
      let ext = Path.GetExtension(path)
      ext = ".mp4" || ext = ".mkv"

    let isNew path =
      m.Items
      |> List.exists(fun x -> x.Video.FilePath = path)
      |> not

    let files = 
      Directory.EnumerateFiles(p)
      |> Seq.filter isVideo
      |> Seq.filter isNew
      |> Seq.toArray
    
    LoadSuccess files


let update msg m =
  match msg with
  | RequestLoad -> m, Cmd.OfAsync.either load () id LoadFailed
  | LoadFavorite -> m, Cmd.OfFunc.result(loadFav m)
  | LoadSuccess f -> 
    let createItem x =
      { Id = Guid.NewGuid()
        Video = Video.create x }

    let items =
      f
      |> Array.map createItem
      |> Array.toList
      |> List.append m.Items

    { m with
        Items = items
        StatusMsg = "File(s) loaded" },
    Cmd.none
  | LoadCancelled -> { m with StatusMsg = "File load cancelled" }, Cmd.none
  | LoadFailed ex ->
    let statusmsg = 
      sprintf 
        "File failed to load with exception %s: %s" 
        (ex.GetType().Name) 
        ex.Message

    { m with StatusMsg = statusmsg }, Cmd.none
  | Remove itemId ->
    let items = 
      m.Items 
      |> List.filter (fun e -> e.Id <> itemId)

    { m with
        Items = items
        StatusMsg = "Item removed" },
    Cmd.none
  | Modify itemId ->
    let item = 
      m.Items 
      |> List.find (fun e -> e.Id = itemId)
      |> (fun x -> 
        let title = Video.getTitle m.Config.Rules x.Video
        QueueItemDialog.create title x.Id)

    { m with Dialog = Some(Dialog.ItemEditor item) }, Cmd.none
  | ResetChange itemId ->
    let resetTitle e =
      match e.Id with
      | guid when guid = itemId ->
        { e with Video = Video.resetTitle e.Video } 
      | _ -> e

    { m with Items =
              m.Items
              |> List.map resetTitle },
    Cmd.none
  | ClearList ->
    { m with
        Items = List.empty
        StatusMsg = "List cleared" },
    Cmd.none
  | CloseDialog -> { m with Dialog = None }, Cmd.none
  | ItemEditor QueueItemDialog.Cancel -> m, Cmd.ofMsg CloseDialog
  | ItemEditor QueueItemDialog.Submit ->
    match m.Dialog with
    | Some(Dialog.ItemEditor m') -> 
      let isEmpty = m'.Title |> String.IsNullOrWhiteSpace
      let setTitle e =
        let update t e =
          { e with Video = Video.updateTitle t e.Video }
        
        match e.Id with
        | guid when guid = m'.Id -> update m'.Title e
        | _ -> e

      let items =
        m.Items 
        |> List.map setTitle

      match isEmpty with
      | false -> { m with Items = items }, Cmd.ofMsg CloseDialog
      | true -> m, Cmd.none
    | None -> m, Cmd.none
  | ItemEditor msg' ->
    match m.Dialog with
    | Some(Dialog.ItemEditor m') ->
      let detailModel, detailMsg = QueueItemDialog.update msg' m'

      { m with Dialog = Some(Dialog.ItemEditor detailModel)}, detailMsg
    | _ -> m, Cmd.none
  | GoNext -> m, Cmd.none // managed by parent model
  | GoRules -> m, Cmd.none // managed by parent model
  | GoOptions -> m, Cmd.none // managed by parent model


let bindings () =
  [ "QueueItems"
    |> Binding.subModelSeq(
      (fun m -> m.Items),
      (fun e -> e.Id),
      (fun () ->
        [ "Title" |> Binding.oneWay (fun (m, e) -> Video.getTitle m.Config.Rules e.Video)
          "Remove" |> Binding.cmd (fun (_, e) -> Remove e.Id)
          "Modify" |> Binding.cmd (fun (_, e) -> Modify e.Id)
          "Reset"
          |> Binding.cmdIf(fun (_, e) ->
            match e.Video.Title with
            | Some _ -> Some(ResetChange e.Id)
            | None -> None
          ) ]
        )
    )
    "DetailFormVisible"
    |> Binding.oneWay(
      fun m ->
        match m.Dialog with
        | Some(Dialog.ItemEditor _) -> true
        | _ -> false
    )
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
    "StatusMsg" |> Binding.oneWay(fun m -> m.StatusMsg)
    "Load" |> Binding.cmd RequestLoad
    "LoadFav" |> Binding.cmd LoadFavorite
    "ClearList" |> Binding.cmd ClearList
    "GoNext" |> Binding.cmd GoNext
    "GoRules" |> Binding.cmd GoRules
    "GoOptions" |> Binding.cmd GoOptions ]
