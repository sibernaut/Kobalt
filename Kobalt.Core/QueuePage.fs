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


type Model =
  { Items: Video list
    SelectedId: Guid option
    Form: QueueDetailForm.Model option
    Config: Config
    StatusMsg: string }

type Msg =
  | RequestLoad
  | LoadFavorite
  | LoadSuccess of string[]
  | LoadCancelled
  | LoadFailed of exn
  | SetSelected of Guid option
  | FormMsg of QueueDetailForm.Msg
  | Remove
  | ClearList
  | Run
  | RunSuccess of Video list
  | RunFailed of exn
  | GoRules

let init config =
  { Items = List.Empty
    SelectedId = None
    Form = None
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
      |> List.exists(fun x -> x.FilePath = path)
      |> not

    let files = 
      Directory.EnumerateFiles(p)
      |> Seq.filter isVideo
      |> Seq.filter isNew
      |> Seq.toArray
    
    LoadSuccess files

let selectItem itemId m =
  match itemId with
  | Some i ->
    let item = m.Items |> List.find(fun e -> e.Id = i)

    { m with 
        SelectedId = itemId
        Form = Some(QueueDetailForm.create m.Config.Rules item)}
  | None -> 
    { m with 
        SelectedId = None
        Form = None }

let removeItem m =
  match m.SelectedId with
  | Some itemId ->
    let items = 
      m.Items 
      |> List.filter (fun e -> e.Id <> itemId)

    { m with
        Items = items
        StatusMsg = "Item removed" }
  | None -> m

let modifyItem m =
  match m.SelectedId with
  | Some itemId ->
    let setTitle e =
      match m.Form with
      | Some f -> Video.updateTitle f.Title e
      | None -> e
    let items =
      m.Items
      |> List.map (fun e -> if e.Id = itemId then setTitle e else e)

    { m with Items = items }
  | None -> m

let resetItem m =
  match m.SelectedId with
  | Some itemId ->
    let resetTitle e =
      match e.Id with
      | guid when guid = itemId -> Video.resetTitle e
      | _ -> e

    { m with Items = m.Items |> List.map resetTitle }
  | None -> m

let run m = 
  Application.Current.Dispatcher.Invoke(fun () ->
    let guiCtx = SynchronizationContext.Current

    async {
      do! Async.SwitchToContext guiCtx
      let items = m.Items |> List.map (fun e -> Video.save m.Config.Rules e)
      return items
    })

let runSuccess items m =
  { m with 
      Items = items 
      StatusMsg = "Run successful" }

let runFailed (ex: exn) m =
  let statusmsg = 
    sprintf 
      "Run failed with exception %s: %s" 
      (ex.GetType().Name) 
      ex.Message

  { m with StatusMsg = statusmsg }

let update msg m =
  match msg with
  | RequestLoad -> m, Cmd.OfAsync.either load () id LoadFailed
  | LoadFavorite -> m, Cmd.OfFunc.result(loadFav m)
  | LoadSuccess f -> 
    let items =
      f
      |> Array.map Video.create
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
  | SetSelected itemId -> selectItem itemId m, Cmd.none
  | FormMsg QueueDetailForm.Rename -> 
    match m.Form with
    | Some m' -> 
      let isEmpty = m'.Title |> String.IsNullOrWhiteSpace
      match isEmpty with
      | true -> m, Cmd.ofMsg(FormMsg QueueDetailForm.Reset)
      | false -> modifyItem m, Cmd.none
    | None -> m, Cmd.none
  | FormMsg QueueDetailForm.Reset -> resetItem m, Cmd.ofMsg(SetSelected m.SelectedId)
  | FormMsg msg' ->
    match m.Form with
    | Some m' -> 
      let model, message = QueueDetailForm.update msg' m'

      { m with Form = Some model }, Cmd.map FormMsg message
    | None -> m, Cmd.none
  | Remove -> removeItem m, Cmd.none
  | ClearList ->
    { m with
        Items = List.empty
        StatusMsg = "List cleared" },
    Cmd.none
  | Run -> m, Cmd.OfAsync.either run m RunSuccess RunFailed
  | RunSuccess items -> runSuccess items m, Cmd.none
  | RunFailed ex -> runFailed ex m, Cmd.none
  | GoRules -> m, Cmd.none // managed by parent model


let bindings () =
  [ "QueueItems"
    |> Binding.subModelSeq(
      (fun m -> m.Items),
      (fun e -> e.Id),
      (fun () ->
        [ "Id" |> Binding.oneWay (fun (_, e) -> e.Id)
          "Title" |> Binding.oneWay (fun (m, e) -> Video.getTitle m.Config.Rules e) ] )
    )
    "SelectedId" |> Binding.twoWayOpt((fun m -> m.SelectedId), SetSelected)
    "DetailFormVisible" |> Binding.oneWay(fun m -> 
      match m.Form with
      | Some _ -> true
      | None -> false
    )
    "DetailForm" |> Binding.subModelOpt((fun m -> m.Form), snd, FormMsg, QueueDetailForm.bindings)
    "Remove" |> Binding.cmdIf(fun m ->
      match m.Items.IsEmpty, m.SelectedId with
      | false, Some _ -> Some Remove
      | _, _ -> None
    )
    "StatusMsg" |> Binding.oneWay(fun m -> m.StatusMsg)
    "Load" |> Binding.cmd RequestLoad
    "LoadFav" |> Binding.cmd LoadFavorite
    "ClearList" |> Binding.cmdIf(fun m -> if m.Items.IsEmpty then None else Some ClearList)
    "Run" |> Binding.cmd Run
    "GoRules" |> Binding.cmd GoRules ]
