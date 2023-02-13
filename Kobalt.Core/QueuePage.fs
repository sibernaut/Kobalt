module Kobalt.Core.QueuePage

open System
open System.IO
open System.Windows
open System.Threading
open Elmish
open Elmish.WPF

module DetailForm =
  let init () : DetailForm * Cmd<DetailFormMsg> =
    { Id = Guid.Empty; Text = "" }, Cmd.none

  let update msg (m: DetailForm) =
    match msg with
    | TextInput t -> { m with Text = t }, Cmd.none
    | Submit -> m, Cmd.none
    | Cancel -> m, Cmd.none

  let bindings () =
    [ "TextInput" |> Binding.twoWay ((fun (m: DetailForm) -> m.Text), TextInput)
      "Submit" |> Binding.cmd Submit
      "Cancel" |> Binding.cmd Cancel ]


let init () =
  { QueueItems = List.Empty
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
        return QueuePageMsg.LoadSuccess dlg.FileNames
      else
        return LoadCancelled
    })


let loadSuccess files m =
  let items =
    files
    |> Array.map (fun item ->
      { Id = Guid.NewGuid()
        FilePath = item
        Title = None
        IsSelected = false
        IsModified = false })
    |> Array.toList

  { m with
      QueueItems = items |> List.append m.QueueItems
      StatusMsg = "File loaded" }


let getTitle (i: Queue) =
  match i.Title with
  | Some t -> t
  | None -> Path.GetFileNameWithoutExtension(i.FilePath)


let update msg m =
  match msg with
  | QueuePageMsg.RequestLoad -> m, Cmd.OfAsync.either load () id QueuePageMsg.LoadFailed
  | QueuePageMsg.LoadSuccess f -> loadSuccess f m, Cmd.none
  | LoadCancelled ->
    { m with
        StatusMsg = "File load cancelled" },
    Cmd.none
  | LoadFailed ex ->
    { m with
        StatusMsg = sprintf "File failed to load with exception %s: %s" (ex.GetType().Name) ex.Message },
    Cmd.none
  | Remove guid ->
    { m with
        QueueItems = m.QueueItems |> List.filter (fun e -> e.Id <> guid)
        StatusMsg = "Item removed" },
    Cmd.none
  | Modify guid ->
    let item = m.QueueItems |> List.find (fun e -> e.Id = guid)
    let m', _ = DetailForm.init ()

    { m with
        QueueItems =
          m.QueueItems
          |> List.map (fun e -> if e.Id = guid then { e with IsModified = true } else e)
        Dialog =
          { m' with
              Id = item.Id
              Text = getTitle item }
          |> DetailForm
          |> Some },
    Cmd.none
  | ResetChange guid ->
    { m with
        QueueItems =
          m.QueueItems
          |> List.map (fun e -> if e.Id = guid then { e with Title = None } else e) },
    Cmd.none
  | ClearList ->
    { m with
        QueueItems = List.empty
        StatusMsg = "List cleared" },
    Cmd.none
  | GoNext -> m, Cmd.none // managed by parent model
  | DetailFormMsg DetailFormMsg.Cancel -> { m with Dialog = None }, Cmd.none
  | DetailFormMsg DetailFormMsg.Submit ->
    let setTitle title =
      match m.Dialog with
      | Some(DetailForm m') -> Some m'.Text
      | None -> title

    let changeTitle i =
      match i.IsModified with
      | true ->
        { i with
            Title = setTitle i.Title
            IsModified = false }
      | false -> i

    { m with
        QueueItems = m.QueueItems |> List.map changeTitle
        Dialog = None },
    Cmd.none
  | DetailFormMsg msg' ->
    match m.Dialog with
    | Some(DetailForm m') ->
      let detailModel, detailMsg = DetailForm.update msg' m'

      { m with
          Dialog = detailModel |> DetailForm |> Some },
      detailMsg
    | _ -> m, Cmd.none


let bindings () =
  [ "QueueItems"
    |> Binding.subModelSeq (
      (fun m -> m.QueueItems),
      (fun e -> e.Id),
      (fun () ->
        [ "Title" |> Binding.oneWay (fun (_, e) -> getTitle e)
          "Remove" |> Binding.cmd (fun (_, e) -> Remove e.Id)
          "Modify" |> Binding.cmd (fun (_, e) -> Modify e.Id)
          "Reset"
          |> Binding.cmdIf (fun (_, (e: Queue)) ->
            match e.Title with
            | Some _ -> Some(ResetChange e.Id)
            | None -> None )])
    )
    "DetailFormVisible"
    |> Binding.oneWay (fun m ->
      match m.Dialog with
      | Some(DetailForm _) -> true
      | _ -> false)
    "DetailForm"
    |> Binding.subModelOpt (
      (fun m ->
        match m.Dialog with
        | Some(DetailForm m') -> Some m'
        | _ -> None),
      snd,
      DetailFormMsg,
      DetailForm.bindings
    )
    "StatusMsg" |> Binding.oneWay (fun m -> m.StatusMsg)
    "Load" |> Binding.cmd QueuePageMsg.RequestLoad
    "ClearList" |> Binding.cmd ClearList
    "GoNext" |> Binding.cmd GoNext ]
