module Kobalt.Core.MainWindow

open Elmish
open Elmish.WPF

type Pages =
  | QueuePage of QueuePage.Model
  | ProgressPage of ProgressPage.Model
//| RulesPage of RulesPage.Model
//| OptionsPage of OptionsPage.Model

type Model =
  { CurrentPage: Pages option
    LastPage: Pages option }

let init () =
  let model, _ = QueuePage.init ()

  { CurrentPage = Some <| QueuePage model
    LastPage = None },
  Cmd.none

type Msg =
  | GoBack
  | ShowQueuePage
  | QueuePageMsg of QueuePage.Msg
  | ShowProgressPage
  | ProgressPageMsg of ProgressPage.Msg

let sendItems m =
  match m.CurrentPage with
  | Some(QueuePage m') -> m'.QueueItems |> List.map (fun i -> i.FileName)
  | _ -> List.empty

let update msg m =
  match msg with
  | GoBack ->
    { m with
        CurrentPage = m.LastPage
        LastPage = m.CurrentPage },
    Cmd.none
  | ShowQueuePage ->
    let queueModel, _ = QueuePage.init ()

    { m with
        CurrentPage = Some <| QueuePage queueModel },
    Cmd.none
  | QueuePageMsg QueuePage.GoNext -> m, Cmd.ofMsg ShowProgressPage
  | QueuePageMsg msg' ->
    match m.CurrentPage with
    | Some(QueuePage m') ->
      let queueModel, queueMsg = QueuePage.update msg' m'

      { m with
          CurrentPage = queueModel |> QueuePage |> Some },
      Cmd.map QueuePageMsg queueMsg
    | _ -> m, Cmd.none
  | ShowProgressPage ->
    let items =
      match m.CurrentPage with
      | Some(QueuePage m') -> m'.QueueItems |> List.map (fun i -> i.FileName)
      | _ -> List.Empty

    let progressModel, _ = ProgressPage.init ()

    { m with
        CurrentPage = Some <| ProgressPage progressModel
        LastPage = m.CurrentPage },
    Cmd.ofMsg (ProgressPageMsg(ProgressPage.RequestLoad items))
  | ProgressPageMsg ProgressPage.GoBack -> m, Cmd.ofMsg GoBack
  | ProgressPageMsg msg' ->
    match m.CurrentPage with
    | Some(ProgressPage m') ->
      let progressModel, progressMsg = ProgressPage.update msg' m'

      { m with
          CurrentPage = progressModel |> ProgressPage |> Some },
      Cmd.map ProgressPageMsg progressMsg
    | _ -> m, Cmd.none

let bindings () =
  [ "QueuePageVisible"
    |> Binding.oneWay (fun m ->
      match m.CurrentPage with
      | Some(QueuePage _) -> true
      | _ -> false)
    "QueuePage"
    |> Binding.subModelOpt (
      (fun m ->
        match m.CurrentPage with
        | Some(QueuePage m') -> Some m'
        | _ -> None),
      snd,
      QueuePageMsg,
      QueuePage.bindings
    )
    "ProgressPageVisible"
    |> Binding.oneWay (fun m ->
      match m.CurrentPage with
      | Some(ProgressPage _) -> true
      | _ -> false)
    "ProgressPage"
    |> Binding.subModelOpt (
      (fun m ->
        match m.CurrentPage with
        | Some(ProgressPage m') -> Some m'
        | _ -> None),
      snd,
      ProgressPageMsg,
      ProgressPage.bindings
    ) ]

let designVm = ViewModel.designInstance (init () |> fst) (bindings ())
