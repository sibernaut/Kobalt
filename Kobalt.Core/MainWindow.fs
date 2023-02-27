module Kobalt.Core.MainWindow

open Elmish
open Elmish.WPF

type Pages =
  | QueuePage of QueuePage.Model
  | ProgressPage of ProgressPage.Model
  | RulesPage of RulesPage.Model
//| OptionsPage of OptionsPage

type MainWindow =
  { CurrentPage: Pages option
    LastPage: Pages option
    Config: Config }

type Msg =
  | GoBack
  | ShowQueuePage
  | QueuePageMsg of QueuePage.Msg
  | ShowProgressPage
  | ProgressPageMsg of ProgressPage.Msg
  | ShowRulesPage
  | RulesPageMsg of RulesPage.Msg

let init () =
  let config = Config.load ()
  let model, _ = QueuePage.init config

  { CurrentPage = Some <| QueuePage model
    LastPage = None 
    Config = config },
  Cmd.none

let update msg m =
  match msg with
  | GoBack ->
    { m with
        CurrentPage = m.LastPage
        LastPage = m.CurrentPage },
    Cmd.none
  | ShowQueuePage ->
    let queueModel, _ = QueuePage.init m.Config

    { m with
        CurrentPage = Some <| QueuePage queueModel },
    Cmd.none
  | QueuePageMsg QueuePage.Msg.GoNext -> m, Cmd.ofMsg ShowProgressPage
  | QueuePageMsg QueuePage.Msg.GoRules -> m, Cmd.ofMsg ShowRulesPage
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
      | Some(QueuePage m') ->
        m'.Items
        |> List.map (fun i -> ProgressPage.createItem i.Video.FilePath (Video.getTitle m.Config.Rules i.Video))
      | _ -> List.Empty

    let m', _ = ProgressPage.init items

    { m with
        CurrentPage = Some <| ProgressPage m'
        LastPage = m.CurrentPage },
    Cmd.ofMsg (ProgressPageMsg ProgressPage.Msg.RequestLoad)
  | ProgressPageMsg ProgressPage.Msg.GoBack -> m, Cmd.ofMsg GoBack
  | ProgressPageMsg msg' ->
    match m.CurrentPage with
    | Some(ProgressPage m') ->
      let progressModel, progressMsg = ProgressPage.update msg' m'

      { m with
          CurrentPage = progressModel |> ProgressPage |> Some },
      Cmd.map ProgressPageMsg progressMsg
    | _ -> m, Cmd.none
  | ShowRulesPage -> 
    { m with 
        CurrentPage = RulesPage.init m.Config.Rules |> fst |> RulesPage |> Some
        LastPage = m.CurrentPage },
    Cmd.none
  | RulesPageMsg RulesPage.GoBack -> m, Cmd.ofMsg GoBack
  | RulesPageMsg RulesPage.Save -> 
    let rules =
      match m.CurrentPage with
      | Some(RulesPage m') -> 
          m'.Items 
          |> List.map (fun r -> r.Rule)
      | _ -> m.Config.Rules
    let config = { m.Config with Rules = rules }
    let queuePage : Pages option = 
      match m.LastPage with
      | Some(QueuePage m') -> Some(QueuePage { m' with Config = config })
      | _ -> m.LastPage
  
    Config.save config

    { m with 
        Config = config 
        LastPage = queuePage }, 
    Cmd.ofMsg GoBack
  | RulesPageMsg msg' ->
    match m.CurrentPage with
    | Some(RulesPage m') ->
    let rulesModel, rulesMsg = RulesPage.update msg' m'

    { m with
        CurrentPage = rulesModel |> RulesPage |> Some },
    Cmd.map RulesPageMsg rulesMsg
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
      QueuePage.bindings )
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
      ProgressPage.bindings)
    "RulesPageVisible"
    |> Binding.oneWay (fun m ->
      match m.CurrentPage with
      | Some(RulesPage _) -> true
      | _ -> false)
    "RulesPage"
    |> Binding.subModelOpt (
      (fun m ->
        match m.CurrentPage with
        | Some(RulesPage m') -> Some m'
        | _ -> None),
      snd,
      RulesPageMsg,
      RulesPage.bindings) ]

let designVm = ViewModel.designInstance (init () |> fst) (bindings ())
