module Kobalt.Core.MainWindow

open System
open System.IO
open Elmish
open Elmish.WPF

type Pages =
  | QueuePage of QueuePage.Model
  | ProgressPage of ProgressPage.Model
  | RulesPage of RulesPage.Model
  | OptionsPage of OptionsPage.Model

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
  | ShowOptionsPage
  | OptionsPageMsg of OptionsPage.Msg

let init () =
  let config = Config.load ()

  { CurrentPage = None
    LastPage = None 
    Config = config },
  Cmd.ofMsg ShowQueuePage

let update msg m =
  match msg with
  | GoBack ->
    { m with
        CurrentPage = m.LastPage
        LastPage = m.CurrentPage },
    Cmd.none
  | ShowQueuePage ->
    let model, _ = QueuePage.init m.Config
    let cmd = 
      match m.Config.AutoScanPath with
      | None -> Cmd.none
      | Some p ->
        let isVideo (path: string) =
          let ext = Path.GetExtension(path)
          ext = ".mp4" || ext = ".mkv"

        let files = 
          Directory.EnumerateFiles(p)
          |> Seq.filter isVideo
          |> Seq.toArray
        
        Cmd.ofMsg (QueuePageMsg(QueuePage.LoadSuccess files))

    { m with CurrentPage = Some(QueuePage model) }, cmd
  | QueuePageMsg QueuePage.Msg.GoNext -> m, Cmd.ofMsg ShowProgressPage
  | QueuePageMsg QueuePage.Msg.GoRules -> m, Cmd.ofMsg ShowRulesPage
  | QueuePageMsg QueuePage.Msg.GoOptions -> m, Cmd.ofMsg ShowOptionsPage
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
  | ShowOptionsPage ->
    { m with
        CurrentPage = 
          let path = 
            match m.Config.AutoScanPath with
            | None -> OptionsPage.init String.Empty
            | Some t -> OptionsPage.init t
          
          path
          |> OptionsPage 
          |> Some
        LastPage = m.CurrentPage },
    Cmd.none
  | OptionsPageMsg OptionsPage.GoBack -> m, Cmd.ofMsg GoBack
  | OptionsPageMsg OptionsPage.Save -> 
    let model = 
      match m.CurrentPage with
      | Some(OptionsPage m') ->
        let path = 
          match m'.AutoScanPath |> String.IsNullOrWhiteSpace with
          | true -> None
          | false -> Some m'.AutoScanPath

        let config = { m.Config with AutoScanPath = path }
        Config.save config
        { m with Config = config } 
      | _ -> m

    model, Cmd.ofMsg GoBack
  | OptionsPageMsg msg' ->
    match m.CurrentPage with
    | Some(OptionsPage m') ->
      let model, message = OptionsPage.update msg' m'

      { m with CurrentPage = model |> OptionsPage |> Some },
      Cmd.map OptionsPageMsg message
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
      RulesPage.bindings)
    "OptionsPageVisible"
    |> Binding.oneWay (fun m ->
      match m.CurrentPage with
      | Some(OptionsPage _) -> true
      | _ -> false)
    "OptionsPage"
    |> Binding.subModelOpt (
      (fun m ->
        match m.CurrentPage with
        | Some(OptionsPage m') -> Some m'
        | _ -> None),
      snd,
      OptionsPageMsg,
      OptionsPage.bindings) ]

let designVm = ViewModel.designInstance (init () |> fst) (bindings ())
