module Kobalt.Core.MainWindow

open System
open System.Windows
open Elmish
open Elmish.WPF

type Pages =
  | QueuePage of QueuePage.Model
  | RulesPage of RulesPage.Model

type MainWindow =
  { CurrentPage: Pages option
    LastPage: Pages option
    Config: Config }

type Msg =
  | Exit
  | GoBack
  | ShowQueuePage
  | QueuePageMsg of QueuePage.Msg
  | ShowRulesPage
  | RulesPageMsg of RulesPage.Msg

let init () =
  let config = Config.load ()

  { CurrentPage = None
    LastPage = None 
    Config = config },
  Cmd.ofMsg ShowQueuePage

let update msg m =
  match msg with
  | Exit -> 
    Application.Current.Shutdown()
    m, Cmd.none
  | GoBack ->
    { m with
        CurrentPage = m.LastPage
        LastPage = m.CurrentPage },
    Cmd.none
  | ShowQueuePage ->
    let m', msg' = QueuePage.init m.Config

    { m with CurrentPage = Some(QueuePage m') }, Cmd.map QueuePageMsg msg'
  | QueuePageMsg QueuePage.GoRules -> m, Cmd.ofMsg ShowRulesPage
  | QueuePageMsg msg' ->
    match m.CurrentPage with
    | Some(QueuePage m') ->
      let m', queueMsg = QueuePage.update msg' m'

      { m with CurrentPage = Some(QueuePage m') }, Cmd.map QueuePageMsg queueMsg
    | _ -> m, Cmd.none
  | ShowRulesPage -> 
    let m', msg' = RulesPage.init m.Config

    { m with 
        CurrentPage = Some(RulesPage m')
        LastPage = m.CurrentPage },
    Cmd.map RulesPageMsg msg'
  | RulesPageMsg RulesPage.GoBack -> m, Cmd.ofMsg GoBack
  | RulesPageMsg RulesPage.Save -> 
    let rules, favpath =
      match m.CurrentPage with
      | Some(RulesPage m') -> 
          m'.Items 
          |> List.map (fun r -> r.Rule), m'.FavPath
      | _ -> m.Config.Rules, m.Config.FavoritePath
    let config = 
      { m.Config with 
          Rules = rules 
          FavoritePath = favpath }
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
    let m', msg' = RulesPage.update msg' m'

    { m with
        CurrentPage = Some(RulesPage m')},
    Cmd.map RulesPageMsg msg'
    | _ -> m, Cmd.none

let bindings () =
  [ "QueuePageVisible"
    |> Binding.oneWay(fun m ->
      match m.CurrentPage with
      | Some(QueuePage _) -> true
      | _ -> false
    )
    "QueuePage"
    |> Binding.subModelOpt(
      (fun m ->
        match m.CurrentPage with
        | Some(QueuePage m') -> Some m'
        | _ -> None),
      snd,
      QueuePageMsg,
      QueuePage.bindings
    )
    "RulesPageVisible"
    |> Binding.oneWay(fun m ->
      match m.CurrentPage with
      | Some(RulesPage _) -> true
      | _ -> false
    )
    "RulesPage"
    |> Binding.subModelOpt(
      (fun m ->
        match m.CurrentPage with
        | Some(RulesPage m') -> Some m'
        | _ -> None),
      snd,
      RulesPageMsg,
      RulesPage.bindings
      ) ]

let designVm = ViewModel.designInstance (init () |> fst) (bindings ())
