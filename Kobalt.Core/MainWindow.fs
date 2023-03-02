//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kobalt.Core.MainWindow

open System
open System.Windows
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
  | Exit
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
  | QueuePageMsg QueuePage.GoNext -> m, Cmd.ofMsg ShowProgressPage
  | QueuePageMsg QueuePage.GoRules -> m, Cmd.ofMsg ShowRulesPage
  | QueuePageMsg QueuePage.GoOptions -> m, Cmd.ofMsg ShowOptionsPage
  | QueuePageMsg msg' ->
    match m.CurrentPage with
    | Some(QueuePage m') ->
      let m', queueMsg = QueuePage.update msg' m'

      { m with CurrentPage = Some(QueuePage m') }, Cmd.map QueuePageMsg queueMsg
    | _ -> m, Cmd.none
  | ShowProgressPage ->
    let items =
      match m.CurrentPage with
      | Some(QueuePage m') ->
        m'.Items
        |> List.map (fun i -> i.Video)
      | _ -> List.Empty

    let m', msg' = ProgressPage.init m.Config items

    { m with
        CurrentPage = Some(ProgressPage m')
        LastPage = m.CurrentPage },
    Cmd.map ProgressPageMsg msg'
  | ProgressPageMsg ProgressPage.Exit -> m, Cmd.ofMsg Exit
  | ProgressPageMsg ProgressPage.GoBack -> m, Cmd.ofMsg GoBack
  | ProgressPageMsg msg' ->
    match m.CurrentPage with
    | Some(ProgressPage m') ->
      let model, message = ProgressPage.update msg' m'

      { m with
          CurrentPage = Some(ProgressPage model) },
      Cmd.map ProgressPageMsg message
    | _ -> m, Cmd.none
  | ShowRulesPage -> 
    let m', msg' = RulesPage.init m.Config.Rules

    { m with 
        CurrentPage = Some(RulesPage m')
        LastPage = m.CurrentPage },
    Cmd.map RulesPageMsg msg'
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
    let m', msg' = RulesPage.update msg' m'

    { m with
        CurrentPage = Some(RulesPage m')},
    Cmd.map RulesPageMsg msg'
    | _ -> m, Cmd.none
  | ShowOptionsPage ->
    let m' = 
      match m.Config.FavoritePath with
      | None -> OptionsPage.init String.Empty
      | Some t -> OptionsPage.init t
          
    { m with
        CurrentPage = Some(OptionsPage m')
        LastPage = m.CurrentPage },
    Cmd.none
  | OptionsPageMsg OptionsPage.GoBack -> m, Cmd.ofMsg GoBack
  | OptionsPageMsg OptionsPage.Save -> 
    let model = 
      match m.CurrentPage with
      | Some(OptionsPage m') ->
        let path = 
          match m'.FavPath |> String.IsNullOrWhiteSpace with
          | true -> None
          | false -> Some m'.FavPath

        let config = { m.Config with FavoritePath = path }
        Config.save config
        { m with Config = config } 
      | _ -> m

    model, Cmd.ofMsg GoBack
  | OptionsPageMsg msg' ->
    match m.CurrentPage with
    | Some(OptionsPage m') ->
      let m', msg' = OptionsPage.update msg' m'

      { m with CurrentPage = Some(OptionsPage m') },
      Cmd.map OptionsPageMsg msg'
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
    "ProgressPageVisible"
    |> Binding.oneWay(fun m ->
      match m.CurrentPage with
      | Some(ProgressPage _) -> true
      | _ -> false
    )
    "ProgressPage"
    |> Binding.subModelOpt(
      (fun m ->
        match m.CurrentPage with
        | Some(ProgressPage m') -> Some m'
        | _ -> None),
      snd,
      ProgressPageMsg,
      ProgressPage.bindings
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
      )
    "OptionsPageVisible"
    |> Binding.oneWay(fun m ->
      match m.CurrentPage with
      | Some(OptionsPage _) -> true
      | _ -> false
    )
    "OptionsPage"
    |> Binding.subModelOpt(
      (fun m ->
        match m.CurrentPage with
        | Some(OptionsPage m') -> Some m'
        | _ -> None),
      snd,
      OptionsPageMsg,
      OptionsPage.bindings
    ) ]

let designVm = ViewModel.designInstance (init () |> fst) (bindings ())
