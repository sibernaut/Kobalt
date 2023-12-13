module Kobalt.Core.RulesPage

open System
open Elmish
open Elmish.WPF


type Item =
  { Id: Guid 
    Rule: Rule }

type Model =
  { Items: Item list
    Selected: Guid option
    Form: RuleEditor.Model option
    FavPath: string option }

type Msg =
  | GoBack
  | SetAutoScanPath of string option
  | SetSelected of Guid option
  | AddNew
  | Remove
  | Save
  | ItemEditor of RuleEditor.Msg

let init config =
  let items = 
    config.Rules 
    |> List.map (fun e ->
      { Id = Guid.NewGuid()
        Rule = e } )

  { Items = items
    Selected = None
    Form = None 
    FavPath = config.FavoritePath },
  Cmd.none

let toRule (model: RuleEditor.Model) = 
  let rule = 
    let r = Rule.create model.SearchFor model.ReplaceWith

    match model.IsRegex with
    | false -> r
    | true -> r |> Rule.setRegex

  { Id = model.Id
    Rule = rule }

let setAutoScanPath p m =
  match p with
  | Some t when t |> String.IsNullOrWhiteSpace -> m
  | None -> m
  | _ -> { m with FavPath = p }

let setSelected i m =
  match i with
  | Some Id ->
    let item =
      m.Items
      |> List.find (fun e -> e.Id = Id)

    let toRuleEditor item =
      let search, replace, isRegex =
        match item.Rule with
        | Regular p -> p.Search, p.Replace, false
        | Regex p -> p.Search, p.Replace, true

      RuleEditor.init search replace isRegex item.Id

    { m with 
        Selected = i 
        Form = Some (toRuleEditor item) }
  | None -> 
    { m with 
        Selected = None 
        Form = None }

let addNew m =
  { m with 
      Form = Some RuleEditor.empty
      Selected = None }

let saveChange m =
  match m.Form with
  | None -> m
  | Some m' ->
    let isExist =
      m.Items
      |> List.exists (fun e -> e.Id = m'.Id)

    match isExist with
    | false -> { m with Items = m.Items @ [ toRule m' ] }
    | true -> 
      let items = m.Items |> List.map (fun e -> if e.Id = m'.Id then toRule m' else e)
      { m with Items = items }
  
let removeItem m =
  match m.Selected with
  | None -> m
  | Some Id -> { m with Items = m.Items |> List.filter (fun e -> e.Id <> Id) }

let update msg m =
  match msg with
  | GoBack -> m, Cmd.none // managed by parent
  | Save -> m, Cmd.none // managed by parent
  | SetAutoScanPath p -> setAutoScanPath p m, Cmd.none
  | SetSelected i -> setSelected i m, Cmd.none
  | AddNew -> addNew m, Cmd.none
  | ItemEditor RuleEditor.Submit -> saveChange m, Cmd.none
  | ItemEditor RuleEditor.Cancel -> m, Cmd.ofMsg(SetSelected m.Selected)
  | Remove -> removeItem m, Cmd.none
  | ItemEditor msg' ->
    match m.Form with
    | Some m' ->
      let itemModel, itemMsg = RuleEditor.update msg' m'

      { m with Form = Some itemModel }, itemMsg
    | None -> m, Cmd.none

let bindings () =
  [ "GoBack" |> Binding.cmd GoBack
    "Items" 
    |> Binding.subModelSeq(
      (fun m -> m.Items),
      (fun e -> e.Id),
      (fun () ->
        [ "ID" |> Binding.oneWay(fun (_, e) -> e.Id)
          "SearchFor" |> Binding.oneWay(fun (_, e) -> Rule.getField e.Rule |> fst)
          "ReplaceWith" |> Binding.oneWay(fun (_, e) -> Rule.getField e.Rule |> snd) ]
      )
    )
    "Selected" |> Binding.twoWayOpt((fun m -> m.Selected), SetSelected)
    "AddNew" |> Binding.cmd AddNew
    "Remove" |> Binding.cmdIf(Remove, (fun m -> m.Selected.IsSome))
    "FavPath" |> Binding.twoWayOpt((fun m -> m.FavPath), SetAutoScanPath)
    "Save" |> Binding.cmd Save
    "RuleEditorVisible"
    |> Binding.oneWay(fun m ->
      match m.Form with
      | Some _ -> true
      | None -> false
    )
    "RuleEditor"
    |> Binding.subModelOpt(
      (fun m ->
        match m.Form with
        | Some m' -> Some m'
        | _ -> None),
      snd,
      ItemEditor,
      RuleEditor.bindings
    ) ]
