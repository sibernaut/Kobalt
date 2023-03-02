module Kobalt.Core.RulesPage

open System
open Elmish
open Elmish.WPF


type Dialog =
  | ItemEditor of RuleEditor.Model

type Item =
  { Id: Guid 
    Rule: Rule }

type Model =
  { Items: Item list
    Selected: Guid option
    Dialog: Dialog option }

type Msg =
  | GoBack
  | AddNew
  | Create
  | SetSelected of Guid option
  | Modify
  | Update
  | CloseDialog
  | Remove
  | Save
  | ItemEditor of RuleEditor.Msg

let init rules =
  let toItem e =
    { Id = Guid.NewGuid()
      Rule = e }

  { Items = 
      rules 
      |> List.map toItem
    Selected = None
    Dialog = None },
  Cmd.none

let toRule (model: RuleEditor.Model) = 
  let rule = 
    let r = Rule.create model.SearchFor model.ReplaceWith

    match model.IsRegex with
    | false -> r
    | true -> r |> Rule.setRegex

  { Id = model.Id
    Rule = rule }

let update msg m =
  match msg with
  | GoBack -> m, Cmd.none // managed by parent
  | Save -> m, Cmd.none // managed by parent
  | CloseDialog -> { m with Dialog = None }, Cmd.none
  | AddNew -> 
    { m with 
        Dialog = Some(Dialog.ItemEditor RuleEditor.empty)
        Selected = None }, 
    Cmd.none
  | Create -> 
    match m.Dialog with
    | None -> m, Cmd.none
    | Some(Dialog.ItemEditor m') ->
      let isEmpty = 
          m'.SearchFor |> String.IsNullOrWhiteSpace
          || m'.ReplaceWith |> String.IsNullOrWhiteSpace

      match isEmpty with
      | false -> 
        { m with Items = m.Items @ [ toRule m' ] }, Cmd.ofMsg CloseDialog
      | true -> m, Cmd.none
  | SetSelected i -> { m with Selected = i }, Cmd.none
  | Modify ->
    let toEditorItem item =
      let search, replace, isregex =
        match item.Rule with
        | Regular p -> p.Search, p.Replace, false
        | Regex p -> p.Search, p.Replace, true

      RuleEditor.init search replace isregex item.Id
    
    let m' =
      m.Items 
      |> List.find (fun e -> e.Id = m.Selected.Value)
      |> toEditorItem

    { m with Dialog = Some(Dialog.ItemEditor m') }, Cmd.none
  | Update -> 
    match m.Dialog with
    | Some(Dialog.ItemEditor m') ->
      let isEmpty = 
        m'.SearchFor |> String.IsNullOrWhiteSpace
        || m'.ReplaceWith |> String.IsNullOrWhiteSpace

      let updateItem e = 
        match e.Id with 
        | guid when guid = m'.Id -> toRule m' 
        | _ -> e

      let items =
        m.Items
        |> List.map updateItem

      match isEmpty with
      | false -> { m with Items = items }, Cmd.ofMsg CloseDialog
      | true -> m, Cmd.none
    | None -> m, Cmd.none
  | Remove -> 
    let isNotSelected e = e.Id <> m.Selected.Value

    { m with Items = m.Items |> List.filter isNotSelected }, Cmd.none
  | ItemEditor RuleEditor.Submit -> 
    match m.Selected with
    | None -> m, Cmd.ofMsg Create
    | Some _ -> m, Cmd.ofMsg Update
  | ItemEditor RuleEditor.Cancel -> { m with Dialog = None }, Cmd.none
  | ItemEditor msg' ->
    match m.Dialog with
    | Some(Dialog.ItemEditor m') ->
      let itemModel, itemMsg = RuleEditor.update msg' m'

      { m with Dialog = Some(Dialog.ItemEditor itemModel) }, itemMsg
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
    "Modify" |> Binding.cmdIf(Modify, (fun m -> m.Selected.IsSome))
    "Remove" |> Binding.cmdIf(Remove, (fun m -> m.Selected.IsSome))
    "Save" |> Binding.cmd Save
    "RuleEditorVisible"
    |> Binding.oneWay(fun m ->
      match m.Dialog with
      | Some(Dialog.ItemEditor _) -> true
      | _ -> false
    )
    "RuleEditor"
    |> Binding.subModelOpt(
      (fun m ->
        match m.Dialog with
        | Some(Dialog.ItemEditor m') -> Some m'
        | _ -> None),
      snd,
      ItemEditor,
      RuleEditor.bindings
    ) ]
