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
  { Items = rules |> List.map (fun e -> { Id = Guid.NewGuid(); Rule = e })
    Selected = None
    Dialog = None },
  Cmd.none

let toRule (model: RuleEditor.Model) = 
  match model.IsRegex with
  | false -> 
    { Id = model.Id
      Rule = Rule.create model.SearchFor model.ReplaceWith }
  | true -> 
    { Id = model.Id
      Rule = 
        Rule.create model.SearchFor model.ReplaceWith
        |> Rule.setRegex }

let update msg m =
  match msg with
  | GoBack -> m, Cmd.none // managed by parent
  | Save -> m, Cmd.none // managed by parent
  | CloseDialog -> { m with Dialog = None }, Cmd.none
  | AddNew -> 
    { m with 
        Dialog = Some(Dialog.ItemEditor (RuleEditor.initEmpty))
        Selected = None }, 
    Cmd.none
  | Create -> 
    match m.Dialog with
    | None -> m, Cmd.none
    | Some(Dialog.ItemEditor m') ->
      { m with 
          Items = m.Items @ [ toRule m' ]
          Dialog = None },
      Cmd.ofMsg CloseDialog
  | SetSelected i -> { m with Selected = i }, Cmd.none
  | Modify ->
    { m with Dialog = 
              m.Items 
              |> List.find (fun e -> e.Id = m.Selected.Value)
              |> (fun r -> 
                  let search, replace, isregex =
                    match r.Rule with
                    | Regular p -> p.Search, p.Replace, false
                    | Regex p -> p.Search, p.Replace, true

                  RuleEditor.init search replace isregex r.Id)
              |> Dialog.ItemEditor 
              |> Some }, 
    Cmd.none
  | Update -> 
    match m.Dialog with
    | Some(Dialog.ItemEditor m') ->
      { m with
          Items = 
            m.Items
            |> List.map (fun e -> if e.Id = m'.Id then toRule m' else e)
          Dialog = None }, 
      Cmd.none
    | None -> m, Cmd.ofMsg CloseDialog
  | Remove -> 
    { m with Items = 
              m.Items
              |> List.filter (fun e -> e.Id <> m.Selected.Value) },
    Cmd.none
  | ItemEditor RuleEditor.Submit -> 
    match m.Selected with
    | None -> m, Cmd.ofMsg Create
    | Some _ -> m, Cmd.ofMsg Update
  | ItemEditor RuleEditor.Cancel ->
    { m with Dialog = None }, Cmd.none
  | ItemEditor msg' ->
    match m.Dialog with
    | Some(Dialog.ItemEditor m') ->
      let itemModel, itemMsg = RuleEditor.update msg' m'

      { m with Dialog = itemModel |> Dialog.ItemEditor |> Some },
      itemMsg
    | None -> m, Cmd.none

let bindings () =
  [ "GoBack" |> Binding.cmd GoBack
    "Items" 
    |> Binding.subModelSeq(
      (fun m -> m.Items),
      (fun e -> e.Id),
      (fun () ->
        [ "ID" |> Binding.oneWay (fun (_, e) -> e.Id)
          "SearchFor" |> Binding.oneWay (fun (_, e) -> Rule.getField e.Rule |> fst)
          "ReplaceWith" |> Binding.oneWay (fun (_, e) -> Rule.getField e.Rule |> snd) ]))
    "Selected" |> Binding.twoWayOpt((fun m -> m.Selected), SetSelected)
    "AddNew" |> Binding.cmd AddNew
    "Modify" |> Binding.cmdIf(Modify, (fun m -> m.Selected.IsSome))
    "Remove" |> Binding.cmdIf(Remove, (fun m -> m.Selected.IsSome))
    "Save" |> Binding.cmd Save
    "RuleEditorVisible"
    |> Binding.oneWay (fun m ->
      match m.Dialog with
      | Some(Dialog.ItemEditor _) -> true
      | _ -> false)
    "RuleEditor"
    |> Binding.subModelOpt (
      (fun m ->
        match m.Dialog with
        | Some(Dialog.ItemEditor m') -> Some m'
        | _ -> None),
      snd,
      ItemEditor,
      RuleEditor.bindings) ]
