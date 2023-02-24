module Kobalt.Core.RulesPage

open System
open Elmish
open Elmish.WPF


type Dialog =
  | ItemEditor of RuleEditor.Model

type Model =
  { Items: RuleEditor.Model list
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
  { Items = rules |> List.map (fun e -> RuleEditor.init e (Guid.NewGuid()))
    Selected = None
    Dialog = None },
  Cmd.none

let update msg m =
  match msg with
  | GoBack -> m, Cmd.none // managed by parent
  | Save -> m, Cmd.none // managed by parent
  | CloseDialog -> { m with Dialog = None }, Cmd.none
  | AddNew -> 
    { m with 
        Dialog = Some(Dialog.ItemEditor (RuleEditor.init Rule.empty (Guid.NewGuid())))
        Selected = None }, 
    Cmd.none
  | Create -> 
    match m.Dialog with
    | None -> m, Cmd.none
    | Some(Dialog.ItemEditor m') ->
      { m with 
          Items = m.Items @ [ m']
          Dialog = None },
      Cmd.ofMsg CloseDialog
  | SetSelected i -> { m with Selected = i }, Cmd.none
  | Modify ->
    { m with Dialog = 
              m.Items 
              |> List.find (fun e -> e.Id = m.Selected.Value)
              |> (fun r -> RuleEditor.init r.Item r.Id)
              |> Dialog.ItemEditor 
              |> Some }, 
    Cmd.none
  | Update -> 
    match m.Dialog with
    | Some(Dialog.ItemEditor m') ->
      { m with
          Items = 
            m.Items
            |> List.map (fun e -> 
                if e.Id = m.Selected.Value then
                  { e with Item = m'.Item }
                else
                  e)
          Dialog = None }, Cmd.none
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
          "SearchFor" |> Binding.oneWay (fun (_, e) -> Rule.getField e.Item |> fst)
          "ReplaceWith" |> Binding.oneWay (fun (_, e) -> Rule.getField e.Item |> snd) ]))
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
