namespace Kobalt.Core

open System


[<AutoOpen>]
module DomainTypes =

  type DetailForm =
    { Id: Guid
      Text: string }

  type DetailFormMsg =
    | TextInput of string
    | Submit
    | Cancel

  type Queue =
    { Id: Guid
      FilePath: string
      Title: string option
      IsSelected: bool
      IsModified: bool }

  type QueuePageDialog =
    | DetailForm of DetailForm

  type QueuePage =
    { QueueItems: Queue list
      Dialog: QueuePageDialog option
      StatusMsg: string }

  type QueuePageMsg =
    | RequestLoad
    | LoadSuccess of string[]
    | LoadCancelled
    | LoadFailed of exn
    | Remove of Guid
    | Modify of Guid
    | ResetChange of Guid
    | ClearList
    | GoNext
    | DetailFormMsg of DetailFormMsg

  type ProgressPageItem =
    { Title: string
      Path: string }

  type ProgressPage = 
    { Items: ProgressPageItem list
      Text: string }

  type ProgressPageMsg =
    | RequestLoad
    | LoadSuccess of string
    | Update
    | GoBack

  type MainWindowPages =
    | QueuePage of QueuePage
    | ProgressPage of ProgressPage
  //| RulesPage of RulesPage
  //| OptionsPage of OptionsPage

  type MainWindow =
    { CurrentPage: MainWindowPages option
      LastPage: MainWindowPages option }

  type MainWindowMsg =
    | GoBack
    | ShowQueuePage
    | QueuePageMsg of QueuePageMsg
    | ShowProgressPage
    | ProgressPageMsg of ProgressPageMsg

