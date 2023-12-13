namespace Kobalt.Core

open System

[<AutoOpen>]
module Domains =

  type Video =
    { Id: Guid
      FilePath: string 
      FileName: string
      Title: string option 
      ProcessOutput: string }

  type Pattern =
    { Search: string
      Replace: string }

  type Rule =
    | Regular of Pattern
    | Regex of Pattern

  type Rules = Rule list

  type Config = 
    { Rules: Rules
      FavoritePath: string option }
