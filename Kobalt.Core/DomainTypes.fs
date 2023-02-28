namespace Kobalt.Core


[<AutoOpen>]
module Domains =

  type Video =
    { FilePath: string 
      Title: string option }

  type Pattern =
    { Search: string
      Replace: string }

  type Rule =
    | Regular of Pattern
    | Regex of Pattern

  type Rules = Rule list

  type Config = 
    { Rules: Rules
      AutoScanPath: string option }
