//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
