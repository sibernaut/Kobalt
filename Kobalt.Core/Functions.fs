﻿namespace Kobalt.Core

open System
open System.IO
type Rgx = System.Text.RegularExpressions.Regex
open System.Text.Json
open System.Text.Json.Serialization


module Rule =
  let create search replace =
    let pattern =
      { Search = search
        Replace = replace }

    Regular pattern

  let empty =
    create String.Empty String.Empty

  let getField rule =
    match rule with
    | Regular p
    | Regex p -> p.Search, p.Replace

  let setRegular rule =
    match rule with
    | Regex pattern -> Regular pattern
    | _ -> rule

  let setRegex rule =
    match rule with
    | Regular pattern -> Regex pattern
    | _ -> rule

  let apply rule (input: string) =
    match rule with
    | Regular pattern -> input.Replace(pattern.Search, pattern.Replace)
    | Regex pattern -> Rgx.Replace(input, pattern.Search, pattern.Replace)


module Rules =
  let apply (rules: Rules) (input: string) =
    rules
    |> List.filter (
      fun r ->
        match r with
        | Regular p -> input.Contains(p.Search)
        | Regex p -> Rgx.Match(input, p.Search).Success )
    |> fun rules ->
      match rules with
      | [] -> input
      | _ -> 
        let mutable result = input

        rules |> List.iter (fun rule -> result <- Rule.apply rule result)

        result


module Video =
  let empty =
    { FilePath = String.Empty
      Title = None }

  let create path =
    { FilePath = path
      Title = None }

  let getTitle rules v =
    match v.Title with
    | Some t -> t
    | None -> 
      let title = Path.GetFileNameWithoutExtension(v.FilePath)
      Rules.apply rules title

  let updateTitle t v = { v with Title = Some t }

  let resetTitle v = { v with Title = None }


module Config =

  let empty =
    { Rules = List.Empty }

  let options =
    let jsonOpt = JsonSerializerOptions()
    jsonOpt.WriteIndented <- true
      
    JsonFSharpOptions.Default()
      .AddToJsonSerializerOptions(jsonOpt)

    jsonOpt

  let save config  =
    let serialize (config: Config) =
      JsonSerializer.Serialize(config, options)

    let json = serialize config
    let path = Path.Combine(AppContext.BaseDirectory, "config.json")

    File.WriteAllText(path, json)

  let rec load () =
    let path = Path.Combine(AppContext.BaseDirectory, "config.json")

    match File.Exists(path) with
    | true ->
        let deserialize (json: string) : Config =
          JsonSerializer.Deserialize(json, options)

        File.ReadAllText(path)
        |> deserialize  
    | false ->
        let file = File.Create(path)
        file.Close()
        save empty 
        load ()
        
