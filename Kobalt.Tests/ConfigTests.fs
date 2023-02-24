module ConfigTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Kobalt.Core


[<Fact>]
let ``Create file if not exist, load, then save`` () =
  let config =
    { Rules = Tests.rules }

  let dir = 
    [| Path.GetTempPath()
       "Kobalt"|]
    |> Path.Combine

  let path = 
    [| dir
       "config.json" |]
    |> Path.Combine

  if not (Directory.Exists(dir)) then
    Directory.CreateDirectory(dir)
    |> ignore

  if File.Exists(path) then
    File.Delete(path)

  Config.load () 
  |> should equal Config.empty

  Config.save config 

  Config.load ()
  |> should equal config
