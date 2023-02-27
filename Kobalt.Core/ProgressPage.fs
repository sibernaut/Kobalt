module Kobalt.Core.ProgressPage

open System
open System.Diagnostics
open Elmish
open Elmish.WPF

type Item =
  { Path: string
    Title: string }

type Model = 
  { Items: Item list
    Text: string }

type Msg =
  | RequestLoad
  | LoadSuccess of string
  | Update
  | GoBack

let createItem path title =
  { Path = path
    Title = title }

let init items =
  { Items = items
    Text = "Processing..." }, 
  Cmd.none

let save (path: string) title =
  let ext = System.IO.Path.GetExtension(path)

  match ext with
  | ".mp4" ->
    let tfile = TagLib.File.Create(path)
    tfile.Tag.Title <- title
    tfile.Save()
    sprintf "%s" title
  | ".mkv" ->
    let startinfo = 
      new ProcessStartInfo(
        UseShellExecute = false,
        CreateNoWindow = true,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        FileName = "mkvpropedit.exe",
        Arguments = sprintf "--edit info --set title=\"%s\" \"%s\"" title path )

    let ps = Process.Start(startinfo)
    ps.Start() |> ignore
    ps.WaitForExit()
    sprintf "%s\r\n%s" title (ps.StandardOutput.ReadToEnd())
  | _ -> 
    sprintf "Cannot process \"%s\". Filetype unsupported." title

let update msg m =
  match msg with
  | RequestLoad  -> 
    let load (dispatch: Msg -> unit) =
      async {
        for item in m.Items do
          dispatch (LoadSuccess (save item.Path item.Title))
          do! Async.Sleep(TimeSpan.FromSeconds(1))

        dispatch (LoadSuccess "Done")
      }
      |> Async.StartImmediate 
    m, Cmd.ofSub load
  | LoadSuccess t -> { m with Text = sprintf "%s\r\n%s" m.Text t }, Cmd.none
  | Update -> { m with Text = "Updated" }, Cmd.none
  | GoBack -> m, Cmd.none // managed by parent model

let bindings () =
  [ "Text" |> Binding.oneWay (fun m -> m.Text)
    "Update" |> Binding.cmd Update
    "GoBack" |> Binding.cmd GoBack ]
