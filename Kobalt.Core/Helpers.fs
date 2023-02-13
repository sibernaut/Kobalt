module Kobalt.Core.Helpers

open System
open System.Diagnostics


let save (item: ProgressPageItem) =
  let title, path = item.Title, item.Path
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
