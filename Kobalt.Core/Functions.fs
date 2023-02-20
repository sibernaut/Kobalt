namespace Kobalt.Core

open System


module Video =
  let empty =
    { FilePath = String.Empty
      Title = None }

  let create path =
    { FilePath = path
      Title = None }

  let getTitle v =
    match v.Title with
    | Some t -> t
    | None -> System.IO.Path.GetFileNameWithoutExtension(v.FilePath)

  let updateTitle t v = { v with Title = Some t }

  let resetTitle v = { v with Title = None }
