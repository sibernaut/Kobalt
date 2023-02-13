module Kobalt.Core.App

open Elmish
open Elmish.WPF

let main window =
  Program.mkProgramWpf MainWindow.init MainWindow.update MainWindow.bindings
  |> Program.withConsoleTrace
  |> Program.startElmishLoop
    { ElmConfig.Default with
        LogConsole = true
        Measure = true }
    window
