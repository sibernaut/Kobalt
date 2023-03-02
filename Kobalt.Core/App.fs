//  This Source Code Form is subject to the terms of the Mozilla Public
//  License, v. 2.0. If a copy of the MPL was not distributed with this
//  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
