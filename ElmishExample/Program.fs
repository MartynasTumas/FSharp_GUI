// Learn more about F# at http://fsharp.org
namespace Main
    module Main =
        open System
        open Elmish.WPF
        open ElmishExample.View
        
        [<EntryPoint; STAThread>]
        let main argv =
            Program.mkSimpleWpf Game.init Game.update Game.bindings
            |> Program.runWindow (GameWindow())
            // return an integer exit code
