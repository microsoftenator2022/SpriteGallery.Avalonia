namespace SpriteGallery.Avalonia

open Avalonia
open Avalonia.Controls
open Avalonia.Themes.Fluent

open Elmish

open Avalonia.FuncUI
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Controls.ApplicationLifetimes

open SpriteGallery.Avalonia.Common
open SpriteGallery.Avalonia.Views

[<RequireQualifiedAccess>]
module App =
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type View =
    | LoadFileView
    | GridView

    type State = { Window : Window; View : View; LoadFileState : LoadFileView.State; GridState : GridView.State }

    let init (window : Window) =
        {
            Window = window
            LoadFileState = LoadFileView.init window
            GridState = GridView.init()
            View = LoadFileView
        },
        Cmd.none

    type Msg =
    | LoadFileMsg of LoadFileView.Msg
    | GridMsg of GridView.Msg

    let update msg state =
        match msg with
        | LoadFileMsg lfm ->
            let lfs, cmd = state.LoadFileState |> LoadFileView.update lfm
            let state, cmds = { state with LoadFileState = lfs }, [Cmd.map LoadFileMsg cmd]

            let state, cmds =
                if lfs.Complete then
                    { state with
                        View = GridView
                    }, (Cmd.map GridMsg (GridView.Msg.UpdateSprites lfs.Sprites |> Cmd.ofMsg)) :: cmds
                else state, cmds
            
            state, cmds |> Cmd.batch
            
        | GridMsg gm ->
            let gs, cmd = state.GridState |> GridView.update gm
            
            { state with GridState = gs },
            Cmd.map GridMsg cmd

    let view state (dispatch : Dispatch<Msg>) =
        View.createGeneric<ExperimentalAcrylicBorder> [
            ExperimentalAcrylicBorder.material (acrylicMaterial Avalonia.Media.Colors.Black)

            ExperimentalAcrylicBorder.child (
                Panel.create [
                    Panel.children [
                        match state.View with
                        | LoadFileView ->
                            LoadFileView.view state.LoadFileState (LoadFileMsg >> dispatch) :> IView
                        | GridView ->
                            GridView.view state.GridState (GridMsg >> dispatch) :> IView
                    ]
                ]
            )
        ]

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- ""
        // base.Height <- 400.0
        // base.Width <- 400.0

        this.TransparencyLevelHint <- [WindowTransparencyLevel.AcrylicBlur]
        this.Background <- Avalonia.Media.Brushes.Transparent

        //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true
        Elmish.Program.mkProgram (fun () -> App.init this) App.update App.view
        |> Program.withHost this
        // |> Program.withConsoleTrace
        |> Program.runWithAvaloniaSyncDispatch ()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Default

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let mainWindow = MainWindow()
            desktopLifetime.MainWindow <- mainWindow
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)