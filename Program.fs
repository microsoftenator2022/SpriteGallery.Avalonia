namespace SpriteGallery.Avalonia

open Avalonia
open Avalonia.Controls
open Avalonia.Media
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

    type Model =
        {
            Window : Window
            View : View
            SpriteDetailsState : SpriteDetailsPanel.Model
            LoadFileState : LoadFileView.Model
            GridState : GridView.Model
            Colors : WindowColors
            // SpritesUpdated : Event<SpritesData option>
            SpriteSelected : Event<Sprite option>
        }

    let init (window : Window) =
        let colors = WindowColors.GetColors window
        {
            Window = window
            LoadFileState = LoadFileView.init window
            SpriteDetailsState = SpriteDetailsPanel.init()
            GridState = GridView.init colors.HighlightBrushOrDefault
            View = LoadFileView
            Colors = colors
            SpriteSelected = new Event<Sprite option>()
        },
        Cmd.none

    type Msg =
    | LoadFileMsg of LoadFileView.Msg
    | GridMsg of GridView.Msg
    // | SpriteDetailsMsg of SpriteDetailsPanel.Msg
    | ChangeView of View
    | UpdateSprites of SpritesData option

    let update msg model =
        match msg with
        | ChangeView view -> { model with View = view }, Cmd.none
        | UpdateSprites spritesData ->
            match model.View with
            | GridView ->
                model, spritesData |> GridView.Msg.UpdateSprites |> Cmd.ofMsg |> (Cmd.map GridMsg)
            | _ -> model, Cmd.none
        // | SpriteDetailsMsg sdm ->
        //     let sdModel, cmd = model.SpriteDetailsState |> SpriteDetailsPanel.update sdm
            
        //     { model with SpriteDetailsState = sdModel }, Cmd.map SpriteDetailsMsg cmd

        | LoadFileMsg lfm ->
            let lfModel, cmd = model.LoadFileState |> LoadFileView.update lfm
            let model, cmd = { model with LoadFileState = lfModel }, Cmd.map LoadFileMsg cmd

            let cmds =
                if lfModel.Complete then
                    let sprites = lfModel.Sprites
                    [Cmd.ofMsg (ChangeView GridView); Cmd.ofMsg (UpdateSprites sprites)]
                else []
                
            model, Cmd.batch (cmd :: cmds)

        | GridMsg gm ->
            let gs, cmd = model.GridState |> GridView.update gm
            
            let model = { model with GridState = gs }
            let cmd = Cmd.map GridMsg cmd

            let cmd = 
                match gm with
                | GridView.Msg.SelectedSpriteChanged s ->
                    model.SpriteSelected.Trigger s
                    cmd
                    // Cmd.batch [cmd; s |> SpriteDetailsPanel.Msg.SpriteSelected |> Cmd.ofMsg |> (Cmd.map SpriteDetailsMsg)]
                | _ -> cmd

            model, cmd

    let spritesViewPart model dispatch (spritesView : IView) =
        SplitView.create [
            SplitView.displayMode SplitViewDisplayMode.Inline
            SplitView.isPaneOpen true
            SplitView.panePlacement SplitViewPanePlacement.Right
            SplitView.openPaneLength 400
            SplitView.paneBackground Colors.Transparent

            SplitView.pane (
                View.createGeneric<ExperimentalAcrylicBorder> [
                    ExperimentalAcrylicBorder.material (acrylicMaterial model.Colors.PanelAcrylicColorOrDefault)

                    ExperimentalAcrylicBorder.child (
                        SpriteDetailsPanel.viewComponent model.SpriteSelected.Publish
                    )
                ]
            )

            SplitView.content spritesView
        ]

    let view model (dispatch : Dispatch<Msg>) =
        View.createGeneric<ExperimentalAcrylicBorder> [
            ExperimentalAcrylicBorder.material (acrylicMaterial model.Colors.AcrylicColorOrDefault)

            ExperimentalAcrylicBorder.child (
                Panel.create [
                    Panel.children [
                        match model.View with
                        | LoadFileView ->
                            LoadFileView.view model.LoadFileState (LoadFileMsg >> dispatch)
                        | GridView ->
                            // GridView.view model.Colors.HighlightBrushOrDefault model.SpritesUpdated.Publish model.SpriteSelected.Trigger 
                            GridView.view model.GridState (GridMsg >> dispatch)
                            |> spritesViewPart model dispatch
                    ]
                ]
            )
        ]

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- ""
        match tryGetAppIcon() with
        | Some icon ->
            base.Icon <- WindowIcon(icon)
        | None -> ()

        base.TransparencyLevelHint <- [WindowTransparencyLevel.AcrylicBlur]
        base.Background <- Avalonia.Media.Brushes.Transparent

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