﻿namespace SpriteGallery.Avalonia

open Elmish

open Avalonia.FuncUI
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Controls.ApplicationLifetimes

open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Rendering
open Avalonia.Themes.Fluent
open Avalonia.VisualTree

open SpriteGallery.Avalonia.Common
open SpriteGallery.Avalonia.Views

[<RequireQualifiedAccess>]
module App =
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type View =
    | LoadFileView
    | GridView
    | ListView

    type Model =
        {
            Window : Window
            View : View
            LoadFileState : LoadFileView.Model
            Colors : WindowColors
            SpritesUpdated : Event<SpritesData option>
            SpriteSelected : Event<Sprite option>
            ChangeToView : View option
        }
    with
        member this.Clipboard = TopLevel.GetTopLevel(this.Window).Clipboard

    let init (window : Window) =
        let colors = WindowColors.GetColors window
        {
            Window = window
            LoadFileState = LoadFileView.init window
            View = LoadFileView
            Colors = colors
            SpritesUpdated = new Event<SpritesData option>()
            SpriteSelected = new Event<Sprite option>()
            ChangeToView = None
        },
        Cmd.none

    type Msg =
    | LoadFileMsg of LoadFileView.Msg
    | ViewChanged of View
    | UpdateSprites of SpritesData option
    | ChangeView of View

    let update msg model =
        match msg with
        | ViewChanged view ->
            // printfn "%A" msg

            { model with View = view; ChangeToView = None }, Cmd.none

        | ChangeView view ->
            // printfn "%A" msg

            { model with ChangeToView = Some view }, Cmd.none

        | UpdateSprites spritesData ->
            model.SpritesUpdated.Trigger spritesData

            model, Cmd.none

        | LoadFileMsg lfm ->
            let lfModel, cmd = model.LoadFileState |> LoadFileView.update lfm
            let model, cmd = { model with LoadFileState = lfModel }, Cmd.map LoadFileMsg cmd

            let cmds =
                if lfm = LoadFileView.Msg.Complete then
                    let sprites = lfModel.Sprites
                    [Cmd.ofMsg (ChangeView GridView); Cmd.ofMsg (UpdateSprites sprites)]
                else []
                
            model, Cmd.batch (cmd :: cmds)

    let panelLength = 400

    let splitView (model : Model) (content : IView) =
        SplitView.create [
            SplitView.displayMode SplitViewDisplayMode.Inline
            SplitView.isPaneOpen (model.View <> LoadFileView)
            SplitView.panePlacement SplitViewPanePlacement.Right
            SplitView.openPaneLength panelLength
            SplitView.paneBackground Colors.Transparent

            SplitView.pane (
                View.createGeneric<ExperimentalAcrylicBorder> [
                    ExperimentalAcrylicBorder.material (acrylicMaterial model.Colors.PanelAcrylicColorOrDefault)

                    ExperimentalAcrylicBorder.child (
                        SpriteDetailsPanel.viewComponent model.SpriteSelected.Publish model.Window
                    )
                ]
            )

            SplitView.content content
        ]
    
    let view model (dispatch : Dispatch<Msg>) =
        let loadTab =
            TabItem.create [
                TabItem.header "Open bundle"
                TabItem.isSelected (model.View = LoadFileView || model.ChangeToView = Some LoadFileView)
                
                TabItem.content (
                    LoadFileView.view panelLength model.LoadFileState (LoadFileMsg >> dispatch)
                )

                TabItem.onIsSelectedChanged (fun s -> if s then ViewChanged LoadFileView |> dispatch)
            ]

        let gridViewTab =
            TabItem.create [
                TabItem.header "Gallery"
                TabItem.isEnabled model.LoadFileState.Complete
                TabItem.isSelected (model.View = GridView || model.ChangeToView = Some GridView)

                TabItem.content (
                    GridView.viewComponent
                        model.LoadFileState.Sprites
                        model.Colors.HighlightBrushOrDefault
                        model.SpriteSelected
                        model.Clipboard
                    |> View.withKey (sprintf "%A" model.LoadFileState.CurrentFile)
                )

                TabItem.onIsSelectedChanged (fun s -> if s then ViewChanged GridView |> dispatch)
            ]

        let listViewTab =
            TabItem.create [
                TabItem.header "Details"
                TabItem.isEnabled model.LoadFileState.Complete
                TabItem.isSelected (model.View = ListView || model.ChangeToView = Some ListView)

                TabItem.content (
                    ListView.viewComponent model.LoadFileState.Sprites model.SpriteSelected model.Clipboard
                    |> View.withKey (sprintf "%A" model.LoadFileState.CurrentFile)
                )

                TabItem.onIsSelectedChanged (fun s -> if s then ViewChanged ListView |> dispatch)
            ]

        View.createGeneric<ExperimentalAcrylicBorder> [
            ExperimentalAcrylicBorder.material (acrylicMaterial model.Colors.AcrylicColorOrDefault)

            ExperimentalAcrylicBorder.child (
                Panel.create [
                    Panel.children [
                        
                        TabControl.create [
                            TabControl.tabStripPlacement Dock.Bottom

                            TabControl.viewItems [
                                loadTab
                                gridViewTab
                                listViewTab
                            ]
                        ]
                        |> splitView model
                    ]
                ]
            )
        ]

#if DEBUG
[<AutoOpen>]
module DebugExtensions =
    type IRenderer with
        member this.Diagnostics = typeof<IRenderer>.GetProperty(nameof(this.Diagnostics)).GetMethod.Invoke(this, [||]) :?> RendererDiagnostics
    
    type RendererDiagnostics with
        member this.EnableOverlay overlay =
            this.DebugOverlays <- this.DebugOverlays ||| overlay
        member this.DisableOverlay overlay =
            this.DebugOverlays <- this.DebugOverlays &&& (~~~overlay)
#endif

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
        let tv =
            match this.PlatformSettings.GetColorValues().ThemeVariant with
            | Platform.PlatformThemeVariant.Dark -> Styling.ThemeVariant.Dark
            | Platform.PlatformThemeVariant.Light -> Styling.ThemeVariant.Light
            | _ -> Styling.ThemeVariant.Dark

        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- tv
        this.Styles.Load "avares://Avalonia.Controls.DataGrid/Themes/Fluent.xaml"
        
    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let mainWindow = MainWindow()
            desktopLifetime.MainWindow <- mainWindow
        | _ -> ()

module Program =

    [<System.STAThread>]
    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
