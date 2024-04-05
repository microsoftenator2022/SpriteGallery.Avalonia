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
            LoadFileState : LoadFileView.Model
            Colors : WindowColors
            SpritesUpdated : Event<SpritesData option>
            SpriteSelected : Event<Sprite option>
            ChangeToView : View option
        }

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
            printfn "%A" msg

            { model with View = view; ChangeToView = None }, Cmd.none

        | ChangeView view ->
            printfn "%A" msg

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
            // model, cmd

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
                        SpriteDetailsPanel.viewComponent model.SpriteSelected.Publish
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
                TabItem.header "Grid"
                TabItem.isEnabled model.LoadFileState.Complete
                TabItem.isSelected (model.View = GridView || model.ChangeToView = Some GridView)

                TabItem.content (
                    GridView.viewComponent
                        model.LoadFileState.Sprites
                        model.Colors.HighlightBrushOrDefault
                        model.SpriteSelected.Trigger
                        // model.SpritesUpdated.Publish
                        model.Window
                    |> View.withKey (sprintf "%A" model.LoadFileState.CurrentFile)
                )

                TabItem.onIsSelectedChanged (fun s -> if s then ViewChanged GridView |> dispatch)
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
                            ]
                        ]
                        |> splitView model
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

    [<System.STAThread>]
    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
