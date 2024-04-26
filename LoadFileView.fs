[<RequireQualifiedAccess>]
module SpriteGallery.Avalonia.Views.LoadFileView

open Elmish

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Elmish

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree

open SpriteGallery.Avalonia

open System.Threading

open MicroUtils.Interop

open SpriteGallery.Avalonia.Common

// TODO improvments:
// 1. GetOneAsync() -> GetSpriteAsync(pathId) (or PPtr?)
// 2. If opening blueprint.assets, read BlueprintReferencedAssets sprites from *all* referenced bundles
// 3. Filter objects with random access reader instead of reading entire objects
type LoadContext (filePath : string, loadDependencies, loadBlueprintAssets) =
    let dir = System.IO.Path.GetDirectoryName filePath
    let baPath = System.IO.Path.Join(dir, "blueprint.assets")

    let mutable started = false
    let mutable spriteCount = 0
    let mutable complete = false
    let mutable spritesData = { Sprites = []; Textures = Map.empty }

    let newWaitHandle() = new EventWaitHandle(false, EventResetMode.AutoReset)
    let mutable waitHandle = lazy(newWaitHandle())
    let spriteUpdate = Event<Sprite option>()

    let loadProc() =
        started <- true

        AssetLoader.init()
        try
            try
                let archive =
                    if loadDependencies then
                        AssetLoader.mountArchiveWithDependencies filePath |> fst
                    else AssetLoader.mountArchive filePath

                if loadBlueprintAssets then    
                    AssetLoader.mountArchive baPath |> ignore
                
                let spriteObjectInfos = AssetLoader.getSpriteObjectsInArchive archive

                spriteCount <- spriteObjectInfos.Length

                for (sfPath, o) in spriteObjectInfos do
                    waitHandle.Force().WaitOne() |> ignore
                    
                    let sf = AssetLoader.getSerializedFile sfPath |> toOption
                    let sprite = sf |> Option.bind (fun sf -> AssetLoader.getSprite o sf)

                    spritesData <-
                        let spritesData = { spritesData with Textures = AssetLoader.textures }

                        match sprite with
                        | Some sprite -> { spritesData with Sprites = sprite :: spritesData.Sprites }
                        | None -> spritesData

                    spriteUpdate.Trigger sprite
            finally
                AssetLoader.cleanup()
        with
            e ->
                eprintfn "%A" e
                reraise()

        complete <- true

        spriteUpdate.Trigger None

    let thread =
        let thread = ThreadStart(loadProc) |> Thread
        thread.IsBackground <- true
        thread

    member _.Count = spriteCount
    member _.SpritesData = spritesData
    member _.Complete = complete

    [<CLIEvent>]
    member private _.spriteUpdateEvent = spriteUpdate.Publish

    member _.Start() =
        thread.Start()

    member this.GetOneAsync() = async {
        waitHandle.Force().Set() |> ignore

        return! this.spriteUpdateEvent |> Async.AwaitEvent
    }

    interface System.IDisposable with
        member _.Dispose() =
            if waitHandle.IsValueCreated then
                waitHandle.Value.Dispose()
                waitHandle <- lazy(newWaitHandle())

type Progress = { Current : int; Total : int; SpritesData : SpritesData }
with
    member this.IsComplete = this.Current = this.Total

type Model =
  { Path : string
    CurrentFile : string option
    Sprites : SpritesData option
    Progress : Progress option
    Window : Window
    StatusMessage : string
    Complete : bool }
with
    member this.InProgress =
        match this.Progress with
        | Some p -> not p.IsComplete
        | None -> false

let init window =
    {
        Path = ""
        CurrentFile = None
        Sprites = None
        Progress = None
        Window = window
        StatusMessage = ""
        Complete = false
    }

type Msg =
| Unit
| SelectFile
| UpdatePathText of string option
| StartLoad
| ProgressUpdate of Progress * LoadContext
| StatusMessage of string
| Complete

let loadStart model =
    let dir = System.IO.Path.GetDirectoryName model.Path
    let baPath = System.IO.Path.Join(dir, "blueprint.assets")

    let useBlueprintAssets = System.IO.File.Exists baPath
    let loadDependencies = System.IO.Path.Join(dir, "dependencylist.json") |> System.IO.File.Exists

    if System.IO.File.Exists model.Path |> not then
        { model with StatusMessage = sprintf "'%s' does not exist" model.Path }, Cmd.none
    // elif state.LoadBlueprintAssets && System.IO.File.Exists baPath |> not then
    //     { state with StatusMessage = sprintf "'%s' does not exist" baPath }, Cmd.none
    else
        model,
        [
            StatusMessage "Loading sprites"
            |> Cmd.ofMsg

            (
                // let context = new LoadContext(state.Path, state.LoadDependencies, state.LoadBlueprintAssets)
                let context = new LoadContext(model.Path, loadDependencies, useBlueprintAssets)
                context.Start()

                ProgressUpdate ({ Current = 0; Total = -1; SpritesData = SpritesData.init() }, context)
                |> Cmd.ofMsg
            )
        ] |> Cmd.batch

let update msg model =
    match msg with
    | Unit -> model, Cmd.none

    | SelectFile ->
        model,
        Cmd.OfAsyncImmediate.perform
            (model.Window.StorageProvider.OpenFilePickerAsync >> Async.AwaitTask)
            (Platform.Storage.FilePickerOpenOptions(AllowMultiple = false))
            (fun f -> f |> Seq.tryHead |> (Option.map (fun path -> path.Path.LocalPath)) |> UpdatePathText)

    | UpdatePathText path -> { model with Path = path |> Option.defaultValue model.Path }, Cmd.none

    | StartLoad ->
        loadStart { model with CurrentFile = None }
    | ProgressUpdate (progress, context) ->
        let state = 
            { model with
                Progress = Some progress
                Sprites = if progress.IsComplete then Some progress.SpritesData else None
            }

        if not progress.IsComplete then
            state,
            Cmd.OfAsync.either
                context.GetOneAsync
                ()
                (function
                | Some sprite ->
                    let progress =
                        { progress with
                            Current = progress.Current + 1
                            Total = context.Count
                            SpritesData = context.SpritesData }

                    ProgressUpdate (progress, context)
                | None -> Unit)
                (fun exn -> eprintfn "%A" exn; Unit)
        else
            (context :> System.IDisposable).Dispose()
            { state with Sprites = Some progress.SpritesData }, Cmd.ofMsg Complete
    | StatusMessage message ->
        printfn "%s" message
        { model with StatusMessage = message }, Cmd.none
    | Complete ->
        { model with Complete = true; CurrentFile = model.Path |> Some; StatusMessage = "Loading complete" }, Cmd.none

let view panelLength (model : Model) (dispatch : Dispatch<Msg>) =
    let suspecting = tryGetSuspectingIcon()

    DockPanel.create [
        DockPanel.lastChildFill true

        DockPanel.children [
            TextBlock.create [
                TextBlock.dock Dock.Top
                TextBlock.horizontalAlignment HorizontalAlignment.Right
                
                TextBlock.text (appVersionString.ToString())
            ]
            Panel.create [
                Panel.dock Dock.Left
                Panel.width (panelLength / 2.0)
            ]
            StackPanel.create [
                StackPanel.dock Dock.Right
                StackPanel.width panelLength
                StackPanel.verticalAlignment VerticalAlignment.Center
                StackPanel.horizontalAlignment HorizontalAlignment.Center
                
                StackPanel.orientation Orientation.Vertical

                StackPanel.children [
                    match suspecting with
                    | Some bitmap ->
                        Image.create [
                            Image.source bitmap
                            Image.width bitmap.Size.Width
                            Image.height bitmap.Size.Height
                            Image.margin 4
                            Image.verticalAlignment VerticalAlignment.Center
                            Image.horizontalAlignment HorizontalAlignment.Center
                        ]
                    | None -> ()

                    Grid.create [
                        Grid.verticalAlignment VerticalAlignment.Center
                        Grid.horizontalAlignment HorizontalAlignment.Center
                        Grid.margin 4
                        Grid.columnDefinitions (List.init 2 (fun _ -> ColumnDefinition.create ColumnWidth.Auto))
                        Grid.rowDefinitions (List.init 2 (fun _ -> RowDefinition.create RowHeight.Auto))

                        Grid.children [
                            TextBlock.create [
                                TextBlock.margin 4
                                TextBlock.column 0
                                TextBlock.row 0
                                
                                TextBlock.text "Arrows"
                            ]
                            TextBlock.create [
                                TextBlock.margin 4
                                TextBlock.column 1
                                TextBlock.row 0
                                
                                TextBlock.text "Move selection"
                            ]
                            TextBlock.create [
                                TextBlock.margin 4
                                TextBlock.column 0
                                TextBlock.row 1
                                
                                TextBlock.text "Ctrl+C"
                            ]
                            TextBlock.create [
                                TextBlock.margin 4
                                TextBlock.column 1
                                TextBlock.row 1
                                
                                TextBlock.text "Copy to clipboard"
                            ]
                        ]
                    ]
                ]
            ]
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.horizontalAlignment HorizontalAlignment.Stretch
                StackPanel.verticalAlignment VerticalAlignment.Center
                StackPanel.margin 4

                StackPanel.children [
                    DockPanel.create [
                        DockPanel.dock Dock.Top
                        DockPanel.lastChildFill true
                        DockPanel.margin 2

                        DockPanel.children [
                            Button.create [
                                Button.dock Dock.Right
                                Button.margin 2
                                Button.content "Select file..."
                                Button.onClick (fun _ -> SelectFile |> dispatch)

                                Button.isEnabled (not model.InProgress)
                            ]
                            TextBox.create [
                                TextBox.horizontalAlignment HorizontalAlignment.Stretch
                                TextBox.margin 2
                                TextBox.text model.Path

                                TextBox.onTextChanged (Some >> UpdatePathText >> dispatch)
                                TextBox.isEnabled (not model.InProgress)
                            ]
                        ]
                    ]
                    DockPanel.create [
                        DockPanel.dock Dock.Bottom
                        DockPanel.margin 2
                        DockPanel.lastChildFill true

                        DockPanel.children [
                            Button.create [
                                Button.dock Dock.Right
                                Button.margin 2
                                Button.content "Load sprites"

                                Button.onClick (fun _ -> StartLoad |> dispatch)
                                Button.isEnabled (not model.InProgress)
                            ]
                            ProgressBar.create [
                                ProgressBar.margin 2
                                ProgressBar.horizontalAlignment HorizontalAlignment.Stretch
                                
                                match model.Progress with
                                | Some p when p.Total <= 0 ->
                                    ProgressBar.isIndeterminate true
                                | Some p ->
                                    ProgressBar.maximum p.Total
                                    ProgressBar.value p.Current
                                | None ->
                                    ProgressBar.maximum 1
                                    ProgressBar.value 0
                            ]
                        ]
                    ]
                    TextBlock.create [
                        TextBlock.horizontalAlignment HorizontalAlignment.Center

                        TextBlock.text model.StatusMessage
                    ]
                ]
            ]
        ]
    ]
