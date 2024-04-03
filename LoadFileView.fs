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

        AssetLoader.cleanup()

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

type State =
  { Path : string
    Sprites : SpritesData option
    Progress : Progress option
    Window : Window
    StatusMessage : string
    Complete : bool }
with
    member this.InProgress =
        match this.Progress with
        | Some p -> p.IsComplete
        | None -> false

let init window =
    {
        Path = ""
        Sprites = None
        Progress = None
        Window = window
        StatusMessage = ""
        Complete = false
    }

type Msg =
| Unit
| SelectFile
| UpdatePathText of string
| StartLoad
| ProgressUpdate of Progress * LoadContext
| StatusMessage of string
| Complete

let loadStart state =
    let dir = System.IO.Path.GetDirectoryName state.Path
    let baPath = System.IO.Path.Join(dir, "blueprint.assets")

    let useBlueprintAssets = System.IO.File.Exists baPath
    let loadDependencies = System.IO.Path.Join(dir, "dependencylist.json") |> System.IO.File.Exists

    if System.IO.File.Exists state.Path |> not then
        { state with StatusMessage = sprintf "'%s' does not exist" state.Path }, Cmd.none
    // elif state.LoadBlueprintAssets && System.IO.File.Exists baPath |> not then
    //     { state with StatusMessage = sprintf "'%s' does not exist" baPath }, Cmd.none
    else
        state,
        [
            StatusMessage "Loading sprites"
            |> Cmd.ofMsg

            (
                // let context = new LoadContext(state.Path, state.LoadDependencies, state.LoadBlueprintAssets)
                let context = new LoadContext(state.Path, loadDependencies, useBlueprintAssets)
                context.Start()

                ProgressUpdate ({ Current = 0; Total = 1; SpritesData = SpritesData.init() }, context)
                |> Cmd.ofMsg
            )
        ] |> Cmd.batch

let update msg state =
    match msg with
    | Unit -> state, Cmd.none

    | SelectFile ->
        state,
        Cmd.OfAsyncImmediate.perform
            (state.Window.StorageProvider.OpenFilePickerAsync >> Async.AwaitTask)
            (Platform.Storage.FilePickerOpenOptions(AllowMultiple = false))
            (fun f -> f |> Seq.tryHead |> (function Some path -> path.Path.LocalPath | None -> "") |> UpdatePathText)

    | UpdatePathText path -> { state with Path = path }, Cmd.none

    | StartLoad -> loadStart state
    | ProgressUpdate (progress, context) ->
        let state = 
            { state with
                Progress = Some progress
                Sprites = if progress.IsComplete then Some progress.SpritesData else None
            }

        if not progress.IsComplete then
            state,
            Cmd.OfAsync.perform
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
        else
            (context :> System.IDisposable).Dispose()
            { state with Sprites = Some progress.SpritesData }, Cmd.ofMsg Complete
    | StatusMessage message ->
        printfn "%s" message
        { state with StatusMessage = message }, Cmd.none
    | Complete ->
        printfn "Complete"
        { state with Complete = true }, Cmd.none

let view state (dispatch : Dispatch<Msg>) =
    let suspecting = tryGetSuspectingIcon()

    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.horizontalAlignment HorizontalAlignment.Stretch
        StackPanel.verticalAlignment VerticalAlignment.Center
        StackPanel.margin 4

        StackPanel.children [
            match suspecting with
            | Some bitmap ->
                Image.create [
                    Image.source bitmap
                    Image.width bitmap.Size.Width
                    Image.height bitmap.Size.Height
                    Image.margin 4
                ]
            | None -> ()

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
                    ]
                    TextBox.create [
                        TextBox.horizontalAlignment HorizontalAlignment.Stretch
                        TextBox.margin 2
                        TextBox.text state.Path

                        TextBox.onTextChanged (UpdatePathText >> dispatch)
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
                    ]
                    ProgressBar.create [
                        ProgressBar.margin 2
                        ProgressBar.horizontalAlignment HorizontalAlignment.Stretch
                        let current, max =
                            match state.Progress with
                            | Some p -> p.Current, p.Total
                            | None -> 0, 1

                        ProgressBar.maximum max
                        ProgressBar.value current
                    ]
                ]
            ]
        ]
    ]
