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

module AssetLoadService =
    type LoadContext =
      { SpriteKeys : (string * UnityDataTools.FileSystem.ObjectInfo) array
        Sprites : Map<string * int64, Sprite>;
        Textures : Map<string * int64, SpriteTexture> }

    type Message =
    | Noop
    | Init
    | Cleanup
    | LoadArchive of string
    | GetSprite of (string * UnityDataTools.FileSystem.ObjectInfo)

    let mutable private message : Message = Noop
    let mutable private thread : Thread option = None

    let dir (filePath : string) = System.IO.Path.GetDirectoryName filePath
    let baPath dir = System.IO.Path.Join(dir, "blueprint.assets")

    let mutable private messageSent = new EventWaitHandle(false, EventResetMode.AutoReset)
    let mutable private notifCompleted = new EventWaitHandle(false, EventResetMode.AutoReset)

    let mutable private result : Sprite option = None
    let tryGetResult() =
        let sprite = result
        result <- None
        sprite

    let mutable private exited = false
    let mutable status = { SpriteKeys = Array.empty; Sprites = Map.empty; Textures = Map.empty }
    let initData keys = { SpriteKeys = keys; Sprites = Map.empty; Textures = Map.empty }

    // let mutable private spriteObjectInfos = None
    let private loadArchive filePath blueprintReferencedAssetsPath =
        let archive = AssetLoader.mountArchiveWithDependencies filePath |> fst
        
        AssetLoader.mountArchive blueprintReferencedAssetsPath |> ignore
        
        AssetLoader.getSpriteObjectsInArchive archive |> initData

    let rec private handleNext() =
        let handleNext notif =
            if notif then notifCompleted.Set() |> ignore
            handleNext()
        
        messageSent.WaitOne() |> ignore
        let m = message
        
        match m with
        | Noop -> handleNext false
        | Init ->
            AssetLoader.init()
            handleNext true
        | LoadArchive path ->
            status <- loadArchive path (path |> dir |> baPath)
            handleNext true
        | GetSprite (sfPath, o) ->
            let sprite =
                status.Sprites
                |> Map.tryFind (sfPath, o.Id)
                |> Option.orElseWith(fun () ->
                    let sf = AssetLoader.getSerializedFile sfPath |> toOption
                    let sprite = sf |> Option.bind (fun sf -> AssetLoader.getSprite o sf)

                    status <-
                        let sd = { status with Textures = AssetLoader.textures }

                        match sprite with
                        | Some sprite -> { sd with Sprites = sd.Sprites |> Map.add (sfPath, o.Id) sprite }
                        | None -> sd

                    sprite)

            result <- sprite
            handleNext true
            
        | Cleanup ->
            AssetLoader.cleanup()

    let private logException exn =
        exn |> sprintf "%A" |> log

    let private serviceProc() =
        messageSent.Reset() |> ignore
        notifCompleted.Reset() |> ignore

        try
            try
                exited <- false
                handleNext()
            with
            | e ->
                logException e
                reraise()

        finally
            exited <- true
            message <- Noop
            result <- None

            messageSent.Dispose()
            messageSent <- new EventWaitHandle(false, EventResetMode.AutoReset)

            notifCompleted.Dispose()
            notifCompleted <- new EventWaitHandle(false, EventResetMode.AutoReset)

            thread <- None

    let mutable private started = false
    let startThread() =
        let t = ThreadStart(serviceProc) |> Thread
        t.IsBackground <- true
        thread <- Some t

        t.Start()

        started <- true

    let running() = started && not exited

    let private sendMessage m =
        message <- m
        if messageSent.Set() |> not then failwith "Could not signal asset load service"

    let initAsync() = async {
        if not (running()) then
            startThread()

        Message.Init |> sendMessage

        return! Async.AwaitWaitHandle notifCompleted |> Async.Ignore
    }

    let loadArchiveAsync path = async {
        sprintf "load archive %s" path |> log
        Message.LoadArchive path |> sendMessage

        let! _ = Async.AwaitWaitHandle notifCompleted |> Async.Ignore

        return status
    }

    let tryGetSpriteAsync (sfPath, o) = async {
        GetSprite (sfPath, o) |> sendMessage

        let! _ = Async.AwaitWaitHandle notifCompleted |> Async.Ignore

        return tryGetResult()
    }

    let cleanup() =
        sendMessage Cleanup

type Progress = { Current : int; LoadStatus : AssetLoadService.LoadContext }
with
    member this.Total =
        let keyCount = this.LoadStatus.SpriteKeys.Length
        if keyCount = 0 then -1 else keyCount
    member this.IsComplete = this.Current = this.Total
    static member init() = { Current = 0; LoadStatus = AssetLoadService.initData [||] }

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
| ProgressUpdate of Progress
| StatusMessage of string
| Complete
| CancelLoad
| Error of string

let loadStart model =
    if System.IO.File.Exists model.Path |> not then
        { model with StatusMessage = sprintf "'%s' does not exist" model.Path }, Cmd.none
    else
        { model with Progress = Progress.init() |> Some },
        [
            Cmd.ofMsg (StatusMessage "Loading sprites")
            Cmd.OfAsync.perform
                (fun path -> async {
                    let! _ = AssetLoadService.initAsync()

                    let! spritesData = AssetLoadService.loadArchiveAsync path

                    return { Current = 0; LoadStatus = spritesData }
                })
                model.Path
                ProgressUpdate
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
    | StartLoad -> loadStart { model with CurrentFile = None }
    | CancelLoad -> { model with Progress = None }, Cmd.none
    | Error errorMessage -> model, Cmd.batch [Cmd.ofMsg CancelLoad; Cmd.ofMsg (StatusMessage errorMessage)]
    | ProgressUpdate progress ->
        let progress = { progress with LoadStatus = AssetLoadService.status }
        let model = 
            { model with
                Progress = Some progress
                Sprites =
                    Some {
                        Textures = progress.LoadStatus.Textures
                        Sprites = progress.LoadStatus.Sprites.Values
                    }
            }

        if not progress.IsComplete then
            model,
            Cmd.OfAsync.either
                (fun key -> async {
                    match! AssetLoadService.tryGetSpriteAsync key with
                    | Some sprite ->
                        sprintf "Got sprite %s" sprite.Name |> log
                        return { progress with Current = progress.Current + 1; }
                    | None ->
                        return
                            sprintf "Could not get sprite %A" progress.LoadStatus.SpriteKeys[progress.Current]
                            |> failwith
                })
                progress.LoadStatus.SpriteKeys[progress.Current]
                ProgressUpdate
                (fun exn -> Error exn.Message)
            else
                { model with
                    Sprites =
                        Some {
                            Sprites = progress.LoadStatus.Sprites |> Map.values
                            Textures = progress.LoadStatus.Textures
                        }
                }, Cmd.ofMsg Complete

    | StatusMessage message ->
        printfn "%s" message
        { model with StatusMessage = message }, Cmd.none
    | Complete ->
        let model = { model with Complete = true; CurrentFile = model.Path |> Some; StatusMessage = "Loading complete" }
        AssetLoadService.cleanup()
        model, Cmd.none

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
