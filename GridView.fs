[<RequireQualifiedAccess>]
module SpriteGallery.Avalonia.Views.GridView

open Elmish

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Elmish.ElmishHook

open Avalonia
open Avalonia.Controls
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree

open SpriteGallery.Avalonia
open SpriteGallery.Avalonia.Common
open SpriteGallery.Avalonia.Controls.SpritesGrid

type Model =
  { Sprites : SpritesData option
    Refresh : int
    HighlightBrush : IBrush
    SpriteSelected : Sprite option -> unit
    Window : Window }

let init spritesData highlightBrush spriteSelected window =
    {
        Sprites = spritesData
        Refresh = 0
        HighlightBrush = highlightBrush
        SpriteSelected = spriteSelected
        Window = window
    }

type Msg =
| Unit
| UpdateSprites of SpritesData option
| Refresh
| SelectedSpriteChanged of Sprite option
| CopySprite of Sprite

let copySpriteToClipboard (clipboard : IClipboard, sprite) =
    let source = System.Span(sprite.BaseTexture.Bytes)
    let stride = sprite.BaseTexture.Bitmap.Force().PixelSize.Width * 4
    let rect = sprite.Rect
    let dest = Array.zeroCreate<byte> (rect.Width * rect.Height * 4)
    copyRect source stride rect (System.Span(dest))
    
    use bitmap = createBitmap dest sprite.Rect.Size
    let ms = new System.IO.MemoryStream()
    
    bitmap.Save(ms)
    
    let pngBytes = ms.ToArray()

    let dataObject = Avalonia.Input.DataObject()
    
    dataObject.Set("image/png", pngBytes)
    
    Avalonia.Threading.Dispatcher.UIThread.InvokeAsync (fun () -> clipboard.SetDataObjectAsync(dataObject))
    |> Async.AwaitTask

let update msg (model : Model) =
    match msg with
    | Unit -> model, Cmd.none
    | UpdateSprites sprites ->
        // printfn "%A" msg
        { model with Sprites = sprites }, Cmd.ofMsg Refresh
    | Refresh -> { model with Refresh = model.Refresh + 1 }, Cmd.ofMsg Unit
    | SelectedSpriteChanged s ->
        model.SpriteSelected s
        model, Cmd.none
    | CopySprite sprite ->
        model,
        Cmd.OfAsync.attempt
            copySpriteToClipboard
            (TopLevel.GetTopLevel(model.Window).Clipboard, sprite)
            (fun exn -> eprintfn "%A" exn; Unit)

let view model dispatch =
    ScrollViewer.create [
        ScrollViewer.bringIntoViewOnFocusChange false

        SpritesGrid.create [
            SpritesGrid.horizontalAlignment HorizontalAlignment.Left
            SpritesGrid.verticalAlignment VerticalAlignment.Top
            
            SpritesGrid.sprites
                (match model.Sprites with
                | Some sprites -> sprites.Sprites |> List.toArray
                | None -> Array.empty)

            SpritesGrid.highlightBrush model.HighlightBrush

            SpritesGrid.onSelectedSpriteChanged ((fun args -> args.Sprite |> SelectedSpriteChanged |> dispatch), SubPatchOptions.Always)
            SpritesGrid.onCopySprite ((fun args -> args.Sprite |> Option.iter (CopySprite >> dispatch)), SubPatchOptions.Always)
               
        ]
        |> View.withKey (model.Refresh.ToString())
        |> ScrollViewer.content
    ]

// let private subscriptions (updateSprites : System.IObservable<SpritesData option>) (_ : Model) : Sub<Msg> =
//     let updateSpritesSub dispatch =
//         updateSprites.Subscribe(fun s -> printfn "%A sprites" (s |> Option.map (fun s -> s.Sprites.Length));  s |> (UpdateSprites >> dispatch))

//     [
//         [nameof updateSpritesSub], updateSpritesSub
//     ]

let viewComponent spritesData highlightBrush onSpriteSelected window =
    Component.create ("grid-component", fun ctx ->
        let model, dispatch = ctx.useElmish(
            (fun (spritesData, highlightBrush, onSpriteSelected, window) -> init spritesData highlightBrush onSpriteSelected window, Cmd.none),
            update,
            (spritesData, highlightBrush, onSpriteSelected, window)
            // , Program.withSubscription (subscriptions updateSprites)
        )

        view model dispatch
    )
