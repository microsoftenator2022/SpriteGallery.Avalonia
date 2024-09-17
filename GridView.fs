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

[<RequireQualifiedAccess>]
type SpritesSort =
| None
| PathID
| Name
| FileId
with
    member this.sort ss =
        ss
        |>
        match this with
        | None -> id
        | PathID -> Seq.sortBy _.PathID
        | Name -> Seq.sortBy _.Name
        | FileId -> Seq.sortBy (_.BlueprintReference >> function Some (_, fileId) -> fileId | _ -> System.Int64.MaxValue)

type Model =
  { Sprites : SpritesData option
    Refresh : int
    HighlightBrush : IBrush
    OnSpriteSelected : Sprite option -> unit
    Clipboard : IClipboard
    SortBy : SpritesSort }

let init spritesData highlightBrush spriteSelected clipboard =
    {
        Sprites = spritesData
        Refresh = 0
        HighlightBrush = highlightBrush
        OnSpriteSelected = spriteSelected
        Clipboard = clipboard
        SortBy = SpritesSort.Name
    }

type Msg =
| Unit
| UpdateSprites of SpritesData option
| Refresh
| SelectedSpriteChanged of Sprite option
| CopySprite of Sprite
| SetSortMode of SpritesSort

let update msg (model : Model) =
    match msg with
    | Unit -> model, Cmd.none
    | UpdateSprites sprites ->
        // printfn "%A" msg
        { model with Sprites = sprites }, Cmd.ofMsg Refresh
    | Refresh -> { model with Refresh = model.Refresh + 1 }, Cmd.ofMsg Unit
    | SelectedSpriteChanged s ->
        model.OnSpriteSelected s
        model, Cmd.none
    | CopySprite sprite ->
        model,
        Cmd.OfAsync.attempt
            copySpriteToClipboard
            (model.Clipboard, sprite)
            (fun exn -> eprintfn "%A" exn; Unit)
    | SetSortMode sort ->
        { model with SortBy = sort }, Cmd.ofMsg Refresh

let view model dispatch =
    ScrollViewer.create [
        ScrollViewer.bringIntoViewOnFocusChange false

        SpritesGrid.create [
            SpritesGrid.horizontalAlignment HorizontalAlignment.Left
            SpritesGrid.verticalAlignment VerticalAlignment.Top
                    
            SpritesGrid.sprites
                (match model.Sprites with
                | Some sprites -> sprites.Sprites |> model.SortBy.sort |> Seq.toArray
                | None -> Array.empty)

            SpritesGrid.highlightBrush model.HighlightBrush

            SpritesGrid.onSelectedSpriteChanged ((fun args -> args.Sprite |> SelectedSpriteChanged |> dispatch), SubPatchOptions.Always)
            SpritesGrid.onCopySprite ((fun args -> args.Sprite |> Option.iter (CopySprite >> dispatch)), SubPatchOptions.Always)
        ]
        |> View.withKey (model.Refresh.ToString())
        |> ScrollViewer.content
    ]
    
let viewComponent spritesData highlightBrush (spriteSelectedEvent : Event<Sprite option>) clipboard =
    Component.create ("grid-component", fun ctx ->
        let model, dispatch = ctx.useElmish(
            (fun (spritesData, highlightBrush, onSpriteSelected, clipboard) -> init spritesData highlightBrush onSpriteSelected clipboard, Cmd.none),
            update,
            (spritesData, highlightBrush, spriteSelectedEvent.Trigger, clipboard))

        view model dispatch
    )
