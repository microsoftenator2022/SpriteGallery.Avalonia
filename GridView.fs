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
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree

open SpriteGallery.Avalonia
open SpriteGallery.Avalonia.Common
open SpriteGallery.Avalonia.Controls.SpritesGrid

type Model = { Sprites : SpritesData option; Refresh : int; HighlightBrush : IBrush; SelectedSprite : Sprite option }

let init highlightBrush = { Sprites = None; Refresh = 0; HighlightBrush = highlightBrush; SelectedSprite = None }

type Msg =
| Unit
| UpdateSprites of SpritesData option
| Refresh
| SelectedSpriteChanged of Sprite option

let update msg (model : Model) =
    match msg with
    | Unit -> model, Cmd.none
    | UpdateSprites sprites ->
        { model with Sprites = sprites }, Cmd.ofMsg Refresh
    | Refresh -> { model with Refresh = model.Refresh + 1 }, Cmd.ofMsg Unit
    | SelectedSpriteChanged s -> { model with SelectedSprite = s }, Cmd.none
    

// let private subscriptions (updateSprites : IEvent<SpritesData option>) (_ : Model) : Sub<Msg> =
//     let updateSpritesSub dispatch =
//         printfn "Update"
//         updateSprites.Subscribe(UpdateSprites >> dispatch)

//     [
//         [nameof updateSpritesSub], updateSpritesSub
//     ]

let view model dispatch =
    ScrollViewer.create [
        SpritesGrid.create [
            SpritesGrid.horizontalAlignment HorizontalAlignment.Left
            SpritesGrid.verticalAlignment VerticalAlignment.Top
            
            SpritesGrid.sprites
                (match model.Sprites with
                | Some sprites -> sprites.Sprites |> List.toArray
                | None -> Array.empty)

            SpritesGrid.highlightBrush model.HighlightBrush

            SpritesGrid.onSelectedSpriteChanged ((fun args -> args.Sprite |> SelectedSpriteChanged |> dispatch), SubPatchOptions.Always)
        ]
        |> View.withKey (model.Refresh.ToString())
        |> ScrollViewer.content
    ]
