namespace SpriteGallery.Avalonia.Views

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
open SpriteGallery.Avalonia.Common
open SpriteGallery.Avalonia.Controls.SpritesGrid

module GridView =
    type State = { Sprites : SpritesData option; Refresh : int; HighlightBrush : IBrush }

    let init highlightBrush = { Sprites = None; Refresh = 0; HighlightBrush = highlightBrush }

    type Msg =
    | Unit
    | UpdateSprites of SpritesData option
    | Refresh

    let update msg (state : State) =
        match msg with
        | Unit -> state, Cmd.none
        | UpdateSprites sprites ->
            { state with Sprites = sprites }, Cmd.ofMsg Refresh
        | Refresh -> { state with Refresh = state.Refresh + 1 }, Cmd.ofMsg Unit

    let view (state : State) (dispatch : Dispatch<Msg>) =
        ScrollViewer.create [
            SpritesGrid.create [
                SpritesGrid.horizontalAlignment HorizontalAlignment.Left
                SpritesGrid.verticalAlignment VerticalAlignment.Top
                
                SpritesGrid.sprites
                    (match state.Sprites with
                    | Some sprites -> sprites.Sprites |> List.toArray
                    | None -> Array.empty)

                SpritesGrid.highlightBrush state.HighlightBrush
            ]
            |> View.withKey (state.Refresh.ToString())
            |> ScrollViewer.content
        ]
