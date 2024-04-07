[<RequireQualifiedAccess>]
module SpriteGallery.Avalonia.Views.ListView

open Elmish

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Elmish.ElmishHook

open Avalonia
open Avalonia.Controls
open Avalonia.Data
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree

open SpriteGallery.Avalonia
open SpriteGallery.Avalonia.Common

type DataGrid with
    static member selectionMode<'t when 't :> DataGrid>(value : DataGridSelectionMode) : IAttr<'t> =
        AttrBuilder.CreateProperty<DataGridSelectionMode>(DataGrid.SelectionModeProperty, value, ValueNone)
    static member clipboardCopyMode<'t when 't :> DataGrid>(value : DataGridClipboardCopyMode) : IAttr<'t> =
        AttrBuilder.CreateProperty<DataGridClipboardCopyMode>(DataGrid.ClipboardCopyModeProperty, value, ValueNone)

type Model =
  { Sprites : Sprite[]
    Selected : Sprite option
    Refresh : int
    OnSpriteSelected : Sprite option -> unit }

let init (sprites : SpritesData option) onSpriteSelected =
    {
        Sprites = sprites |> Option.toList |> Seq.collect _.Sprites |> Seq.toArray
        Selected = None
        Refresh = 0
        OnSpriteSelected = onSpriteSelected
    }

type Msg =
| Unit
| Refresh

let update msg model =
    match msg with
    | Unit -> model, Cmd.none
    | Refresh -> { model with Refresh = model.Refresh + 1 }, Cmd.none

let tileSize = 64
let view (model : Model) dispatch =
    DataGrid.create [
        DataGrid.isReadOnly true

        DataGrid.columns [
            DataGridTemplateColumn.create [
                DataGridTemplateColumn.width DataGridLength.SizeToCells
                DataGridTemplateColumn.cellTemplate (
                    DataTemplateView<_>.create (fun (data : Sprite) ->
                        Image.create [
                            Image.width tileSize
                            Image.height tileSize
                            Image.stretch Stretch.Uniform
                            Image.margin 2

                            data.GetHeightScaledBitmap(tileSize)
                            |> Image.source
                        ]
                    )
                )
            ]
            DataGridTextColumn.create [
                DataGridTextColumn.header "Name"
                DataGridTextColumn.binding (Binding (nameof(Unchecked.defaultof<Sprite>.Name), BindingMode.OneWay))
                DataGridTextColumn.width (DataGridLength.Auto)
            ]
            DataGridTextColumn.create [
                DataGridTextColumn.header "Container"
                DataGridTextColumn.binding (Binding (nameof(Unchecked.defaultof<Sprite>.Container), BindingMode.OneWay))
                DataGridTextColumn.width (DataGridLength.Auto)
            ]
            DataGridTextColumn.create [
                DataGridTextColumn.header "AssetId"
                DataGridTextColumn.binding (Binding (nameof(Unchecked.defaultof<Sprite>.ReferenceAssetId), BindingMode.OneWay))
                DataGridTextColumn.width (DataGridLength.Auto)
            ]
            DataGridTextColumn.create [
                DataGridTextColumn.header "FileId"
                DataGridTextColumn.binding (Binding (nameof(Unchecked.defaultof<Sprite>.ReferenceFileId), BindingMode.OneWay))
                DataGridTextColumn.width (DataGridLength.Auto)
            ]
        ]

        model.Sprites
        |> DataGrid.items

        DataGrid.selectionMode DataGridSelectionMode.Single
        DataGrid.clipboardCopyMode DataGridClipboardCopyMode.None

        DataGrid.onSelectedItemChanged (fun item -> 
            match item with
            | :? Sprite as s -> model.OnSpriteSelected (Some s)
            | _ -> model.OnSpriteSelected None
        )
        
        DataGrid.onKeyDown (fun args ->
            if args.KeyModifiers = Avalonia.Input.KeyModifiers.Control && args.Key = Avalonia.Input.Key.C then
                args.Source 
                |> function
                | :? DataGrid as grid ->
                    if grid.CurrentColumn |> isNull |> not && grid.SelectedItem |> isNull |> not then
                        grid.Columns[grid.CurrentColumn.DisplayIndex].GetCellContent(dataItem = grid.SelectedItem)
                        |> function
                        | :? TextBlock as tb -> Some tb.Text
                        | _ -> None
                        |> printfn "%A"

                        args.Handled <- true
                | _ -> ()
        )
    ]

let viewComponent (spritesData : SpritesData option) (spriteSelectedEvent : Event<Sprite option>) =
    Component.create ("sprites-table", fun ctx ->
        let model, dispatch =
            ctx.useElmish((
                fun (sprites, onSpriteSelected) -> init sprites onSpriteSelected, Cmd.ofMsg Refresh),
                update,
                (spritesData, spriteSelectedEvent.Trigger)
            )

        view model dispatch
    )
