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

open DotNet.Globbing

open SpriteGallery.Avalonia
open SpriteGallery.Avalonia.Common

type DataGrid with
    static member selectionMode<'t when 't :> DataGrid>(value : DataGridSelectionMode) : IAttr<'t> =
        AttrBuilder.CreateProperty<DataGridSelectionMode>(DataGrid.SelectionModeProperty, value, ValueNone)
    static member clipboardCopyMode<'t when 't :> DataGrid>(value : DataGridClipboardCopyMode) : IAttr<'t> =
        AttrBuilder.CreateProperty<DataGridClipboardCopyMode>(DataGrid.ClipboardCopyModeProperty, value, ValueNone)

let filterSprites (filterMap : Sprite -> string) filterString sprites =
    if filterString = "" then sprites
    else
        let go = GlobOptions.Default
        go.Evaluation.CaseInsensitive <- true
        
        let glob = Glob.Parse($"*{filterString}*", go)

        sprites |> Seq.where (fun s -> filterMap s |> glob.IsMatch)

type Model =
  { Sprites : Sprite[]
    Selected : Sprite option
    Refresh : int
    OnSpriteSelected : Sprite option -> unit
    Clipboard : IClipboard
    FilterString : string
    FilterMap : (Sprite -> string) }

let init (sprites : SpritesData option) onSpriteSelected clipboard =
    {
        Sprites = sprites |> Option.toList |> Seq.collect _.Sprites |> Seq.toArray
        Selected = None
        Refresh = 0
        OnSpriteSelected = onSpriteSelected
        Clipboard = clipboard
        FilterString = ""
        FilterMap = _.Name
    }

type Msg =
| Unit
| Refresh
| SelectedSpriteChanged of Sprite option
| CopyText of string
| CopySprite of Sprite
| UpdateFilterText of string
| UpdateFilterMap of (Sprite -> string)

let update msg model =
    match msg with
    | Unit -> model, Cmd.none
    | Refresh -> { model with Refresh = model.Refresh + 1 }, Cmd.none
    | SelectedSpriteChanged sprite ->
        model.OnSpriteSelected sprite

        { model with Selected = sprite }, Cmd.none
    | CopySprite sprite ->
        model,
        Cmd.OfAsync.attempt
            copySpriteToClipboard
            (model.Clipboard, sprite)
            (fun exn -> eprintfn "%A" exn; Unit)
    | CopyText text ->
        model,
        Cmd.OfAsync.attempt
            copyTextToClipboard
            (model.Clipboard, text)
            (fun exn -> eprintfn "%A" exn; Unit)
    | UpdateFilterText filterString ->
        { model with FilterString = filterString }, Cmd.ofMsg Refresh
    | UpdateFilterMap map ->
        { model with FilterMap = map }, Cmd.ofMsg Refresh

let tileSize = 64
let view (model : Model) dispatch =
    DockPanel.create [
        DockPanel.lastChildFill true

        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Top

                StackPanel.orientation Orientation.Horizontal

                StackPanel.children [
                    TextBlock.create [
                        TextBlock.margin 4
                        TextBlock.verticalAlignment VerticalAlignment.Center

                        TextBlock.text "Filter:"
                    ]
                    TextBox.create [
                        TextBox.margin 4
                        TextBox.width 200
                        TextBox.verticalAlignment VerticalAlignment.Center

                        TextBox.onTextChanged (fun text -> UpdateFilterText text |> dispatch)
                    ]
                    RadioButton.create [
                        RadioButton.margin 4
                        RadioButton.verticalAlignment VerticalAlignment.Center
                        
                        RadioButton.groupName "Filter"

                        RadioButton.content "Name"
                        RadioButton.onChecked (fun _ -> UpdateFilterMap _.Name |> dispatch)

                        RadioButton.isChecked true
                    ]
                    RadioButton.create [
                        RadioButton.margin 4
                        RadioButton.verticalAlignment VerticalAlignment.Center
                        
                        RadioButton.groupName "Filter"

                        RadioButton.content "Container"
                        RadioButton.onChecked (fun _ -> UpdateFilterMap _.Container |> dispatch)
                    ]
                    RadioButton.create [
                        RadioButton.margin 4
                        RadioButton.verticalAlignment VerticalAlignment.Center
                        
                        RadioButton.groupName "Filter"

                        RadioButton.content "Reference AssetId"
                        RadioButton.onChecked (fun _ -> UpdateFilterMap (_.BlueprintReference >> Option.map fst >> Option.defaultValue "") |> dispatch)
                    ]
                ]
            ]
            DataGrid.create [
                DataGrid.horizontalAlignment HorizontalAlignment.Stretch

                DataGrid.isReadOnly true
                DataGrid.selectionMode DataGridSelectionMode.Single
                DataGrid.clipboardCopyMode DataGridClipboardCopyMode.None

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
                |> filterSprites model.FilterMap model.FilterString
                |> Seq.toArray
                |> DataGrid.items

                DataGrid.onSelectedItemChanged (fun item -> 
                    match item with
                    | :? Sprite as s -> Some s
                    | _ -> None
                    |> SelectedSpriteChanged
                    |> dispatch
                )
                
                DataGrid.onKeyDown (fun args ->
                    if args.KeyModifiers = Avalonia.Input.KeyModifiers.Control && args.Key = Avalonia.Input.Key.C then
                        args.Source 
                        |> function
                        | :? DataGrid as grid ->
                            if grid.CurrentColumn |> isNull |> not && grid.SelectedItem |> isNull |> not then
                                (grid.Columns[grid.CurrentColumn.DisplayIndex].GetCellContent(dataItem = grid.SelectedItem), grid.SelectedItem)
                                |> function
                                | (:? TextBlock as tb), _ -> CopyText tb.Text
                                | _, (:? Sprite as sprite) -> CopySprite sprite
                                | _ -> Unit
                                |> dispatch
                                
                                args.Handled <- true
                        | _ -> ()
                )
            ]
        ]
    ]

let viewComponent (spritesData : SpritesData option) (spriteSelectedEvent : Event<Sprite option>) clipboard =
    Component.create ("sprites-table", fun ctx ->
        let model, dispatch =
            ctx.useElmish((
                fun (sprites, onSpriteSelected, clipboard) -> init sprites onSpriteSelected clipboard, Cmd.ofMsg Refresh),
                update,
                (spritesData, spriteSelectedEvent.Trigger, clipboard)
            )

        view model dispatch
    )
