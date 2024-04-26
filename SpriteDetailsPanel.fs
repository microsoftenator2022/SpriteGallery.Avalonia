[<RequireQualifiedAccess>]
module SpriteGallery.Avalonia.Views.SpriteDetailsPanel

open Avalonia.FuncUI.DSL

open Elmish

open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Elmish.ElmishHook

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage

open SpriteGallery.Avalonia.Common

type Model = { Sprite : Sprite option; Window : Window }

type Msg =
| Unit
| SpriteSelected of Sprite option
| SaveToFile

let init window = { Sprite = None; Window = window }

let update msg model =
    match msg with
    | Unit -> model, Cmd.none
    | SpriteSelected sprite ->
        { model with Sprite = sprite }, Cmd.none
    | SaveToFile ->
        match model.Sprite with
        | Some sprite ->
            model,
            Cmd.OfAsyncImmediate.perform
                (model.Window.StorageProvider.SaveFilePickerAsync >> Async.AwaitTask)
                (FilePickerSaveOptions(
                    DefaultExtension = "png",
                    FileTypeChoices = [FilePickerFileTypes.ImagePng],
                    SuggestedFileName = sprite.Name))
                (fun path ->
                    match path |> Option.ofObj with
                    | Some f ->
                        sprite |> saveSprite f.Path.LocalPath
                        Unit
                    | None -> Unit
                )
        | None -> model, Cmd.none

let margin = 4.0

let labelTextBlockStyle = [
    TextBlock.margin margin
    TextBlock.verticalAlignment VerticalAlignment.Center
    TextBlock.horizontalAlignment HorizontalAlignment.Left
]

let valueTextBoxStyle = [
    TextBox.isReadOnly true
    TextBox.margin margin
    TextBox.verticalAlignment VerticalAlignment.Center
    TextBox.horizontalAlignment HorizontalAlignment.Stretch
]

let view (model : Model) dispatch =
    DockPanel.create [
        DockPanel.margin 12
        DockPanel.lastChildFill true
        
        DockPanel.children [
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.dock Dock.Bottom

                StackPanel.children [
                    Grid.create [
                        Grid.horizontalAlignment HorizontalAlignment.Stretch

                        [ColumnWidth.Auto; ColumnWidth.Star 1; ColumnWidth.Auto; ColumnWidth.Auto]
                        |> List.map ColumnDefinition.create
                        |> Grid.columnDefinitions

                        List.init 2 (fun _ -> RowDefinition.create RowHeight.Auto)
                        |> Grid.rowDefinitions

                        Grid.children [
                            Button.create [
                                Button.row 0
                                //Button.rowSpan 2
                                Button.column 0

                                Button.content "Save to file"

                                Button.isEnabled model.Sprite.IsSome

                                Button.onClick (fun _ -> SaveToFile |> dispatch)
                            ]

                            TextBlock.create [
                                yield! labelTextBlockStyle
                                
                                TextBlock.row 0
                                TextBlock.column 2

                                TextBlock.text "Width"
                            ]

                            TextBox.create [
                                yield! valueTextBoxStyle

                                TextBox.row 0
                                TextBox.column 3

                                TextBox.text (
                                    match model.Sprite with
                                    | Option.Some s -> s.Rect.Width.ToString()
                                    | None -> ""
                                )
                            ]

                            TextBlock.create [
                                yield! labelTextBlockStyle
                                
                                TextBlock.row 1
                                TextBlock.column 2

                                TextBlock.text "Height"
                            ]

                            TextBox.create [
                                yield! valueTextBoxStyle

                                TextBox.row 1
                                TextBox.column 3

                                TextBox.text (
                                    match model.Sprite with
                                    | Option.Some s -> s.Rect.Height.ToString()
                                    | None -> ""
                                )
                            ]
                        ]
                    ]

                    Grid.create [
                        Grid.horizontalAlignment HorizontalAlignment.Stretch

                        Grid.columnDefinitions [ColumnDefinition.create(ColumnWidth.Auto); ColumnDefinition.create(ColumnWidth.Star 1)]

                        List.init 6 (fun _ -> RowDefinition.create(RowHeight.Auto))
                        |> Grid.rowDefinitions

                        Grid.children [
                            TextBlock.create [
                                yield! labelTextBlockStyle

                                TextBlock.row 0
                                TextBlock.column 0

                                TextBlock.text "Name"
                            ]
                            TextBox.create [
                                yield! valueTextBoxStyle

                                TextBox.row 0
                                TextBox.column 1

                                TextBox.text (
                                    model.Sprite |> Option.map _.Name |> Option.defaultValue ""
                                )
                            ]

                            TextBlock.create [
                                yield! labelTextBlockStyle

                                TextBlock.row 1
                                TextBlock.column 0

                                TextBlock.text "Container"
                            ]
                            TextBox.create [
                                yield! valueTextBoxStyle

                                TextBox.row 1
                                TextBox.column 1

                                TextBox.text (
                                    match model.Sprite with
                                    | Some s -> s.Container
                                    | None -> ""
                                )
                            ]

                            TextBlock.create [
                                yield! labelTextBlockStyle

                                TextBlock.row 2
                                TextBlock.column 0

                                TextBlock.text "PathID"
                            ]
                            TextBox.create [
                                yield! valueTextBoxStyle

                                TextBox.row 2
                                TextBox.column 1

                                TextBox.text (
                                    match model.Sprite with
                                    | Some s -> s.PathID.ToString()
                                    | None -> ""
                                )
                            ]

                            TextBlock.create [
                                yield! labelTextBlockStyle

                                TextBlock.row 3
                                TextBlock.column 0
                                TextBlock.columnSpan 2

                                TextBlock.text "Blueprint Reference"
                            ]

                            TextBlock.create [
                                yield! labelTextBlockStyle

                                TextBlock.row 4
                                TextBlock.column 0

                                TextBlock.text "AssetId"
                            ]
                            TextBox.create [
                                yield! valueTextBoxStyle

                                TextBox.row 4
                                TextBox.column 1

                                TextBox.text (
                                    model.Sprite
                                    |> Option.bind (fun s -> s.BlueprintReference)
                                    |> Option.map fst
                                    |> Option.defaultValue ""
                                )
                            ]

                            TextBlock.create [
                                yield! labelTextBlockStyle

                                TextBlock.row 5
                                TextBlock.column 0

                                TextBlock.text "FileId"
                            ]
                            TextBox.create [
                                yield! valueTextBoxStyle

                                TextBox.row 5
                                TextBox.column 1

                                TextBox.text (
                                    model.Sprite
                                    |> Option.bind (fun s -> s.BlueprintReference)
                                    |> Option.map (snd >> string)
                                    |> Option.defaultValue ""
                                )
                            ]
                        ]

                    ]
                ]
            ]
        
            Panel.create [
                Panel.horizontalAlignment HorizontalAlignment.Stretch
                Panel.verticalAlignment VerticalAlignment.Stretch

                Panel.children [
                    match model.Sprite with
                    | Some sprite ->
                        yield
                            Viewbox.create [
                                Viewbox.stretch Stretch.Uniform
                                Viewbox.stretchDirection StretchDirection.DownOnly
                                Viewbox.horizontalAlignment HorizontalAlignment.Center
                                Viewbox.verticalAlignment VerticalAlignment.Center

                                Viewbox.child
                                    (Image.create [
                                        // TODO: cache this?
                                        new CroppedBitmap(sprite.BaseTexture.Bitmap.Force(), sprite.Rect)
                                        |> Image.source
                                        
                                        ScaleTransform(1, -1)
                                        |> Image.renderTransform

                                        Image.width sprite.Rect.Width
                                        Image.height sprite.Rect.Height
                                    ])
                            ]
                    | _ -> ()
                ]
            ]
        ]
    ]

let private subscriptions (spriteSelected : System.IObservable<Sprite option>) model : Sub<Msg> =
    let spriteSelectedSub dispatch =
        spriteSelected.Subscribe(SpriteSelected >> dispatch)
    [
        [nameof spriteSelectedSub], spriteSelectedSub
    ]

let viewComponent spriteSelected window = Component.create ("sprite-details", fun ctx ->
    let model, dispatch = ctx.useElmish((fun window -> init window, Cmd.none), update, window, Program.withSubscription (subscriptions spriteSelected))

    view model dispatch
)
