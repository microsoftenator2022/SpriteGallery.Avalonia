module SpriteGallery.Avalonia.Grid

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree

open Common

type Sprite = { Background : Color }

type SpritesGrid () as this =
    inherit Control()

    let calculateColumnCount tileSize margin width =
        let innerSize = (width - (margin * 2))
        innerSize / (tileSize + margin) |> max 1
    
    let calculateGrid tileSize margin tileCount (size : Size) =
        let columns = calculateColumnCount tileSize margin (size.Width |> int)
        let rows = (tileCount |> float) / (columns |> float) |> ceil |> int

        (columns, rows)

    // let calculateHeight tileSize margin tileCount columns =
    //     let rows = (tileCount / columns) + 1
    //     (margin * (rows + 1)) + (rows * tileSize)

    let calculateSize tileSize margin tileCount (size : Size) =
        let columns, rows = calculateGrid tileSize margin tileCount size
        let dimension tiles = (tiles * tileSize) + (margin * (tiles + 1))

        Size (dimension columns, dimension rows)

    let mutable effectiveViewPort = Rect()

    let mutable visualParent = None

    let isInParentBounds (point : Point) =
        match visualParent with
        | None -> visualParent <- this.GetVisualParent() |> Option.ofObj
        | _ -> ()

        match visualParent with
        | None -> false
        | Some parent -> 
            let parentSize = parent.Bounds.Size
        
            let x = this.Bounds.X + point.X
            let y = this.Bounds.Y + point.Y

            Rect(Point(), parentSize).Contains(Point(x, y))

    do
        this.EffectiveViewportChanged.Add (fun args ->
            let currentGrid = calculateGrid this.TileSize this.Margin this.Sprites.Length effectiveViewPort.Size
            let newGrid = calculateGrid this.TileSize this.Margin this.Sprites.Length args.EffectiveViewport.Size

            effectiveViewPort <- args.EffectiveViewport

            if currentGrid <> newGrid then                
                printfn "%i x %i" this.Columns this.Rows
                this.InvalidateVisual())

    member val TileSize : int = tileSize

    member val Sprites : Sprite[] = Array.create 18000 ({ Background = Colors.Red })
    static member SpritesProperty = AvaloniaProperty.RegisterDirect<SpritesGrid, Sprite[]>("Sprites", fun o -> o.Sprites)

    member val Margin : int = 2
    static member MarginProperty = AvaloniaProperty.RegisterDirect<SpritesGrid, int>("Margin", fun o -> o.Margin)
    
    member this.EffectiveViewPort = effectiveViewPort
    member this.Columns = calculateGrid this.TileSize this.Margin this.Sprites.Length this.EffectiveViewPort.Size |> fst
    member this.Rows = calculateGrid this.TileSize this.Margin this.Sprites.Length this.EffectiveViewPort.Size |> snd

    override this.MeasureOverride(_) = calculateSize this.TileSize this.Margin this.Sprites.Length this.EffectiveViewPort.Size
    override this.ArrangeOverride(_) = calculateSize this.TileSize this.Margin this.Sprites.Length this.EffectiveViewPort.Size

    override this.Render(context) =
        if not (this.IsArrangeValid && this.IsMeasureValid = false) then
            for j in 0..(this.Rows - 1) do
                for i in 0..(this.Columns - 1) do
                    let index = (j * this.Columns) + i
                    if index < this.Sprites.Length then
                        let sprite = this.Sprites[index]

                        let x = this.Margin + ((this.TileSize + (this.Margin * 2)) * i)
                        let y = this.Margin + ((this.TileSize + (this.Margin * 2)) * j)

                        let rect = Rect(x, y, this.TileSize, this.TileSize)
                        
                        if rect.TopLeft |> isInParentBounds
                        || rect.BottomRight |> isInParentBounds
                        || rect.BottomLeft |> isInParentBounds then
                            context.DrawRectangle(Pen(SolidColorBrush(sprite.Background)), rect)

        base.Render(context)

type State = { Count : int; }

let init() = { Count = 30; }

type Msg =
| Unit

let update msg state =
    match msg with
    | Unit -> state

// type ExperimentalAcrylicBorder with
//     static member material<'t when 't :> ExperimentalAcrylicBorder>(value : ExperimentalAcrylicMaterial) : IAttr<'t> =
//         AttrBuilder.CreateProperty<ExperimentalAcrylicMaterial>(ExperimentalAcrylicBorder.MaterialProperty, value, ValueNone)

let view state dispatch =
    let sprites : Sprite seq = (Array.create state.Count ({ Background = Colors.Red }))
    ScrollViewer.create [
        View.createGeneric<SpritesGrid> [
            Control.horizontalAlignment HorizontalAlignment.Left
            Control.verticalAlignment VerticalAlignment.Top
        ]
        |> ScrollViewer.content
    ]

    // View.createGeneric<ExperimentalAcrylicBorder> [
    //     let material = ExperimentalAcrylicMaterial()
    //     material.BackgroundSource <- AcrylicBackgroundSource.Digger
    //     material.TintColor <- Colors.Black
    //     material.TintOpacity <- 1.0
    //     material.MaterialOpacity <- 0.65

    //     ExperimentalAcrylicBorder.child content
    //     ExperimentalAcrylicBorder.material material
    // ]
