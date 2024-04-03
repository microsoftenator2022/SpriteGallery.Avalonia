module SpriteGallery.Avalonia.Controls.SpritesGrid

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree

open SpriteGallery.Avalonia.Common

[<Struct>]
type SpriteCell = { CellSpan : int; Sprite : Sprite option; ColumnIndex : int; RowIndex : int }

let coordComponentToGrid (tileSize : float) (margin : float) (x : float) =
    ((x - (2.0 * margin)) / (tileSize + (2.0 * margin))) |> int

let pointToGrid (tileSize : float) (margin : float) (point : Point) =
    let toGrid = coordComponentToGrid tileSize margin
    point.X |> toGrid, point.Y |> toGrid

let gridToRect (tileSize : float) (margin : float) (column : int, row : int) =
    let d x = (margin * 2.0) + (((margin * 2.0) + tileSize) * x)
    Rect(d column, d row, tileSize, tileSize)

let calculateColumnCount (tileSize : float) (margin : float) (width : float) =
    (coordComponentToGrid tileSize margin width) + 1

let calculateGrid (tileSize : float) (margin : float) tileCount (size : Size) =
    let columns = (calculateColumnCount tileSize margin size.Width)
    let rows = (tileCount |> float) / (columns |> float) |> ceil |> int

    (columns, rows)

let calculateSize (tileSize : float) (margin : float) tileCount (size : Size) =
    let columns, rows = calculateGrid tileSize margin tileCount size
    Rect(Point(), (gridToRect tileSize margin (columns, rows)).BottomRight).Size

let arBias = 0.25

let generateLayout (sprites : Sprite seq) columns (tileSize : float) =
    // printfn "generating %i column layout" columns
    let layout =
        seq {
            let mutable ci = 0
            let mutable ri = 0

            for sprite in sprites do
                let aspectRatio = (sprite.Rect.Width |> float) / (max tileSize sprite.Rect.Height |> float)
                
                let cellSpan =
                    if sprite.Rect.Width > (tileSize |> int) then
                        max 1 (min (ceil (aspectRatio - arBias) |> int) (columns - 1))
                    else 1

                if ci + cellSpan >= columns then
                    ci <- 0
                    ri <- ri + 1

                yield { Sprite = Some sprite; CellSpan = cellSpan; ColumnIndex = ci; RowIndex = ri }

                ci <- ci + cellSpan
        }
        |> Seq.toArray

    layout

let cellSize (tileSize : float) (margin : float) (cell : SpriteCell) =
    let cs = cell.CellSpan |> float
    Size((cs * tileSize) + ((cs - 1.0) * margin), tileSize)

type SpritesGrid () as this =
    inherit Control()

    let cellSize cell = cellSize this.TileSize this.Margin cell

    let mutable layout : (int * SpriteCell[]) = (0, Array.empty)
    let generateLayout sprites columns = generateLayout sprites columns this.TileSize

    let coordComponentToGrid x = coordComponentToGrid this.TileSize this.Margin x
    let pointToGrid p = pointToGrid this.TileSize this.Margin p
    let gridToRect g = gridToRect this.TileSize this.Margin g
    let calculateColumnCount width = calculateColumnCount this.TileSize this.Margin width

    let getRowCount() = layout |> snd |> Array.tryLast |> function Some c -> c.RowIndex + 1 | None -> 0

    let calculateActualSize (size : Size) =
        let columns = calculateColumnCount size.Width

        if fst layout <> columns then
            layout <- columns, generateLayout this.Sprites columns

        let rows = getRowCount()

        let bottomRightRect = gridToRect (columns, rows)
        
        Rect(Point(), bottomRightRect.BottomRight).Size
    
    let mutable sprites : Sprite[] = Array.empty

    let mutable effectiveViewport = Rect()

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
            if coordComponentToGrid (args.EffectiveViewport.Bottom) <> coordComponentToGrid (effectiveViewport.Bottom) then
                this.InvalidateVisual()

            effectiveViewport <- args.EffectiveViewport
        )

    override this.OnSizeChanged(args) =
        let columns = calculateColumnCount args.NewSize.Width

        if columns <> calculateColumnCount args.PreviousSize.Width then
            layout <- columns, columns |> generateLayout this.Sprites
            this.InvalidateVisual()

        base.OnSizeChanged(args)

    override this.OnPointerPressed(args) =
        let (row, column) = args.GetPosition(this) |> pointToGrid
        let spriteCell = layout |> snd |> Seq.tryFind (fun c -> c.RowIndex = row && c.ColumnIndex <= column && (c.ColumnIndex + c.CellSpan) > column)
        
        this.SelectedSprite <-
            if spriteCell = this.SelectedSprite then
                None
            else spriteCell

        printfn "%A" this.SelectedSprite

        args.Handled <- true
    
    member val SelectedSprite : SpriteCell option = None with get, set

    member val TileSize : float = defaultTileSize with get, set
    static member TileSizeProperty = AvaloniaProperty.RegisterDirect<SpritesGrid, float>(
        nameof(Unchecked.defaultof<SpritesGrid>.TileSize),
        (fun o -> o.TileSize),
        (fun o v -> o.TileSize <- v))

    member _.Sprites
        with get() : Sprite[] = sprites
        and set (value : Sprite[]) =
            sprites <- value
            this.InvalidateMeasure()
    static member SpritesProperty =
        AvaloniaProperty.RegisterDirect<SpritesGrid, Sprite[]>(
            nameof(Unchecked.defaultof<SpritesGrid>.Sprites),
            (fun o -> o.Sprites),
            (fun o v -> o.Sprites <- v))
    
    member val Margin : float = 2 with get, set
    static member MarginProperty = AvaloniaProperty.RegisterDirect<SpritesGrid, float>(
        nameof(Unchecked.defaultof<SpritesGrid>.Margin),
        (fun o -> o.Margin),
        (fun o v -> o.Margin <- v))

    member this.Columns = fst layout
    member this.Rows = getRowCount()

    override this.MeasureOverride(size) = calculateActualSize size
    override this.ArrangeOverride(size) = calculateActualSize size

    override this.Render(context) =
        if not (this.IsArrangeValid && this.IsMeasureValid = false) then
            for cell in snd layout do
                let rect = (gridToRect (cell.ColumnIndex, cell.RowIndex)).WithWidth((cellSize cell).Width)
                
                if rect.TopLeft |> isInParentBounds
                // || rect.BottomRight |> isInParentBounds
                || rect.BottomLeft |> isInParentBounds then
                    // if sprite.Rect.Height < this.TileSize then
                    //     context.DrawImage(sprite.BaseTexture.Bitmap.Force(), sprite.Rect.ToRectWithDpi(96), rect)
                    // else
                    match cell.Sprite with
                    | Some s ->
                        let bitmap = s.GetHeightScaledBitmap(this.TileSize |> int)
                        let size = MediaExtensions.CalculateSize(Stretch.Uniform, rect.Size, bitmap.Size, StretchDirection.DownOnly)
                        let rect = rect.CenterRect(Rect(rect.Position, size))

                        context.DrawImage(bitmap, rect)
                    | None -> ()

        base.Render(context)

[<AutoOpen>]
module SpritesGrid =
    let create(attrs : IAttr<SpritesGrid> list) : IView<SpritesGrid> = ViewBuilder.Create<SpritesGrid>(attrs)

    type SpritesGrid with
        static member sprites<'t when 't :> SpritesGrid>(value : Sprite[]) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<Sprite[]>(SpritesGrid.SpritesProperty, value, ValueNone)
        static member margin<'t when 't :> SpritesGrid>(value : float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(SpritesGrid.MarginProperty, value, ValueNone)
        static member tileSize<'t when 't :> SpritesGrid>(value : float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(SpritesGrid.TileSizeProperty, value, ValueNone)
