module SpriteGallery.Avalonia.Controls.SpritesGrid

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree
open Avalonia.Rendering
open Avalonia.Interactivity

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

let getSpriteIndexInDirection (layout : SpriteCell[], index) directionKey =
    let currentColumn, currentRow = layout[index].ColumnIndex, layout[index].RowIndex

    match directionKey with
    | Key.Down ->
        layout
        |> Seq.pairwise
        |> Seq.tryFindIndex (fun (x, y) -> 
            x.RowIndex = currentRow + 1
            && (y.ColumnIndex > currentColumn || y.RowIndex > currentRow + 1)
            && x.ColumnIndex <= currentColumn)

    | Key.Up ->
        layout
        |> Seq.pairwise
        |> Seq.tryFindIndex (fun (x, y) ->
            x.RowIndex = currentRow - 1
            && (y.ColumnIndex > currentColumn || y.RowIndex > currentRow - 1)
            && x.ColumnIndex <= currentColumn)
    
    | Key.Left ->
        if index <= 0 then None
        elif layout[index - 1].RowIndex < layout[index].RowIndex then None
        else Some (index - 1)

    | Key.Right ->
        if index >= layout.Length then None
        elif layout[index + 1].RowIndex > layout[index].RowIndex then None
        else Some (index + 1)
    | _ -> None

type SpriteEventArgs (routedEvent : RoutedEvent, sprite : Sprite option) =
    inherit RoutedEventArgs(routedEvent)
    member _.Sprite = sprite

    override this.ToString() =
        sprintf "%s %A" routedEvent.Name this.Sprite

type SpritesGrid () as this =
    inherit Control()

    let mutable sprites : Sprite[] = Array.empty
    let mutable layout : (int * SpriteCell[]) = (0, Array.empty)
    let mutable selectedSpriteIndex = ValueNone

    let cellSize cell = cellSize this.TileSize this.TileMargin cell
    let generateLayout sprites columns = generateLayout sprites columns this.TileSize

    let coordComponentToGrid x = coordComponentToGrid this.TileSize this.TileMargin x
    let pointToGrid p = pointToGrid this.TileSize this.TileMargin p
    let gridToRect g = gridToRect this.TileSize this.TileMargin g
    let calculateColumnCount width = calculateColumnCount this.TileSize this.TileMargin width

    let getRowCount() = layout |> snd |> Array.tryLast |> function Some c -> c.RowIndex + 1 | None -> 0

    let calculateActualSize (size : Size) =
        let columns = calculateColumnCount size.Width

        if fst layout <> columns then
            layout <- columns, generateLayout this.Sprites columns

        let rows = getRowCount()

        let bottomRightRect = gridToRect (columns, rows)
        
        Rect(Point(), bottomRightRect.BottomRight).Size
    
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

    static let spriteSelectedEvent = RoutedEvent.Register<SpritesGrid, SpriteEventArgs>("SelectedSpriteChanged", RoutingStrategies.Bubble)
    static let copySpriteEvent = RoutedEvent.Register<SpritesGrid, SpriteEventArgs>("CopySprite", RoutingStrategies.Bubble)

    do
        this.Focusable <- true

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

    override this.OnKeyDown(args) =
        let (|DirectionKey|_|) (args : KeyEventArgs) =
            match args.Key with
            | Key.Up -> Some DirectionKey
            | Key.Down -> Some DirectionKey
            | Key.Left -> Some DirectionKey
            | Key.Right -> Some DirectionKey
            | _ -> None

        let (|CtrlC|_|) (args : KeyEventArgs) =
            match args.KeyModifiers, args.Key with
            | KeyModifiers.Control, Key.C -> Some CtrlC
            | _ -> None

        match args with
        | DirectionKey ->
            match this.SelectedSpriteIndex with
            | ValueSome index ->
                this.SelectedSpriteIndex <-
                    getSpriteIndexInDirection (snd layout, index) args.Key
                    |> function
                    | Some i -> ValueSome i
                    | None -> ValueSome index
            | ValueNone -> ()
        | CtrlC -> SpriteEventArgs(SpritesGrid.CopySpriteEvent, this.SelectedSprite |> function ValueSome s -> s.Sprite | ValueNone -> None) |> this.RaiseEvent
        | _ -> ()

    override this.OnPointerPressed(args) =
        let (column, row) = args.GetPosition(this) |> pointToGrid
        let selected = layout |> snd |> Seq.tryFindIndex (fun c -> c.RowIndex = row && c.ColumnIndex <= column && (c.ColumnIndex + c.CellSpan) > column)

        match selected with
        | Some index ->
            this.SelectedSpriteIndex <-
                if this.SelectedSpriteIndex = ValueSome index then ValueNone
                else ValueSome index

            args.Handled <- true
        | None -> ()

    member this.SelectedSpriteIndex
        with get() = selectedSpriteIndex
        and set value =
            if selectedSpriteIndex <> value then
                selectedSpriteIndex <- value

                let s =
                    match value with
                    | ValueSome i -> (layout |> snd).[i].Sprite
                    | ValueNone -> None
                
                SpriteEventArgs(SpritesGrid.SelectedSpriteChangedEvent, s) |> this.RaiseEvent

                this.InvalidateVisual()

    member this.SelectedSprite =
        match this.SelectedSpriteIndex with
        | ValueSome i ->
            let sprites = snd layout
            if i <= sprites.Length then ValueSome sprites[i] else ValueNone
        | ValueNone -> ValueNone

    member val TileSize : float = defaultTileSize with get, set
    static member TileSizeProperty = AvaloniaProperty.RegisterDirect<SpritesGrid, float>(
        nameof(Unchecked.defaultof<SpritesGrid>.TileSize),
        _.TileSize,
        (fun o v -> o.TileSize <- v))

    member _.Sprites
        with get() : Sprite[] = sprites
        and set (value : Sprite[]) =
            sprites <- value
            this.InvalidateMeasure()
    static member SpritesProperty =
        AvaloniaProperty.RegisterDirect<SpritesGrid, Sprite[]>(
            nameof(Unchecked.defaultof<SpritesGrid>.Sprites),
            _.Sprites,
            (fun o v -> o.Sprites <- v))
    
    member val TileMargin : float = 2 with get, set
    static member TileMarginProperty = AvaloniaProperty.RegisterDirect<SpritesGrid, float>(
        nameof(Unchecked.defaultof<SpritesGrid>.TileMargin),
        _.TileMargin,
        (fun o v -> o.TileMargin <- v))

    member val HighlightBrush : IBrush = Brushes.Blue with get, set
    static member HighlightBrushProperty = AvaloniaProperty.RegisterDirect<SpritesGrid, IBrush>(
        nameof(Unchecked.defaultof<SpritesGrid>.HighlightBrush),
        _.HighlightBrush,
        (fun o v -> o.HighlightBrush <- v))

    static member SelectedSpriteChangedEvent with get() = spriteSelectedEvent

    [<CLIEvent>]
    member _.SelectedSpriteChanged =
        { new IDelegateEvent<_> with
            member _.AddHandler value = this.AddHandler(SpritesGrid.SelectedSpriteChangedEvent, value)
            member _.RemoveHandler value = this.RemoveHandler(SpritesGrid.SelectedSpriteChangedEvent, value)
        }

    static member CopySpriteEvent with get() = copySpriteEvent

    [<CLIEvent>]
    member _.CopySprite =
        { new IDelegateEvent<_> with
            member _.AddHandler value = this.AddHandler(SpritesGrid.CopySpriteEvent, value)
            member _.RemoveHandler value = this.RemoveHandler(SpritesGrid.CopySpriteEvent, value)
        }

    member this.Columns = fst layout
    member this.Rows = getRowCount()

    override this.MeasureOverride(size) = calculateActualSize size
    override this.ArrangeOverride(size) = calculateActualSize size
    override this.Render(context) =
        if not (this.IsArrangeValid && this.IsMeasureValid = false) then
            for cell in snd layout do
                let rect = (gridToRect (cell.ColumnIndex, cell.RowIndex)).WithWidth((cellSize cell).Width)
                
                if rect.TopLeft |> isInParentBounds
                || rect.BottomLeft |> isInParentBounds then
                    match cell.Sprite with
                    | Some s ->
                        if this.SelectedSprite = ValueSome cell then
                            context.DrawRectangle(
                                Pen(this.HighlightBrush, this.TileMargin * 2.0),
                                Rect(
                                    x = rect.X - (this.TileMargin),
                                    y = rect.Y - (this.TileMargin),
                                    width = rect.Width + (this.TileMargin * 2.0),
                                    height = rect.Height + (this.TileMargin * 2.0)
                                )
                            )

                        let bitmap = s.GetHeightScaledBitmap(this.TileSize |> int)
                        let size = MediaExtensions.CalculateSize(Stretch.Uniform, rect.Size, bitmap.Size, StretchDirection.DownOnly)
                        let rect = rect.CenterRect(Rect(rect.Position, size))

                        context.DrawImage(bitmap, rect)
                    | None -> ()

        base.Render(context)

    interface ICustomHitTest with
        member this.HitTest point = true

[<AutoOpen>]
module SpritesGrid =
    let create(attrs : IAttr<SpritesGrid> list) : IView<SpritesGrid> = ViewBuilder.Create<SpritesGrid>(attrs)

    type SpritesGrid with
        static member sprites<'t when 't :> SpritesGrid>(value : Sprite[]) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<Sprite[]>(SpritesGrid.SpritesProperty, value, ValueNone)
        static member tileMargin<'t when 't :> SpritesGrid>(value : float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(SpritesGrid.TileMarginProperty, value, ValueNone)
        static member tileSize<'t when 't :> SpritesGrid>(value : float) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<float>(SpritesGrid.TileSizeProperty, value, ValueNone)
        static member highlightBrush<'t when 't :> SpritesGrid>(value : IBrush) : IAttr<'t> =
            AttrBuilder<'t>.CreateProperty<IBrush>(SpritesGrid.HighlightBrushProperty, value, ValueNone)
        static member onSelectedSpriteChanged<'t when 't :> SpritesGrid>(func, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription(SpritesGrid.SelectedSpriteChangedEvent, func, ?subPatchOptions = subPatchOptions)
        static member onCopySprite<'t when 't :> SpritesGrid>(func, ?subPatchOptions) =
            AttrBuilder<'t>.CreateSubscription(SpritesGrid.CopySpriteEvent, func, ?subPatchOptions = subPatchOptions)
