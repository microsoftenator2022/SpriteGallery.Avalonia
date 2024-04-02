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

open SpriteGallery.Avalonia.Common

module SpriteGridCalc =
    let coordComponentToGrid (tileSize : float) (margin : float) (x : float) =
        ((x - (2.0 * margin)) / (tileSize + (2.0 * margin))) |> int

    let pointToGrid (tileSize : float) (margin : float) (point : Point) =
        let toGrid = coordComponentToGrid tileSize margin
        point.X |> toGrid, point.Y |> toGrid

    let gridToRect (tileSize : float) (margin : float) (column : int, row : int) =
        let d x = (margin * 2.0) + (((margin * 2.0) + tileSize) * x)
        Rect(d column, d row, tileSize, tileSize)

    let calculateColumnCount (tileSize : float) (margin : float) (width : float) =
        coordComponentToGrid tileSize margin width

    let calculateGrid (tileSize : float) (margin : float) tileCount (size : Size) =
        let columns = (calculateColumnCount tileSize margin size.Width)
        let rows = (tileCount |> float) / (columns |> float) |> ceil |> int

        (columns, rows)

    let calculateSize (tileSize : float) (margin : float) tileCount (size : Size) =
        let columns, rows = calculateGrid tileSize margin tileCount size
        Rect(Point(), (gridToRect tileSize margin (columns, rows)).BottomRight).Size

open SpriteGridCalc

type SpritesGrid () as this =
    inherit Control()

    let coordComponentToGrid x = coordComponentToGrid this.TileSize this.Margin x
    let pointToGrid p = pointToGrid this.TileSize this.Margin p
    let gridToRect c = gridToRect this.TileSize this.Margin c
    let calculateColumnCount width = calculateColumnCount this.TileSize this.Margin width
    let calculateGrid tileCount size = calculateGrid this.TileSize this.Margin tileCount size
    let calculateSize tileCount size = calculateSize this.TileSize this.Margin tileCount size

    let mutable sprites : Sprite[] = Array.empty

    let tileRect (column : int) (row : int) = gridToRect (column, row)

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
        this.SizeChanged.Add (fun args ->
            // let currentGrid = calculateGrid this.TileSize this.Margin this.Sprites.Length args.PreviousSize

            // let size =
            //     args.NewSize
            //     // match visualParent with
            //     // | Some p -> p.Bounds.Size
            //     // | None -> args.EffectiveViewport.Size

            // let newGrid = calculateGrid this.TileSize this.Margin this.Sprites.Length size

            // let size = calculateSize this.TileSize this.Margin sprites.Length size
            // printfn "Calculated size: %f x %f" size.Width size.Height

            // let columns = (calculateColumnCount this.TileSize this.Margin size.Width) |> max 1

            // let i = (sprites.Length - 1)
            // let r = i / columns
            // let c = i % columns

            // let g = calculateGrid this.TileSize this.Margin sprites.Length size

            // let rect = tileRect c r
            // printfn "Last sprite c/r: %i, %i" c r
            // printfn "Grid size c/r: %i, %i" <|| g
            // printfn "Last sprite rect: (%f, %f) -> (%f, %f)" rect.TopLeft.X rect.TopLeft.Y rect.BottomRight.X rect.BottomRight.Y
            // let dy = (rect.BottomRight.Y - size.Height)
            // printfn "dY = %f = %f rows" dy (dy / this.TileSize)
            let calculateGrid = calculateGrid this.Sprites.Length

            if calculateGrid args.NewSize <> calculateGrid args.PreviousSize then
                this.InvalidateVisual())

    member val TileSize : float = tileSize

    member _.Sprites
        with get() : Sprite[] = sprites
        and set (value : Sprite[]) =
            sprites <- value

    member val Margin : float = 2 with get, set

    member this.Columns = calculateGrid this.Sprites.Length this.Bounds.Size |> fst
    member this.Rows = calculateGrid this.Sprites.Length this.Bounds.Size |> snd

    override this.MeasureOverride(size) = calculateSize this.Sprites.Length size
    override this.ArrangeOverride(size) = calculateSize this.Sprites.Length size

    override this.Render(context) =
        if not (this.IsArrangeValid && this.IsMeasureValid = false) then
            for (i, s) in this.Sprites |> Seq.indexed do
                let row = i / this.Columns
                let column = i % this.Columns

                let rect = gridToRect (column, row)
                
                if rect.TopLeft |> isInParentBounds
                || rect.BottomRight |> isInParentBounds
                || rect.BottomLeft |> isInParentBounds then
                    // if sprite.Rect.Height < this.TileSize then
                    //     context.DrawImage(sprite.BaseTexture.Bitmap.Force(), sprite.Rect.ToRectWithDpi(96), rect)
                    // else
                        context.DrawImage(s.GetHeightScaledBitmap(this.TileSize |> int), rect)

        base.Render(context)

[<AutoOpen>]
module SpritesGrid =
    let create(attrs : IAttr<SpritesGrid> list) : IView<SpritesGrid> = ViewBuilder.Create<SpritesGrid>(attrs)

module GridView =
    open SpriteGallery.Avalonia

    type State = { Sprites : SpritesData option; Refresh : bool }

    let init() = { Sprites = None; Refresh = false }

    type Msg =
    | Unit
    | UpdateSprites of SpritesData option
    | Refresh

    let update msg (state : State) =
        match msg with
        | Unit -> state, Cmd.none
        | UpdateSprites sprites ->
            { state with Sprites = sprites }, Cmd.ofMsg Refresh
        | Refresh -> { state with Refresh = not state.Refresh }, Cmd.ofMsg Unit

    let view (state : State) (dispatch : Dispatch<Msg>) =
        ScrollViewer.create [
            SpritesGrid.create [
                SpritesGrid.horizontalAlignment HorizontalAlignment.Left
                SpritesGrid.verticalAlignment VerticalAlignment.Top
                
                SpritesGrid.init (fun sg ->
                    let sprites =
                        match state.Sprites with
                        | Some sprites -> sprites.Sprites |> List.toArray
                        | None -> Array.empty

                    // printfn "%i sprites" sprites.Length

                    sg.Sprites <- sprites)
            ]
            |> View.withKey (state.Refresh.ToString())
            |> ScrollViewer.content
        ]
