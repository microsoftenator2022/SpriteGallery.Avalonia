module SpriteGallery.Avalonia.Common

open Avalonia.Controls
open Avalonia.Media
open Avalonia.FuncUI.DSL

let defaultTileSize = 64

[<AutoOpen>]
module ExperimentalAcrylicBorder =
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Types
    
    type ExperimentalAcrylicBorder with
        static member material<'t when 't :> ExperimentalAcrylicBorder>(value : ExperimentalAcrylicMaterial) : IAttr<'t> =
            AttrBuilder.CreateProperty<ExperimentalAcrylicMaterial>(ExperimentalAcrylicBorder.MaterialProperty, value, ValueNone)

[<AutoOpen>]
module ScrollViewer =
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Types

    type Control with
        static member bringIntoViewOnFocusChange<'t when 't :> Control>(value : bool) : IAttr<'t> =
            AttrBuilder.CreateProperty<bool>(ScrollViewer.BringIntoViewOnFocusChangeProperty, value, ValueNone)

let acrylicMaterial color =
    let material = ExperimentalAcrylicMaterial()

    material.BackgroundSource <- AcrylicBackgroundSource.Digger
    material.FallbackColor <- color
    material.TintColor <- color
    material.TintOpacity <- 1
    material.MaterialOpacity <- 0.5

    material

let tryGetThemeResource<'a> resourceKey (window : Window) : 'a option =
    match window.TryFindResource(resourceKey, window.ActualThemeVariant) with
    | true, color ->
        match color with
        | :? 'a as resource -> Some resource
        | _ ->
            eprintfn "Resource is not %A. Is %A" (typeof<'a>) (color.GetType())
            None
    | _ -> None

let tryGetColor (name : string) (window : Window) = tryGetThemeResource<Color> name window

type WindowColors = {
    AcrylicColor : Color option
    PanelAcrylicColor : Color option
    HighlightBrush : IBrush option
}
with
    static member GetColors (window : Window) =
        {
            AcrylicColor =
                window |> tryGetColor "SystemAltMediumHighColor"

            PanelAcrylicColor =
                window |> tryGetColor "SystemAltMediumColor"

            HighlightBrush =
                window |> tryGetThemeResource<IBrush> "SystemControlHighlightAccentBrush"
        }
    static member Defaults =
        {|
            AcrylicColor = Colors.DimGray
            PanelAcrylicColor = Colors.Gray
            HighlightBrush = Brushes.Blue
        |}
    
    member this.AcrylicColorOrDefault = this.AcrylicColor |> Option.defaultValue WindowColors.Defaults.AcrylicColor
    member this.PanelAcrylicColorOrDefault = this.PanelAcrylicColor |> Option.defaultValue WindowColors.Defaults.PanelAcrylicColor
    member this.HighlightBrushOrDefault = this.HighlightBrush |> Option.defaultValue WindowColors.Defaults.HighlightBrush

let copyRect (textureBytes : System.Span<byte>) stride (rect : Avalonia.PixelRect) (dest : System.Span<byte>) =
    let xOffsetBytes = rect.X * 4
    let widthBytes = rect.Width * 4

    for n in 0..(rect.Height - 1) do
        let line = textureBytes.Slice((n + rect.Y) * stride, stride)

        let source = line.Slice(xOffsetBytes, widthBytes)
        let destLine = dest.Slice((rect.Height - n - 1) * widthBytes, widthBytes)
        
        source.CopyTo(destLine)

let createBitmap bytes size =
    let handle = System.Runtime.InteropServices.GCHandle.Alloc(bytes)
    
    try
        let bitmap =
            new Avalonia.Media.Imaging.Bitmap(
                Avalonia.Platform.PixelFormat.Bgra8888,
                Avalonia.Platform.AlphaFormat.Unpremul,
                System.Runtime.InteropServices.Marshal.UnsafeAddrOfPinnedArrayElement(bytes, 0),
                // System.Runtime.InteropServices.GCHandle.ToIntPtr(handle),
                size,
                Avalonia.Vector(96, 96),
                size.Width * 4)

        bitmap

    finally
        handle.Free()

type SpriteTexture (bytes : byte[], size : Avalonia.PixelSize) =
    member val Bitmap = lazy (createBitmap bytes size)
    member _.Bytes = bytes
    member _.Size = size

    interface System.IDisposable with
        member this.Dispose() =
            if this.Bitmap.IsValueCreated then
                this.Bitmap.Value.Dispose()

    override this.Finalize() = (this :> System.IDisposable).Dispose()
    
type Sprite =
  { BaseTexture : SpriteTexture
    Rect : Avalonia.PixelRect
    Name : string option
    RenderDataKey : struct (System.Guid * int64) option
    SerializedFile : string
    Container : string
    PathID : int64
    BlueprintReference : (string * int64) option
    mutable ScaledBitmapCache : Avalonia.Media.Imaging.Bitmap array }
with
    static member Create (texture, rect) =
      { BaseTexture = texture
        Rect = rect
        Name = None
        RenderDataKey = None
        SerializedFile = ""
        Container = ""
        PathID = 0
        BlueprintReference = None
        ScaledBitmapCache = [||] }

    interface System.IDisposable with
        member this.Dispose() =
            let scaledBitmapCache = this.ScaledBitmapCache
            this.ScaledBitmapCache <- [||]
            for bitmap in scaledBitmapCache do
                bitmap.Dispose()

    override this.Finalize() = (this :> System.IDisposable).Dispose()

    member this.GetHeightScaledBitmap (height : int, ?scaleTolerance : float, ?fractional : bool) =
        let height = min height this.Rect.Height

        let scaleTolerance = defaultArg scaleTolerance 0.0
        let fractional = defaultArg fractional false

        let isWholeNumber (x : float) =
            x - (x |> int |> float) = 0.0

        let cached = 
            this.ScaledBitmapCache
            |> Array.tryFind (fun b -> b.PixelSize.Height = height)
            |> Option.orElseWith (fun () ->
                if scaleTolerance = 0.0 then None
                else
                    this.ScaledBitmapCache
                    |> Seq.map (fun b -> b, ((b.PixelSize.Height |> float) / (height |> float)) - 1.0)
                    |> Seq.where (fun (_, scaleDelta) -> fractional || (scaleDelta |> isWholeNumber))
                    |> Seq.where (fun (_, scaleDelta) -> scaleDelta > 0.0 && scaleDelta < scaleTolerance)
                    |> Seq.sortBy (fun (_, scaleDelta) -> scaleDelta)
                    |> Seq.map (fun (b, _) -> b)
                    |> Seq.tryHead)

        match cached with
        | Some bitmap -> bitmap
        | None ->
            let aspectRatio = this.Rect.Size.AspectRatio

            let bytes = Array.zeroCreate<byte> (this.Rect.Width * this.Rect.Height * 4)

            copyRect (System.Span(this.BaseTexture.Bytes)) (this.BaseTexture.Size.Width * 4) (this.Rect) (System.Span(bytes))

            use bitmap = createBitmap bytes (this.Rect.Size)

            let width = aspectRatio * (height |> float) |> int |> max 1

            let bitmap = bitmap.CreateScaledBitmap(Avalonia.PixelSize(width, height))

            this.ScaledBitmapCache <- this.ScaledBitmapCache |> Array.append [| bitmap |]
            
            bitmap


let tryGetIcon filename =
    let resourceName = $"SpriteGallery.Avalonia.{filename}"

    let ass = System.Reflection.Assembly.GetExecutingAssembly()
    
    if ass.GetManifestResourceNames() |> Seq.contains resourceName then
        let stream = ass.GetManifestResourceStream(resourceName)
        let bitmap = new Avalonia.Media.Imaging.Bitmap(stream)

        stream.Dispose()

        Some bitmap
    else None

let mutable appIcon = None

let tryGetAppIcon() =
    appIcon
    |> Option.orElseWith (fun () -> tryGetIcon "owlcat_suspecting_framed.png")

let mutable owlcat_suspecting_icon = None

let tryGetSuspectingIcon() =
    owlcat_suspecting_icon
    |> Option.orElseWith (fun () -> tryGetIcon "owlcat_suspecting.png")
