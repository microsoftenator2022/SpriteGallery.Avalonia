module SpriteGallery.Avalonia.Common

open Avalonia.Controls
open Avalonia.Input.Platform
open Avalonia.Media
open System.Runtime.InteropServices

let appVersionString = 
    let ass = System.Reflection.Assembly.GetExecutingAssembly()
    System.Diagnostics.FileVersionInfo.GetVersionInfo(ass.Location).ProductVersion

let mutable logStream : System.IO.StreamWriter option = None
let openLog() =
    let assLocation = System.Reflection.Assembly.GetExecutingAssembly().Location
    let filePath =
        System.IO.Path.Join(System.IO.Path.GetDirectoryName(assLocation), "log.txt")
    let stream = System.IO.File.CreateText(filePath)
    logStream <- Some(stream)

    stream

let closeLog() =
    logStream |> Option.iter (fun s -> s.Dispose())
    logStream <- None

let log (message : string) =
    #if DEBUG
    System.Diagnostics.Debug.Print message
    #endif

    let s = logStream |> Option.defaultWith(fun () -> openLog())

    s.WriteLine(message)
    s.Flush()

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
    let handle = System.Runtime.InteropServices.GCHandle.Alloc(bytes, GCHandleType.Pinned)
    
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
    Name : string
    RenderDataKey : struct (System.Guid * int64) option
    SerializedFile : string
    Container : string
    PathID : int64
    BlueprintReference : (string * int64) option
    mutable ScaledBitmapCache : Avalonia.Media.Imaging.Bitmap array }
with
    member this.ReferenceAssetId = this.BlueprintReference |> Option.map fst |> Option.defaultValue ""
    member this.ReferenceFileId = this.BlueprintReference |> Option.map (snd >> _.ToString()) |> Option.defaultValue ""
    
    static member Create ((texture : SpriteTexture), (rect : Avalonia.PixelRect)) =
        //let rect =
        //    if (rect.X + rect.Width) > texture.Size.Width then
        //        rect.WithWidth(texture.Size.Width - rect.X)
        //    else rect

        //let rect =
        //    if (rect.Y + rect.Height) > texture.Size.Height then
        //        rect.WithHeight(texture.Size.Height - rect.Y)
        //    else rect

        //let rect =
        //    if rect.X < 0 then
        //        rect.WithWidth(rect.Width + rect.X).WithX(0)
        //    else rect

        //let rect =
        //    if rect.Y < 0 then
        //        rect.WithHeight(rect.Height + rect.Y).WithY(0)
        //    else rect

        if rect.Height < 0 || rect.Width < 0 then
            System.ArgumentOutOfRangeException("Invalid texture rect") |> raise
        
        {
            BaseTexture = texture
            Rect = rect
            Name = ""
            RenderDataKey = None
            SerializedFile = ""
            Container = ""
            PathID = 0
            BlueprintReference = None
            ScaledBitmapCache = [||]
        }

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

let writePng (stream : System.IO.Stream) sprite =
    let source = System.Span(sprite.BaseTexture.Bytes)
    let stride = sprite.BaseTexture.Bitmap.Force().PixelSize.Width * 4
    let rect = sprite.Rect
    let dest = Array.zeroCreate<byte> (rect.Width * rect.Height * 4)
    copyRect source stride rect (System.Span(dest))
    
    use bitmap = createBitmap dest sprite.Rect.Size
    
    bitmap.Save(stream)

let saveSprite path sprite =
    use stream = System.IO.File.OpenWrite(path)
    sprite |> writePng stream

let copySpriteToClipboard (clipboard : IClipboard, sprite) =
    //let source = System.Span(sprite.BaseTexture.Bytes)
    //let stride = sprite.BaseTexture.Bitmap.Force().PixelSize.Width * 4
    //let rect = sprite.Rect
    //let dest = Array.zeroCreate<byte> (rect.Width * rect.Height * 4)
    //copyRect source stride rect (System.Span(dest))
    
    //use bitmap = createBitmap dest sprite.Rect.Size
    //let ms = new System.IO.MemoryStream()

    //bitmap.Save(ms)
    
    let ms = new System.IO.MemoryStream()
    sprite |> writePng ms
    
    let pngBytes = ms.ToArray()

    let dataObject = Avalonia.Input.DataObject()
    
    dataObject.Set("image/png", pngBytes)
    
    Avalonia.Threading.Dispatcher.UIThread.InvokeAsync (fun () -> clipboard.SetDataObjectAsync(dataObject))
    |> Async.AwaitTask

let copyTextToClipboard (clipboard : IClipboard, text) =
    Avalonia.Threading.Dispatcher.UIThread.InvokeAsync (fun () -> clipboard.SetTextAsync(text)) |> Async.AwaitTask
