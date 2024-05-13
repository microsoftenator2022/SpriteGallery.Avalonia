namespace SpriteGallery.Avalonia

open SpriteGallery.Avalonia.Common

type SpritesData =
    {
        Textures : Map<(string * int64), SpriteTexture>
        Sprites : Sprite seq
    }

module SpritesData =
    let init() = { Textures = Map.empty; Sprites = [] }

