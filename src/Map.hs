{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module Map where

import qualified Data.Map as M
import qualified Data.PathGrid as PG
import           Data.Tiled
import           Overture hiding (distance)


getTileCrop :: Tileset -> Word32 -> Form
getTileCrop ts = \gid ->
  let g      = fromIntegral $ gid - tsInitialGid ts
      img    = head $ tsImages ts
      fs     = iSource img
      stride = iWidth img `div` tileWidth
      crop   = Crop (g `mod` stride * tileWidth)
                    (g `div` stride * tileHeight)
                    tileWidth
                    tileHeight
   -- TODO(sandy): probably smarter to do this shifting later, when we draw it
   in move (negate $ V2 tileWidth tileHeight)
    . toForm
    . croppedImage crop
    $ "maps/" <> fs


drawSquare :: Layer -> [Tileset] -> Int -> Int -> Maybe Form
drawSquare (Layer {..}) ts = \x y ->
  M.lookup (x, y) layerData <&> \(tileGid -> gid) ->
    getTileCrop (getTilesetForGid ts gid) gid
drawSquare _ _ = error "terrible layer choice"


getTilesetForGid :: [Tileset] -> Word32 -> Tileset
getTilesetForGid ts gid = head $ dropWhile ((> gid) . tsInitialGid) ts


orderTilesets :: [Tileset] -> [Tileset]
orderTilesets = sortBy . flip $ comparing tsInitialGid


parseMap :: TiledMap -> Map
parseMap TiledMap{..} =
    Map (drawSquare ground ts)
        (drawSquare doodads ts)
        (buildNavMesh mapWidth mapHeight collision)
        -- (NavMesh (isOpen collision)
        --        $ makeGrid mapWidth mapHeight collision)
        mapWidth
        mapHeight
  where
    getLayer name = maybe (error $ "no " <> name <> " layer") id
                  $ find ((== name) . layerName) mapLayers
    ground    = getLayer "ground"
    doodads   = getLayer "doodads"
    collision = getLayer "collision"
    ts = orderTilesets mapTilesets

buildNavMesh :: Int -> Int -> Layer -> NavMesh
buildNavMesh w h l =
  let closed = do
        x <- [0..w-1]
        y <- [0..h-1]
        let p = (x, y)
        guard $ not $ isOpen l p
        pure p
   in foldr (\p -> PG.closeArea p p) (PG.make w h) closed


isOpen :: Layer -> (Int, Int) -> Bool
isOpen l xy = maybe True (const False) $  M.lookup xy $ layerData l


maps :: M.Map String Map
maps = M.fromList $
  [ "rpg2k"
  , "hoth"
  ]
  <&> \i -> ( i
            , parseMap . unsafePerformIO
                       . loadMapFile
                       $ "maps/" <> i <> ".tmx"
            )
{-# NOINLINE maps #-}

