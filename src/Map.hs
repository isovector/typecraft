{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module Map where

import GameData
import qualified Algorithm.Search.JumpPoint as JP
import qualified Data.Map as M
import           Data.Tiled
import           Overture hiding (distance)


getTileCrop :: Tileset -> Word32 -> Form
getTileCrop ts = \gid ->
  let g      = fromIntegral $ gid - tsInitialGid ts
      crop   = Crop (fromIntegral $ g `mod` stride * tileWidth)
                    (fromIntegral $ g `div` stride * tileHeight)
                    tileWidth
                    tileHeight
   in toForm
    . croppedImage crop
    $ "maps/" <> fs
  where
    img    = head $ tsImages ts
    fs     = iSource img
    stride = iWidth img `div` tileWidth


-- TODO(sandy): this leaks memory
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
        (buildDudesLayer me)
  where
    getLayer name = maybe (error $ "no " <> name <> " layer") id
                  $ find ((== name) . layerName) mapLayers
    ground    = getLayer "ground"
    doodads   = getLayer "doodads"
    collision = getLayer "collision"
    me        = getLayer "me"
    ts = orderTilesets mapTilesets


buildDudesLayer :: Layer -> Game ()
buildDudesLayer l = do
  for_ (layerObjects l) $ \Object{..} ->
    createEntity $ (protos M.! read (fromJust objectType))
      { pos   = Just $ V2 objectX objectY
      , owner = Just mePlayer
      }


buildNavMesh :: Int -> Int -> Layer -> NavMesh
buildNavMesh w h l =
  let closed = do
        x <- [0..w-1]
        y <- [0..h-1]
        let p = (x, y)
        guard $ not $ isOpen l p
        pure p
   in foldr (\p -> JP.closeArea p p) (JP.make w h) closed


isOpen :: Layer -> (Int, Int) -> Bool
isOpen l xy = maybe True (const False) $  M.lookup xy $ layerData l


maps :: M.Map String Map
maps = M.fromList $
  [ "wc2" ]
   <&> \i -> ( i
             , let !x = parseMap . unsafePerformIO
                                 . loadMapFile
                                 $ "maps/" <> i <> ".tmx"
                in x
             )
{-# NOINLINE maps #-}

