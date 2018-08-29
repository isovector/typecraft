{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Art where

import Data.Spriter.Skeleton
import Data.Spriter.Types
import Data.Vector (Vector, (!), fromList)
import Overture hiding (Attempt (..))


makeSprites :: Schema -> Maybe Color -> Vector Form
makeSprites schema correction = fromList $ toProp
                 <$> schema ^. schemaFolder._head.folderFile
  where
    toProp File{..} = toForm . ImageElement Nothing correction
                             $ "assets/gareth/" <> _fileName

drawArt :: Art
        -> Maybe Color
        -> Form
drawArt Art{ _aCanned = CannedAnim{..}
           , _aTime
           } correction =
  let Just entity    = _aSchema ^. schemaEntity    . at _aEntity
      Just animation = entity   ^. entityAnimation . at _aAnim
      thisFrame = _aTime * _aSpeedMult
      totalLength = animation ^. animLength
      frame = case _aRepeat || thisFrame <= totalLength of
                True -> fmod totalLength thisFrame
                False -> totalLength - 1
      sprites = makeSprites _aSchema correction
      drawBone ResultBone{..} = move (V2 _rbX $ -_rbY)
                              . rotate (-_rbAngle)
                              . group
                              . return
                              . scaleXY _rbScaleX _rbScaleY
   in case animate animation frame of
        Just (filter (not . isBone) -> objs) ->
          scale _aScale
            .  group
            . fmap (\x -> drawBone x $ sprites ! (_boneObjFile . fromJust $ _rbObj x))
            $ objs
        Nothing -> blank

