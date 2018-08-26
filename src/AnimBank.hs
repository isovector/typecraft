{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AnimBank where

import Data.Aeson
import Data.Spriter.Types
import Data.String.Conv (toS)
import Overture hiding (Attempt (..))


garethSchema :: Schema
garethSchema =
  let Just j = decode
             . toS
             . unsafePerformIO  -- doncha worry. it's fine.
             $ readFile "assets/gareth/gareth.scon"
      Success s = fromJSON j
   in s
{-# NOINLINE garethSchema #-}


__garethIdle :: CannedAnim
__garethIdle = CannedAnim garethSchema "gareth" "idle" 80 True

__garethAttack :: CannedAnim
__garethAttack = CannedAnim garethSchema "gareth" "attack" 80 True

