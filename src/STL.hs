module STL (
    V2(..),
    V4(..),
    ($=),
    sleep,
    opaque,
    transparent,
    drawTriangle,
  ) where

import SDL (Renderer, V2(..), V4(..), ($=))
import qualified SDL
import GHC.Word
import Foreign.C.Types
import Control.Concurrent

-- SDL_ALPHA_TRANSPARENT
transparent :: Word8
transparent = 0

-- SDL_ALPHA_OPAQUE
opaque :: Word8
opaque = 255

-- sleeps for a determined amount of seconds
sleep :: Int -> IO ()
sleep = threadDelay . (*1000000)

-- An alias for a 2 dimension vector
type Point = V2 CInt

-- Draw a triangle using vertex p, q and r.
-- Lines are drawn from p->q->r->p (p to q, then q to r, finally closing r to p)
drawTriangle :: Renderer -> Point -> Point -> Point -> IO ()
drawTriangle renderer p q r = do
  SDL.drawLine renderer (SDL.P p) (SDL.P q)
  SDL.drawLine renderer (SDL.P q) (SDL.P r)
  SDL.drawLine renderer (SDL.P r) (SDL.P p)
