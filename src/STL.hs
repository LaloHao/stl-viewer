module STL (
    ($=),
    sleep,
    opaque,
    transparent,
    drawTriangle,
    color3f,
    vertex3f,
  ) where

import SDL (Renderer, V2(..), V4(..), ($=))
import Graphics.Rendering.OpenGL
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

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f r g b = vertex $ Vertex3 r g (b :: GLfloat)

-- Draw a triangle using vertex p, q and r.
-- Lines are drawn from p->q->r->p (p to q, then q to r, finally closing r to p)
-- drawTriangle :: Renderer -> Color3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
drawTriangle color v₀ v₁ v₂
  = renderPrimitive Triangles $ do
      color
      v₀
      v₁
      v₂
