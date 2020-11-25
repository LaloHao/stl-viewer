module STL where

import Graphics.Rendering.OpenGL hiding (Vertex, Color)

normal3f, vertex3f, color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
normal3f x y z = normal $ Normal3 x y (z :: GLfloat)
color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

-- type Vertex = IO GLfloat
-- type Color  = IO GLfloat
type Vertex = IO ()
type Color  = IO ()

-- drawTriangle :: Color -> Vertex -> Vertex -> Vertex -> IO ()
-- drawTriangle color v₀ v₁ v₂
drawTriangle :: Vertex -> Vertex -> Vertex -> IO ()
drawTriangle v₀ v₁ v₂
  = renderPrimitive Triangles $ do
      v₀
      v₁
      v₂
      return ()
