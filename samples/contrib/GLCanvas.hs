{-
   Author :  shelarcy 2004
   Advised by: Sean Seefried

   Adapted from: BezCurve.hs
   By: (c) Sven Panne 2003 <sven_panne@yahoo.com>
   
   "BezCurve.hs (adapted from fog.c which is (c) Silicon Graphics, Inc)
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE"

   This program renders a lighted, filled Bezier surface, using two-dimensional
   evaluators.
-}

-- ghci -package wx -package OpenGL

module Main
where
    
import Data.List ( transpose )
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL
-- Many code and Type are ambiguous, so we must qualify names.
import qualified Graphics.UI.WX as WX
import qualified Graphics.Rendering.OpenGL as GL

main = start gui


defaultWidth  = 320
defaultHeight = 200

gui = do
   f <- frame [ text := "Simple OpenGL" ]
   glCanvas <- glCanvasCreateEx f 0 (Rect 0 0 defaultWidth defaultHeight)
        0 "GLCanvas" [GL_RGBA] nullPalette
   let glWidgetLayout = (fill . widget) glCanvas
   WX.set f [ layout := glWidgetLayout
            , on paint := paintGL
            ]


convWG (WX.Size w h) = (GL.Size (convInt32  w) (convInt32  h))
convInt32 = fromInteger . toInteger

paintGL :: DC() -> WX.Rect -> IO ()
paintGL dc rect = do
   -- write your commands to do OpenGL things here.
   myInit
   reshape $ convWG $ rectSize rect
   -- Or not reshape the size.
   -- reshape (GL.Size 320 200)
   display
   return ()


ctrlPoints :: [[Vertex3 GLfloat]]
ctrlPoints = [
   [ Vertex3 (-1.5) (-1.5)   4.0,  Vertex3 (-0.5) (-1.5)   2.0,
     Vertex3   0.5  (-1.5) (-1.0), Vertex3   1.5  (-1.5)   2.0 ],
   [ Vertex3 (-1.5) (-0.5)   1.0,  Vertex3 (-0.5) (-0.5)   3.0,
     Vertex3   0.5  (-0.5)   0.0,  Vertex3   1.5  (-0.5) (-1.0) ],
   [ Vertex3 (-1.5)   0.5    4.0,  Vertex3 (-0.5)   0.5    0.0,
     Vertex3   0.5    0.5    3.0,  Vertex3   1.5    0.5    4.0 ],
   [ Vertex3 (-1.5)   1.5  (-2.0), Vertex3 (-0.5)   1.5  (-2.0),
     Vertex3   0.5    1.5    0.0,  Vertex3   1.5    1.5  (-1.0) ]]

initlights :: IO ()
initlights = do
   lighting $= Enabled
   light (Light 0) $= Enabled

   ambient  (Light 0) $= Color4 0.2 0.2 0.2 1.0
   GL.position (Light 0) $= Vertex4 0 0 2 1

   materialDiffuse   Front $= Color4 0.6 0.6 0.6 1.0
   materialSpecular  Front $= Color4 1.0 1.0 1.0 1.0
   materialShininess Front $= 50

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   depthFunc $= Just Less
   m <- newMap2 (0, 1) (0, 1) (transpose ctrlPoints)
   map2 $= Just (m :: GLmap2 Vertex3 GLfloat)
   autoNormal $= Enabled
   mapGrid2 $= ((20, (0, 1)), (20, (0, 1 :: GLfloat)))
   initlights  -- for lighted version only

display = do
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      rotate (85 :: GLfloat) (Vector3 1 1 1)
      evalMesh2 Fill (0, 20) (0, 20)
   flush

reshape size@(GL.Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-4.0) 4.0 (-4.0*hf/wf) (4.0*hf/wf) (-4.0) 4.0
      else ortho (-4.0*wf/hf) (4.0*wf/hf) (-4.0) 4.0 (-4.0) 4.0
   matrixMode $= Modelview 0
   loadIdentity

