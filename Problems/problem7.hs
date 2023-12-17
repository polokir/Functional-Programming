import Graphics.UI.GLUT
import Data.IORef
import System.Random

data Ball = Ball {
  ballPos :: (GLfloat, GLfloat),
  ballVel :: (GLfloat, GLfloat),
  ballRadius :: GLfloat,
  ballColor :: Color3 GLfloat
}

createBall :: GLfloat -> IO Ball
createBall radius = do
  x <- randomRIO (-1.0, 1.0)
  y <- randomRIO (-1.0, 1.0)
  vx <- randomRIO (-0.1, 0.1)
  vy <- randomRIO (-0.1, 0.1)
  r <- randomRIO (0.0, 1.0)
  g <- randomRIO (0.0, 1.0)
  b <- randomRIO (0.0, 1.0)
  return $ Ball (x, y) (vx, vy) radius (Color3 r g b)

display :: IORef [Ball] -> DisplayCallback
display ballsRef = do
  balls <- readIORef ballsRef
  clear [ColorBuffer]
  mapM_ drawBall balls
  flush
  where
    drawBall (Ball (x, y) _ radius color) = do
      preservingMatrix $ do
        translate $ Vector3 x y 0
        color color
        renderObject Solid $ Sphere' radius 32 32

update :: IORef [Ball] -> IO ()
update ballsRef = do
  balls <- readIORef ballsRef
  let updateBall (Ball (x, y) (vx, vy) radius color) =
        let (x', y') = (x + vx, y + vy)
            (vx', vy') = if x' > 1.0 - radius || x' < -1.0 + radius
                          then (-vx, vy)
                          else (vx, vy)
                         if y' > 1.0 - radius || y' < -1.0 + radius
                          then (vx, -vy)
                          else (vx, vy)
        in Ball (x', y') (vx', vy') radius color
  writeIORef ballsRef $ map updateBall balls

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (fromIntegral (width size) / fromIntegral (height size)) 0.1 100.0
  matrixMode $= Modelview 0

idle :: IORef [Ball] -> IdleCallback
idle ballsRef = do
  update ballsRef
  postRedisplay Nothing

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 640 480
  initialWindowPosition $= Position 100 100
  _window <- createWindow "Bouncing Balls"
  ballsRef <- newIORef []
  forM_ [1..10] $ \_ -> do
    ball <- createBall 0.1
    modifyIORef ballsRef (ball:)
  displayCallback $= display ballsRef
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle ballsRef)
  clearColor $= Color4 0 0 0 0
  mainLoop