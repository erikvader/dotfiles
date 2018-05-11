module Erik.MyStuff (
  rotLastUp, rotLastDown, rotLast',
  rotUp, rotDown,
  onLayout,
  writeStd
  -- pointerDance
) where

import System.Directory (getHomeDirectory)
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.List (find)
import Control.Concurrent (threadDelay)
import XMonad.Actions.Warp
import Control.Monad
import System.Random

-- pointerDance (num of jumps) (delay in microseconds)
-- pointerDance :: Int -> Int -> X ()
-- pointerDance n t = do
--   pos <- warps
--   sequence_ $ tail $ (++[head sleeps]) . concat $ zipWith (\a b -> [a,b]) sleeps pos
--   where
--     warps :: X [X ()]
--     warps = do
--       ls <- io $ rposes n [] 0 >>= return . (++ [(0.5, 0.5)])
--       return $ map (uncurry warpToWindow) (map (\(a, b) -> (toRational a, toRational b)) ls)

--     sleeps = repeat (io $ threadDelay t)

--     rposes :: Int -> [(Double, Double)] -> Int -> IO [(Double, Double)]
--     rposes 0 l _ = return l
--     rposes n l a = do
--       x <- randomRIO (0.1, 0.9)
--       y <- randomRIO (0.1, 0.9)
--       let t = (x, y) in case l of
--                           [] -> rposes (n-1) [t] a
--                           (x:xs) | a >= 3 || good t x -> rposes (n-1) (t:x:xs) 0
--                                  | otherwise          -> rposes n (x:xs) (a + 1)

--     good (x1, y1) (x2, y2) = ((x1-x2) ** 2) + ((y1-y2) ** 2) < (100 ** 2)



rotLastUp :: X ()
rotLastUp = windows $ W.modify' (rotLast' rotUp)

rotLastDown :: X ()
rotLastDown = windows $ W.modify' (rotLast' rotDown)

-- rotate the lower windows in the stack (including focused)
-- with a given function (rotate direction)
rotLast' :: ([a] -> [a]) -> W.Stack a -> W.Stack a
rotLast' f (W.Stack t l r) = W.Stack t' l r'
  where
    (t':r') = f (t:r)

-- $generic
-- Generic list rotations such that @rotUp [1..4]@ is equivalent to
-- @[2,3,4,1]@ and @rotDown [1..4]@ to @[4,1,2,3]@. They both are
-- @id@ for null or singleton lists.
rotUp :: [a] -> [a]
rotUp   l = drop 1 l ++ take 1 l
rotDown :: [a] -> [a]
rotDown = reverse . rotUp . reverse

-- runs a X command(?) depending on the description of the current
-- layout. Useful for assigning layout specific keybinds. The string
-- "__DEFAULT__" is the default value and is run if no other
-- description is matched.
onLayout :: [(String, X a)] -> X a -> X a
onLayout xs def = do
  d <- description . W.layout . W.workspace . W.current <$> gets windowset
  case find ((== d) . fst) xs of
    Just (_, x) -> x
    Nothing -> def

-- writes to "stdout"
writeStd :: String -> IO ()
writeStd s = do
  home <- getHomeDirectory
  appendFile (home ++ "/.xmonad/stdout") (s ++ "\n")
