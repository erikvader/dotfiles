module Erik.MyStuff (
  rotLastUp, rotLastDown, rotLast',
  rotUp, rotDown
) where

import XMonad
import qualified XMonad.StackSet as W

rotLastUp :: X()
rotLastUp = windows $ W.modify' (rotLast' rotUp)

rotLastDown :: X()
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
