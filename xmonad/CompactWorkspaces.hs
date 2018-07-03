{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing#-}
module Erik.CompactWorkspaces (
  compactWorkspace,
  combinations, combinationsSorted
    ) where

import XMonad
import qualified Data.Set as S
import Data.List (elemIndex,sortOn,sort)

-- get all combinations of all lengths in some order
combinations :: [a] -> [[a]]
combinations [] = []
combinations (x:xs) = [[x]] ++ combinations xs ++ [x:y | y <- combinations xs]

-- get all permutations of all lengths in some order
permutations :: [a] -> [[a]]
permutations [] = []
permutations (x:xs) = [[x]] ++ permutations xs ++ (concat [comb x y | y <- permutations xs])
  where
    comb :: a -> [a] -> [[a]]
    comb y [] = [[y]]
    comb y (x:xs) = [y:x:xs] ++ [x:z | z <- comb y xs]

-- sort output from combinations or permutations in a sensible way
niceSort :: Ord a => [[a]] -> [[a]]
niceSort = sortOn length . sort

-- get all combinations in a nice sorted order
combinationsSorted :: Ord a => [a] -> [[a]]
combinationsSorted = niceSort . combinations

-- compactWorkspace f order start mods keys
-- access 2^n workspaces with n keys
-- f is the function to run on the selected workspace
-- order is a list of sets with combinations of keysyms to specify wich button combination maps to which workspace. The first element maps to the first workspace and so on
-- start is the initial key that started this command
-- mods is a list of modifier keys whose release events shall finish the command
-- keys is a list of the main keys. Their press events registers that key and their release event finished the command (be careful or repeating keys)
compactWorkspace :: (WorkspaceId -> WindowSet -> WindowSet) -> [S.Set KeySym] -> KeySym -> [KeySym] -> [KeySym] -> X ()
compactWorkspace f order start mods keys = do
  XConf {theRoot = root, display = d} <- ask
  let event = allocaXEvent $ \p -> do
                maskEvent d (keyPressMask .|. keyReleaseMask) p
                KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                s <- keycodeToKeysym d c 0
                return (t, s)
  let setOption ks released = do
                       (t, s) <- io event
                       case () of
                         () | t == keyPress   && elem s keys -> setOption (S.insert s ks) released
                            | t == keyRelease && elem s mods -> return ks
                            | t == keyRelease && elem s keys -> let r' = S.insert s released
                                                                in if ks == r'
                                                                   then return ks
                                                                   else setOption ks r'
                            | otherwise -> setOption ks released
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  pressed <- setOption (S.singleton start) S.empty
  io $ ungrabKeyboard d currentTime
  let mi = elemIndex pressed order
  ws <- XMonad.workspaces . config <$> ask
  mapM_ (windows . f . (!!) ws) mi

