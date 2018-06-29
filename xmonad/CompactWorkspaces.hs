{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-missing-signatures #-}
module Erik.CompactWorkspaces (
  compactWorkspace,
  combinations, combinationsSorted
    ) where

import XMonad
import qualified Data.Set as S
import Data.List (elemIndex,sortOn,sort)

-- get all combinations in some order
combinations :: [a] -> [[a]]
combinations [] = []
combinations (x:xs) = [[x]] ++ combinations xs ++ [x:y | y <- combinations xs]

-- get all combinations in a nice sorted order
combinationsSorted :: Ord a => [a] -> [[a]]
combinationsSorted l = sortOn length $ sort $ combinations l

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
  let setOption ks = do
                       (t, s) <- io event
                       case () of
                         () | t == keyPress && elem s keys -> setOption (S.insert s ks)
                            | t == keyRelease && elem s (mods ++ keys) -> return ks
                            | otherwise -> setOption ks
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  pressed <- setOption $ S.singleton start
  io $ ungrabKeyboard d currentTime
  let mi = elemIndex pressed order
  ws <- XMonad.workspaces . config <$> ask
  mapM_ (windows . f . (!!) ws) mi

