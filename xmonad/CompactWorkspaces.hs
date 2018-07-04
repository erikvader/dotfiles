{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing#-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}
module Erik.CompactWorkspaces (
  compactWorkspaceCombinations, combinations,
  compactWorkspacePermutations, permutations,
  compactWorkspace
    ) where

import XMonad
import qualified Data.Set as S
import Data.List (elemIndex,sortOn,sort)

-- get all combinations of all lengths in some order
combinations_ :: [a] -> [[a]]
combinations_ [] = []
combinations_ (x:xs) = [[x]] ++ combinations_ xs ++ [x:y | y <- combinations_ xs]

-- get all permutations of all lengths in some order
permutations_ :: [a] -> [[a]]
permutations_ [] = []
permutations_ (x:xs) = [[x]] ++ permutations_ xs ++ concat [comb x y | y <- permutations_ xs]
  where
    comb :: a -> [a] -> [[a]]
    comb y [] = [[y]]
    comb y (x:xs) = (y:x:xs) : [x:z | z <- comb y xs]

-- sort output from combinations or permutations in a sensible way
niceSort :: Ord a => [[a]] -> [[a]]
niceSort = sortOn length . sort

-- get all combinations in a nice sorted order
combinations :: Ord a => [a] -> [[a]]
combinations = niceSort . combinations_

-- get all permutations in a nice sorted order
permutations :: Ord a => [a] -> [[a]]
permutations = niceSort . permutations_

class (Ord a, Eq (c a)) => Compact c a where
  -- an empty container to start with
  cempty :: c a

  -- a key has been pressed! record it
  -- ...or released, then record it by adding it to another container
  cadd :: a -> c a -> c a

  -- take a list of keys (from the order list) and convert it to a
  -- container so that cindex can use it to figure out what workspace
  -- should be selected
  cfromList :: [a] -> c a

  -- check if all pressed keys are equal to all released keys
  cequal :: c a -> c a -> Bool

  -- take the final key sequence and find it in a order list
  cindex :: c a -> [[a]] -> Maybe Int
  cindex e ls = elemIndex e (map cfromList ls)

-- for combinations
instance Compact S.Set KeySym where
  cempty = S.empty
  cadd = S.insert
  cfromList = S.fromList
  cequal = (==)

-- for permutations
instance Compact [] KeySym where
  cempty = []
  cadd x ls | elem x ls = ls
            | otherwise = x:ls
  cfromList = reverse
  cequal a b = S.fromList a == S.fromList b

-- compactWorkspace f order start mods keys
-- f is the function to run on the selected workspace
-- order is a list of sets with combinations/permutations of keysyms to specify wich button combination/permutation maps to which workspace. The first element maps to the first workspace and so on
-- start is the initial key that started this command. Should be the only element in a Compact container. This is what determines what behaviour should be used
-- mods is a list of modifier keys whose release events shall finish the command
-- keys is a list of the main keys. Their press events registers that key and their release event finished the command (be careful or repeating keys)
compactWorkspace :: Compact c KeySym => (WorkspaceId -> WindowSet -> WindowSet) -> [[KeySym]] -> c KeySym -> [KeySym] -> [KeySym] -> X ()
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
                         () | t == keyPress   && elem s keys -> setOption (cadd s ks) released
                            | t == keyRelease && elem s mods -> return ks
                            | t == keyRelease && elem s keys -> let r' = cadd s released
                                                                in if cequal ks r'
                                                                   then return ks
                                                                   else setOption ks r'
                            | otherwise -> setOption ks released
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  pressed <- setOption start cempty
  io $ ungrabKeyboard d currentTime
  let mi = cindex pressed order
  ws <- XMonad.workspaces . config <$> ask
  mapM_ (windows . f . (!!) ws) mi

-- use standard combinations order
compactWorkspaceCombinations :: (WorkspaceId -> WindowSet -> WindowSet) -> KeySym -> [KeySym] -> [KeySym] -> X ()
compactWorkspaceCombinations f start mods keys = compactWorkspace f (combinations keys) (S.singleton start) mods keys

-- use standard permutations order
compactWorkspacePermutations :: (WorkspaceId -> WindowSet -> WindowSet) -> KeySym -> [KeySym] -> [KeySym] -> X ()
compactWorkspacePermutations f start mods keys = compactWorkspace f (permutations keys) [start] mods keys
