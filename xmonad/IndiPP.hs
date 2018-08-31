{-# OPTIONS_GHC -W -fwarn-unused-imports -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveDataTypeable, TupleSections #-}
module Erik.IndiPP (
  indiPPStartupHook,
  indiPPEventHook,
  multiPP,
  indiPP
) where

import qualified XMonad.Util.ExtensibleState as XS
import System.Directory (getHomeDirectory)
import XMonad
import qualified XMonad.StackSet as W
import Data.List (find)
import Data.Monoid
import Data.Maybe (maybe,isNothing,catMaybes,fromMaybe,mapMaybe)
import Control.Monad(forM,mapM,when)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Graphics.X11.Xrandr
import Graphics.X11.Xinerama (xineramaQueryScreens)
import XMonad.Hooks.DynamicLog
import Data.Foldable (traverse_)

newtype IndiPPState = IndiPPState [(ScreenId, String)] deriving Typeable
instance ExtensionClass IndiPPState where
  initialValue = IndiPPState []

indiPPStartupHook :: X ()
indiPPStartupHook = do
  d <- asks display
  r <- asks theRoot
  io $ xrrSelectInput d r rrScreenChangeNotifyMask
  updateXineramaMapping

indiPPEventHook :: Event -> X All
-- indiPPEventHook e = trace (show e) >> return (All True)
indiPPEventHook RRScreenChangeNotifyEvent{} = updateXineramaMapping >> return (All True)
indiPPEventHook _ = return (All True)

updateXineramaMapping :: X ()
updateXineramaMapping = getXineramaMapping >>= \xm -> XS.put (IndiPPState xm)

getXineramaMapping :: X [(ScreenId, String)]
getXineramaMapping = do
  d <- asks display
  r <- asks theRoot
  physcrns <- io $ getXrandr d r
  mapMaybe (\(si, r) -> (\(s, _) -> (si, s)) <$> find ((==r) . snd) physcrns) <$> getXinerama

getXinerama :: X [(ScreenId, Rectangle)]
getXinerama = do
  ss <- W.screens <$> gets windowset
  return $ map (\s -> (W.screen s, screenRect $ W.screenDetail s)) ss

getXrandr :: Display -> Window -> IO [(String, Rectangle)]
getXrandr dis root = do
  mres <- xrrGetScreenResources dis root
  ml <- forM mres $ \res ->
          let outputs = xrr_sr_outputs res
          in (catMaybes <$> mapM (xrrGetOutputInfo dis res) outputs) >>= getRect res
  return $ fromMaybe [] ml
  where
    getRect :: XRRScreenResources -> [XRROutputInfo] -> IO [(String, Rectangle)]
    getRect res os = catMaybes <$> mapM f os
      where
        f :: XRROutputInfo -> IO (Maybe (String, Rectangle))
        f o | xrr_oi_crtc o `elem` xrr_oi_crtcs o = crtcToRect (xrr_oi_crtc o) >>= \mr -> return (mr >>= \r -> return (xrr_oi_name o, r))
            | otherwise = return Nothing

        crtcToRect :: RRCrtc -> IO (Maybe Rectangle)
        crtcToRect c = xrrGetCrtcInfo dis res c >>= \ci -> return ((\XRRCrtcInfo {xrr_ci_x=x, xrr_ci_y=y, xrr_ci_width=w, xrr_ci_height=h} -> Rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)) <$> ci)

multiPP :: PP -- ^ The PP to use if the screen is focused
        -> PP -- ^ The PP to use otherwise
        -> (String -> PP -> X PP) -- ^ A function that prepares a PP to be used on a certain screen
        -> X ()
multiPP = multiPPFormat dynamicLogWithPP

multiPPFormat :: (PP -> X ()) -> PP -> PP -> (String -> PP -> X PP) -> X ()
multiPPFormat dynlStr focusPP unfocusPP ppmod = do
  (IndiPPState l) <- XS.get
  multiPP' l dynlStr focusPP unfocusPP ppmod

multiPP' :: [(ScreenId, String)] -> (PP -> X ()) -> PP -> PP -> (String -> PP -> X PP) -> X ()
multiPP' screens dynlStr focusPP unfocusPP ppmod = do
  st <- get
  let pickPP :: (WorkspaceId, String) -> WriterT (Last XState) X ()
      pickPP (ws,output) = do
        let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset st
        put st{ windowset = W.view ws $ windowset st }
        out <- lift $ dynlStr =<< ppmod output (if isFoc then focusPP else unfocusPP)
        when isFoc $ get >>= tell . Last . Just
        return out
  traverse_ put . getLast
    =<< execWriterT . mapM pickPP . catMaybes
    =<< mapM (\(s,o) -> screenWorkspace s >>= (\w -> return ((,o) <$> w))) screens

indiPP :: XConfig a -> XConfig a
indiPP c = c {startupHook = startupHook c <+> indiPPStartupHook, handleEventHook = handleEventHook c <+> indiPPEventHook}


