{-# LANGUAGE DeriveDataTypeable, TupleSections #-}
module Erik.IndiPP (
  indiPPStartupHook,
  indiPPEventHook,
  multiPP,
  indiPP
) where

import qualified XMonad.Util.ExtensibleState as XS
import XMonad
import qualified XMonad.StackSet as W
import Data.List (find)
import Data.Monoid
import Data.Maybe (catMaybes,fromMaybe,mapMaybe)
import Control.Monad(forM,when)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Graphics.X11.Xrandr
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
  root <- asks theRoot
  physcrns <- io $ getXrandr d root
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
          in mapM (xrrGetOutputInfo dis res) outputs >>= getRect res . catMaybes
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

multiPP :: (String -> Bool -> X PP) -- ^ A function that prepares a PP to be used on a certain screen
        -> X ()
multiPP = multiPPFormat dynamicLogWithPP

multiPPFormat :: (PP -> X ()) -> (String -> Bool -> X PP) -> X ()
multiPPFormat dynlStr ppmod = do
  (IndiPPState l) <- XS.get
  multiPP' l dynlStr ppmod

multiPP' :: [(ScreenId, String)] -> (PP -> X ()) -> (String -> Bool -> X PP) -> X ()
multiPP' screens dynlStr ppmod = do
  st <- get
  let pickPP :: (WorkspaceId, String) -> WriterT (Last XState) X ()
      pickPP (ws,output) = do
        let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset st
        put st{ windowset = W.view ws $ windowset st }
        out <- lift $ dynlStr =<< ppmod output isFoc
        when isFoc $ get >>= tell . Last . Just
        return out
  traverse_ put . getLast
    =<< execWriterT . mapM pickPP . catMaybes
    =<< mapM (\(s,o) -> screenWorkspace s >>= (\w -> return ((,o) <$> w))) screens

-- adds format and other hooks to the right places
-- This also runs format on the startupHook
indiPP :: X () -> XConfig a -> XConfig a
indiPP format c = c {
  startupHook = indiPPStartupHook <+> format <+> startupHook c,
  logHook = logHook c <+> format,
  handleEventHook = handleEventHook c <+> indiPPEventHook
  }


