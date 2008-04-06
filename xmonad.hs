{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import XMonad hiding (keys,layoutHook,manageHook,modMask,workspaces)
import qualified XMonad (keys,layoutHook,manageHook,modMask,workspaces)
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLayout,ewmhDesktopsLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts,manageDocks,ToggleStruts(..))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise

import Control.Applicative ((<$>))
import Control.Monad (filterM,liftM)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import qualified Data.Map as M

main = xmonad bogConfig

bogConfig = defaultConfig
            { XMonad.keys          = \c -> keys `M.union` XMonad.keys defaultConfig c
            , XMonad.mouseBindings = mouse
            , XMonad.logHook       = ewmhDesktopsLogHook
            , XMonad.manageHook    = manageHook
            , XMonad.modMask       = modMask
            , XMonad.terminal      = "urxvtcd"
            , XMonad.workspaces    = workspaces
            , XMonad.layoutHook    = layoutHook
            }

modMask = mod4Mask -- Super

keys = M.fromList $
       [ ((modMask,               xK_p), runOrRaisePrompt xpConfig)
       , ((modMask .|. shiftMask, xK_p), shellPrompt xpConfig)
       , ((modMask,               xK_b), sendMessage ToggleStruts)
       -- alt-tab, for when others use my computer
       , ((mod1Mask,              xK_Tab), windows W.focusDown)
       -- since we have alt-tab, super-tab might as well cycle backwards
       , ((modMask,               xK_Tab), windows W.focusUp)
       ]

mouse (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, button1), (\w -> focus w >> float w))
    , ((modMask, button1),
       (\w -> focus w >> withFloat mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask .|. controlMask, button1),
       (\w -> focus w >> withFloat mouseResizeWindow w))
    ]

withFloat f w = gets windowset >>= \ws -> if (isFloat w ws)
                                          then f w
                                          else return ()
isFloat w ws = M.member w $ W.floating ws

-- TODO: if we can determine what window is at a given x y coord, then
{-
moveTiled w = whenX (isClient w) $ withDisplay $ \d -> do
                io $ raiseWindow d w -- we probably don't need this
                (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d w
                let ox = fromIntegral ox'
                    oy = fromIntegral oy'
                mouseDrag (\ex ey -> io $ swapWins (windowAt ex ey) (windowAt ox oy))
-}

xpConfig = defaultXPConfig
           { font        = "xft:Bitstream Vera Sans Mono:pixelsize=10"
           , bgColor     = "#3f3f3f"
           , fgColor     = "#dcdccc"
           , fgHLight    = "#3f3f3f"
           , bgHLight    = "#dcdccc"
           , borderColor = "#dcdccc"
           , position    = Top
           }

manageHook = composeAll
             [ className =? "Emacs"           --> doF (shiftView "dev" . W.swapMaster)
             , className =? "Firefox"         --> doF (shiftView "web")
             , className =? "Pidgin"          --> doF (shiftView "com")
             , className =? "Thunderbird-bin" --> doF (shiftView "com")
             -- floats need swapUp, otherwise they show up below other floats
             , floating                       --> doF W.swapUp
             , manageDocks
             ] <+> doF W.swapDown
    where
      floating = (ask >>= liftX . willFloat)
                 -- if we apply swapUp to the desktop, things move around on restart
                 <&&> (liftM not $ resource  =? "desktop_window")

{- This is logic copied from XMonad.Operations.manage,
   since manageHook is called before windows are floated -}
willFloat w = withDisplay $ \d -> do
                sh <- io $ getWMNormalHints d w
                let isFixedSize = sh_min_size sh /= Nothing && sh_min_size sh == sh_max_size sh
                isTransient <- isJust <$> io (getTransientForHint d w)
                ws <- gets windowset
                return (isFixedSize || isTransient || isFloat w ws)

shiftView w = W.greedyView w . W.shift w

workspaces = ["web", "dev", "com" ] ++ map show [4..9]

layoutHook =
    ewmhDesktopsLayout $ avoidStruts $ smartBorders $
    tiled ||| bigTiled ||| Mirror tiled ||| PerRow ||| Full
        where
          tiled    = Tall nmaster delta (1/2)
          bigTiled = Tall nmaster delta (11/16)
          nmaster  = 1
          delta    = 3/100

data PerRow a = PerRow deriving (Read, Show)

instance LayoutClass PerRow Window where
    doLayout PerRow r s = groupByM sameClass (W.integrate s) >>=
                          \ws -> return ((arrange r ws),Nothing)
    description _ = "PerRow"

-- span and groupBy lifted to monads, thanks to Cale on #haskell
spanM             :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
spanM p []         = return ([],[])
spanM p xs@(x:xs') = do v <- p x
                        if v then do (ys,zs) <- spanM p xs'
                                     return (x:ys,zs)
                             else return ([], xs)

groupByM           :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
groupByM eq []     = return []
groupByM eq (x:xs) = do (ys,zs) <- spanM (eq x) xs 
                        gs <- groupByM eq zs
                        return ((x:ys) : gs) 

sameClass w1 w2 = flip runQuery w1 $ getClass w1 >>= \q -> getClass w2 =? q
getClass w = liftX $ withDisplay $ \d -> fmap resClass $ io $ getClassHint d w

arrange      :: Eq a => Rectangle -> [[a]] -> [(a, Rectangle)]
arrange r ws = concat $ zipWith place (splitVertically (length ws) r) ws

place      :: Rectangle -> [a] -> [(a, Rectangle)]
place r ws = zip ws $ splitHorizontally (length ws) r
