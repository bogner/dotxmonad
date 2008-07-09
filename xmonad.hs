{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import XMonad hiding (keys,layoutHook,logHook,manageHook,modMask,startupHook,workspaces)
import qualified XMonad (keys,layoutHook,logHook,manageHook,modMask,startupHook,workspaces)
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLayout,ewmhDesktopsLogHook)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts,manageDocks,ToggleStruts(..))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.WorkspaceCompare (getSortByIndex, WorkspaceSort)

import Control.Applicative ((<$>))
import Control.Monad (filterM,liftM)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import qualified Data.Map as M

main = xmonad bogConfig

bogConfig = defaultConfig
            { XMonad.focusedBorderColor = "#5c888b"
            , XMonad.keys               = \c -> keys `M.union` XMonad.keys defaultConfig c
            , XMonad.layoutHook         = layoutHook
            , XMonad.logHook            = logHook
            , XMonad.manageHook         = manageHook
            , XMonad.modMask            = modMask
            , XMonad.mouseBindings      = mouse
            , XMonad.normalBorderColor  = "#dcdccc"
            , XMonad.startupHook        = startupHook
            , XMonad.terminal           = "urxvtcd"
            , XMonad.workspaces         = workspaces
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
       , ((modMask,               xK_w), kill)
       , ((modMask .|. shiftMask, xK_slash), spawn "todo-notify.sh")
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
             [ className =? "Emacs"           --> doF (shiftView "dev" . insertMaster)
             , resource  =? "Navigator"       --> doF (shiftView "web")
             , className =? "Pidgin"          --> doF (shiftView "com")
             , className =? "Thunderbird-bin" --> doF (shiftView "com")
             -- floats should always appear at the very top
             , floating                       --> doF insertMaster
             -- i like these to show up in the master area when using emacs
             , className =? "XDvi"            --> doF W.swapUp
             , className =? "GV"              --> doF W.swapUp
             , className =? "feh"             --> doFloat
             , className =? "Vncviewer"       --> doFloat
             , manageDocks
             ] <+> doF W.swapDown
    where
      floating = (ask >>= liftX . willFloat)
                 -- if we apply swapUp to the desktop, things move
                 -- around on restart, similarly for looking at
                 -- gnome-panel's calendar
                 <&&> (liftM not $ resource =? "desktop_window"
                       <||> resource =? "gnome-panel")

{- This is logic copied from XMonad.Operations.manage,
   since manageHook is called before windows are floated -}
willFloat w = withDisplay $ \d -> do
                sh <- io $ getWMNormalHints d w
                let isFixedSize = sh_min_size sh /= Nothing && sh_min_size sh == sh_max_size sh
                isTransient <- isJust <$> io (getTransientForHint d w)
                ws <- gets windowset
                return (isFixedSize || isTransient || isFloat w ws)

shiftView w = W.greedyView w . W.shift w

insertMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
insertMaster = W.modify' $ \c -> case c of
    W.Stack _ [] _  -> c    -- already master.
    W.Stack t ls rs -> W.Stack t [] (reverse ls ++ rs)

workspaces = ["web", "dev", "com"] ++ map show [4..9]

layoutHook =
    smartBorders $
    layoutHints $
    ewmhDesktopsLayout $ avoidStruts $
    tiled ||| bigTiled ||| Mirror tiled ||| PerRow ||| Full
        where
          tiled    = Tall nmaster delta (1/2)
          bigTiled = Tall nmaster delta (11/16)
          nmaster  = 1
          delta    = 3/100

----------------------------------------------------------------------
-- We set initial layouts on our workspaces. Perhaps we should use
-- LayoutCombinators to clean this up, but this gets the job done.
----------------------------------------------------------------------

startupHook = sequence_ $ zipWith workspaceLayout workspaces [ 4, 0, 3 ]

-- Set the layout on a workspace
workspaceLayout :: String -> Int -> X ()
workspaceLayout w l = do
  c <- W.tag . W.workspace . W.current <$> gets windowset
  windows (W.greedyView w)
  sendMessage' FirstLayout
  sequence_ . take l . repeat $ sendMessage' NextLayout
  windows (W.greedyView c)

-- like sendMessageWithNoRefresh but with sendMessage's type
sendMessage' :: Message a => a -> X ()
sendMessage' m = W.workspace . W.current <$> gets windowset >>=
                 sendMessageWithNoRefresh m

----------------------------------------------------------------------
-- Log Hook
----------------------------------------------------------------------

logHook = ewmhDesktopsLogHook >> fewerDesktops >> fadeInactiveLogHook 0xe0000000

-- redundant code, we actually just want to replace getSortByIndex in ewmhDesktopsLogHook

setNumberOfDesktops :: (Integral a) => a -> X ()
setNumberOfDesktops n = withDisplay $ \dpy -> do
    a <- getAtom "_NET_NUMBER_OF_DESKTOPS"
    c <- getAtom "CARDINAL"
    r <- asks theRoot
    io $ changeProperty32 dpy r a c propModeReplace [fromIntegral n]

fewerDesktops :: X ()
fewerDesktops = withWindowSet $ \s -> do
                  sort' <- getSortByIndex'
                  let ws = sort' $ W.workspaces s
                  setNumberOfDesktops (length ws)

-- End redundant code

-- Sort by index, dropping unused workspaces from the end of the list
getSortByIndex' :: X WorkspaceSort
getSortByIndex' = withWindowSet $ \s ->
                  getSortByIndex >>= \sort ->
                      return $ dropBoring s . sort

-- Drop empty and inactive workspaces from the end of the list
dropBoring :: Eq i => W.StackSet i l a s sd -> [W.Workspace i l a] -> [W.Workspace i l a]
        -- :: WindowSet -> [WindowSpace] -> [WindowSpace]
dropBoring s = reverse . dropWhile boring . reverse
    where boring (W.Workspace _ _ (Just _)) = False
          boring (W.Workspace i _ Nothing)  = i /= W.currentTag s

----------------------------------------------------------------------
-- PerRow, A layout which gives each independent application it's own
-- row of windows. This only groups consecutive windows of an
-- application, so that changing the order of windows always has an
-- effect. It might be better to group them no matter what order they
-- come in, but I haven't worked out a way to do that that doesn't
-- result in very awkward behaviour.
----------------------------------------------------------------------

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
