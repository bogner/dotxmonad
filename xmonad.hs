{-# LANGUAGE FlexibleInstances #-}
import XMonad hiding (keys,layoutHook,logHook,manageHook,modMask,startupHook,workspaces)
import qualified XMonad (keys,layoutHook,logHook,manageHook,modMask,startupHook,workspaces)
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLayout,ewmhDesktopsLogHook)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts,manageDocks,ToggleStruts(..))
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerRow (PerRow (..))
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.WorkspaceCompare (getSortByIndex, WorkspaceSort)

import Prelude hiding (catch)

import Control.Applicative ((<$>))
import Control.Exception (bracket, catch)
import Control.Monad (filterM,liftM)
import Control.Monad.State (when)
import Data.Bits ((.|.))
import Data.List (insertBy)
import Data.Maybe (isJust,fromMaybe)
import Data.Monoid (Endo(..))
import qualified Data.Map as M
import Text.Regex.Posix ((=~))

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
            , XMonad.terminal           = "dterm.sh"
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
       , ((modMask,               xK_grave), windows viewPrev)
       , ((modMask,               xK_w), kill)
       , ((modMask .|. shiftMask, xK_slash), spawn "todo-notify.sh")
       ]

viewPrev :: W.StackSet i l a s sd -> W.StackSet i l a s sd
--viewHidden = W.view . head . W.hidden
viewPrev s = s { W.current = (W.current s) { W.workspace = head (W.hidden s) }
               , W.hidden = W.workspace (W.current s) : tail (W.hidden s) }

mouse (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, button1), (\w -> focus w >> float w))
    , ( (modMask, button1)
      , (\w -> whenFloat w (focus w >> mouseMoveWindow w
                                    >> windows insertMaster))
      )
    , ( (modMask, button2)
      , (\w -> focus w >> windows insertMaster)
      )
    , ( (modMask .|. controlMask, button1)
      , (\w -> whenFloat w (focus w >> mouseResizeWindow w
                                    >> windows insertMaster))
      )
    ]

whenFloat :: Window -> X () -> X ()
whenFloat w f = isFloat w >>= \b -> when b f

isFloat :: Window -> X Bool
isFloat w = gets windowset >>= \ws -> return (M.member w $ W.floating ws)

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
             , className =? "Pidgin"          --> doF (W.shift "com")
             , className =? "Thunderbird-bin" --> doF (shiftView "com")
             -- floats should always appear at the very top
             , floating                       --> doF insertMaster
             -- i like these to show up in the master area when using emacs
             , className =? "XDvi"            --> doF W.swapUp
             , className =? "GV"              --> doF W.swapUp
             -- apps that are bad at tiling
             , className =? "feh"             --> doFloat
             , className =? "Gitk"            --> doFloat
             , className =? "Meld"            --> doFloat
             , className =? "Kompare"         --> doFloat
             -- bitkeeper's dialogs have stupid names
             , className ~? "(Diff|Rev)tool"  --> doFloat
             , className ~? "Toplevel"        --> doFloat
             --
             , className =? "Mbrowse"         --> (doFloat <+> doF insertMaster)
             --
             , title ~? "\"\\w+@\\w+ \\(0x[0-9]{2}\\)\""
                         --> insertSorted
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
                f <- isFloat w
                return (isFixedSize || isTransient || f)

shiftView w = W.greedyView w . W.shift w

insertMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
insertMaster = W.modify' $ \c -> case c of
    W.Stack _ [] _  -> c    -- already master.
    W.Stack t ls rs -> W.Stack t [] (reverse ls ++ rs)

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    otherwise           -> c

insertSorted :: ManageHook
insertSorted = do
  t <- ask >>= titleOf
  io $ putStrLn $ "new window: "++t
  s <- liftX $ gets windowset
  let ws = W.allWindows s
  ts <- mapM (titleOf) ws
  return $ Endo $ insertSorted' t (zip ws ts)

insertSorted' :: String -> [(Window, String)] -> (WindowSet -> WindowSet)
insertSorted' t m = W.modify' f
    where f c@(W.Stack w ls rs) = W.Stack w (reverse ls') rs'
              where (ls', (r:rs')) = span (/=w) ws
                    ws = insertBy (\_ y -> compare t (name y)) w .
                         filter (/=w) $ W.integrate c
                    name x = fromMaybe (show x) . flip lookup m $ x

-- | Return the title of a window.
titleOf :: Window -> Query String
titleOf w = liftX $ do
    d <- asks display
    let
        getProp =
            (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `catch` \_ -> getTextProperty d w wM_NAME
        extract = fmap head . wcTextPropertyToTextList d
    io $ bracket getProp (xFree . tp_value) extract `catch` \_ -> return ""

-- | @q ~? x@. if the result of @q@ matches the Regex @x@, return 'True'.
q ~? x = fmap (=~ x) q

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
          delta    = 1/200

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
