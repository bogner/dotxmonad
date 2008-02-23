import XMonad hiding
    (workspaces,manageHook,numlockMask,keys,logHook,borderWidth,mouseBindings
    ,defaultGaps,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor)
import qualified XMonad
    (workspaces,manageHook,numlockMask,keys,logHook,borderWidth,mouseBindings
    ,defaultGaps,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor)
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Actions.WindowGo
import XMonad.Util.Run
import Data.Maybe
import Control.Monad

import qualified Data.Map as M
import Data.Bits ((.|.))

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
           , fgHLight    = "#94bff3"
--           , bgHLight    = "#dcdccc"
           , borderColor = "#dcdccc"
           , position    = Top
           }

manageHook = composeAll
             [ className =? "Emacs"           --> doF (shiftView "dev")
             , className =? "Firefox-bin"     --> doF (shiftView "web")
             , className =? "Pidgin"          --> doF (shiftView "com")
             , className =? "Thunderbird-bin" --> doF (shiftView "com")
             , (ask >>= \w -> liftX $ withWindowSet $ \ws -> return $ isFloat w ws) --> doF W.swapUp
             , manageDocks
             ] <+> doF W.swapDown

shiftView w = W.greedyView w . W.shift w

workspaces = ["web", "dev", "com" ] ++ map show [4..9]

layoutHook =
    avoidStruts $
    tiled ||| bigTiled ||| Mirror tiled
              ||| Mirror Grid ||| noBorders Full
        where
          tiled    = Tall nmaster delta (1/2)
          bigTiled = Tall nmaster delta (11/16)
          nmaster  = 1
          delta    = 3/100

data RunOrRaisePrompt = RRP
instance XPrompt RunOrRaisePrompt where
    showXPrompt RRP = "Run or Raise: "

runOrRaisePrompt :: XPConfig -> X ()
runOrRaisePrompt c = do cmds <- io $ getCommands
                        mkXPrompt RRP c (getShellCompl cmds) action
    where action = uncurry runOrRaise . getTarget
          getTarget x = (x,isApp x)

isApp :: String -> Query Bool
isApp "firefox" = className =? "Firefox-bin"
isApp "thunderbird" = className =? "Thunderbird-bin"
isApp x = liftM2 (==) pid $ pidof x

pidof :: String -> Query Int
pidof x = io $ (runProcessWithInput "pidof" [x] [] >>= readIO) `catch` (\e -> return $ 0)

pid :: Query Int
pid = ask >>= (\w -> liftX $ withDisplay $ \d -> getPID d w)
    where getPID d w = getAtom "_NET_WM_PID" >>= \a -> io $
                       getWindowProperty32 d a w >>= return . getPID'
          getPID' (Just (x:xs)) = fromIntegral x
          getPID' (Just [])     = -1
          getPID' (Nothing)     = -1
