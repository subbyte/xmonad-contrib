{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.BorderPerWindow
-- Description :  Set border width for each window in all layouts.
-- Copyright   :  Xiaokui Shu <subbyte@gmail.com>
-- License     :  BSD (see LICENSE)
--
-- Maintainer  :  <subbyte@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Want to customize border width, for each window on all layouts? Want
-- specific window have no border on all layouts? Try this.
-----------------------------------------------------------------------------

module XMonad.Hooks.BorderPerWindow ( -- * Usage
                                      -- $usage

                                      -- * Design Considerations
                                      -- $design

                                      -- * Why Not Something Simpler?
                                      -- $options
                                      defineWindowBorder
                                    , setBorderPerWindow
                                    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import Graphics.X11.Xlib (Dimension)

-- $usage
--
-- 1. Specify which window to customize border in 'manageHook' of the config:
--
-- > myManageHook :: ManageHook
-- > myManageHook = composeAll
-- >     [ className =? "firefox"  --> defineWindowBorder 0
-- >     , className =? "Chromium" --> defineWindowBorder 0
-- >     , isDialog                --> defineWindowBorder 8
-- >     ]
--
-- 2. Execute the border setter at layout handling hook:
--
-- > myLayoutHook = setBorderPerWindow
-- >              $ Tall 1 (3/100) (1/2) ||| Full ||| Grid ||| ...
--
-- 3. Add the two hooks in your config:
--
-- > myConfig = def
-- >     { ...
-- >     , layoutHook = myLayoutHook
-- >     , manageHook = myManageHook
-- >     , ...
-- >     }

-- $design
--
-- 1. Keep it simple. Since the extension does not aim to change border setting
--    when layout changes, only execute the border setting function once to
--    avoid potential window flashing/jumping/scaling.
--
-- 2. The 'ManageHook' eDSL is a nice language for specifying windows. Let's
--    build on top of it and use it to specify window to define border.

-- $options
--
-- 1. Just setting window-specific border in 'manageHook' does not work, since
--    XMonad sets window border after 'ManageHook's are applied.
--
-- 2. 'XMonad.Layout.NoBorders' provides a way to set zero-width window border
--    not permanently, e.g., need to recover border when layout changes, which
--    results in complexity, e.g., need to set border every time a layout is
--    rendered, and has a window jumping effect when switching between
--    workspaces: for a zero-width border window, the border (global config) is
--    temporarily drawn and then removed; all pixels of the window is scaled
--    during the add-n-remove procedure, which could be noticeable.
--
-- 3. 'XMonad.Layout.VoidBorders' allows one to choose layout but not window.

-- | The XMonad message that transfers the window border spec from the window
-- creation event to the layout handling function.
-- Note that XMonad reads config and draws window border after 'ManageHook's
-- are applied. It does not work if we set the border in a 'ManageHook'
-- (hooking window creation). So we choose to send a message in
-- 'defineWindowBorder' function to a later stage, e.g., layout handling, to
-- set the window border.
data WindowBorderSpec
    = WindowBorderSpec Window Dimension deriving (Read, Show)

instance Message WindowBorderSpec

-- | The function to specify border width in a 'ManageHook'.
defineWindowBorder :: Dimension -> ManageHook
defineWindowBorder bw = do
    w <- ask
    liftX . broadcastMessage $ WindowBorderSpec w bw
    idHook

-- | The core XMonad 'LayoutModifier' type for this extension. This will be
-- used by our layout modifier function 'setBorderPerWindow' to execute the
-- procedure of setting border for each window according to the
-- 'WindowBorderSpecMessage' message received for that window.
data BorderPerWindow a
    = BorderPerWindow (Maybe WindowBorderSpec) deriving (Read, Show)

instance LayoutModifier BorderPerWindow a where
    modifierDescription = const "BorderPerWindow"

    hook (BorderPerWindow Nothing) =
        return ()

    hook (BorderPerWindow (Just (WindowBorderSpec w bw))) =
        withDisplay $ \d -> io $ setWindowBorderWidth d w bw

    pureMess (BorderPerWindow _) msg
        | Just spec@WindowBorderSpec{} <- fromMessage msg =
            Just . BorderPerWindow $ Just spec
        | otherwise                                       =
            Nothing

-- | The function to modify layouts in 'layoutHook'.
setBorderPerWindow :: l Window -> ModifiedLayout BorderPerWindow l Window
setBorderPerWindow = ModifiedLayout (BorderPerWindow Nothing)
