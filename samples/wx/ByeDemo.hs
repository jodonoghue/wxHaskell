{--------------------------------------------------------------------------------
This program implements the "goodbye" demo as posted by John Meacham on
the Haskell GUI mailing list. The program is specified as:

  I propose a simple program which pops up a window saying 'Hello World'
  with a button saying 'Bye' which you click and it changes the message
  to 'Goodbye'. if you click the button again the program exits.

When the button is clicked the first time, it calls "bye". This function changes the
text of the text and installs another event handler on the button that closes the
main window.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX

main :: IO ()
main = start demo     -- "start" initializes the GUI.

demo :: IO ()
demo = do f <- frame        [text := "Bye!"]
          p <- panel f      []  -- keyboard navigation
          t <- staticText p [text := "Hello World"]
          b <- button     p [text := "Bye"]
          set b [on command := bye f t b]                      -- set event handler on button
          set f [layout := fill $ container p $ margin 10 $    -- layout controls in frame
                           column 5 [widget t, widget b]]      
          focusOn b
     where
       -- called on the first click, with the text, button, and frame as arguments.
       bye f t b
         = do set t [text := "Goodbye"]
              set b [on command := close f]   -- set a new event handler on the button