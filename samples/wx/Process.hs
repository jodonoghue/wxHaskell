module Main where

import Graphics.UI.WXH
import Graphics.UI.WX

main :: IO ()
main
  = start gui



gui :: IO ()
gui
  = do f      <- frame [text := "Process test"]
       p      <- panel f []                       -- panel for tab-management etc.
       input  <- comboBox p False ["cmd"] [style :~ \stl -> stl .+. wxTE_PROCESS_ENTER]
       output <- textCtrlRich p WrapNone  [bgcolor := black, textColor := red, font := fontFixed{ _fontSize = 12 }]
       stop   <- button p                 [text := "kill", enable := False]
       focusOn input
       textCtrlSetEditable output False
       set f [layout := container p $
                        margin 10 $ column 5 [fill (widget output)
                                             ,row 5 [hfill (widget input), widget stop]]
             ,clientSize  := sz 600 400
             ]

       let message txt = appendText output txt
       set input [on keyboard := onEnter (startProcess f input stop message)]
       return ()
  where
    startProcess f input stop message
      = do txt <- get input text
           appendText input txt
           (send,process,pid) <- processExecAsync f txt 256
                                  (onEndProcess f input stop message) (onReceive message) (onReceive message)
           let sendLn txt = send (txt ++ "\n")
           if (pid /= 0)
            then do message ("-- start process: '" ++ txt ++ "' --\n")
                    set input [on keyboard := onEnter (sendCommand input sendLn)]
                    set stop  [enable := True, on command  := unitIO (kill pid wxSIGKILL)]
            else return ()

    sendCommand input send
      = do txt <- get input text
           count <- comboBoxGetCount input
           appendText input txt
           set input [selection := count]
           send txt


    onEndProcess f input stop message exitcode
      = do message ("\n-- process ended with exitcode " ++ show exitcode ++ " --\n")
           set input [on keyboard := onEnter (startProcess f input stop message)]
           set stop  [enable := False, on command  := return ()]

    onEnter io (EventKey key mod pt)
      | key == KeyReturn  = io
      | otherwise         = propagateEvent


    onReceive message txt streamStatus
      = message txt
