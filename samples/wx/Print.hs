module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do -- create a global that holds the printer dialog data
       pdata  <- printDialogDataCreateDefault
  
       -- the application frame
       f      <- frame         [text := "Print demo", clientSize := sz 300 200]                               

       -- create file menu  
       file   <- menuPane      [text := "&File"]
       mprint   <- menuItem file [text := "&Print", help := "Print a test", on command := print pdata f]
       mpreview <- menuItem file [text := "Print preview", help := "Print preview", on command := preview pdata f]
       msetup   <- menuItem file [text := "Print setup", help := "Setup the printer", on command := setup pdata f]
       quit   <- menuQuit file [help := "Quit the demo", on command := close f]

       -- create statusbar field
       status <- statusField   [text := "Welcome to wxHaskell"]

       -- set the statusbar and menubar
       set f [ statusBar := [status]
             , menuBar   := [file]
             ]

  where
    print :: PrintDialogData a -> Frame b -> IO ()
    print printDialogData f
      = do printer      <- printerCreate printDialogData
           printout     <- wxcPrintoutCreate "Print demo"
           printOutOnPrint printout (onprint printout)
           printerPrint printer f printout True {- show printer setup? -}
           objectDelete printout
           objectDelete printer

    onprint printOut ev
      = case ev of
          PrintPrepare
            -> wxcPrintoutSetPageLimits printOut 1 2 1 2
          PrintPage cancel dc n
            -> drawText dc ("Test Page " ++ show n) (point 100 100) []
          _ -> return ()

    preview printDialogData f
      = do printout1 <- wxcPrintoutCreate "Print demo (preview)"
           printout2 <- wxcPrintoutCreate "Print demo (printer)"
           printOutOnPrint printout1 (onprint printout1)
           printOutOnPrint printout2 (onprint printout2)
           wxcPrintoutSetPageLimits printout1 1 2 1 2
           wxcPrintoutSetPageLimits printout2 1 2 1 2
           preview      <- printPreviewCreateFromDialogData printout1 printout2 printDialogData
           previewFrame <- previewFrameCreate preview f "Print preview" rectNull frameDefaultStyle "Name"
           previewFrameInitialize previewFrame
           set previewFrame [visible := True]
          
           
    setup :: PrintDialogData b -> Frame a -> IO ()
    setup printDialogData f
      = do printData    <- printDialogDataGetPrintData printDialogData
           pageSetupData<- pageSetupDialogDataCreateFromData printData
           pageSetup    <- pageSetupDialogCreate f pageSetupData
           dialogShowModal pageSetup
           printData'   <- pageSetupDialogDataGetPrintData pageSetupData
                           -- pageSetupDialogGetPageSetupData pageSetup >>= pageSetupDialogDataGetPrintData
           printDialogDataSetPrintData printDialogData printData'
           objectDelete pageSetup
