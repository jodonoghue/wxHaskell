import Graphics.UI.WX
import Graphics.UI.WXCore
import Test.HUnit

main :: IO ()
main = start $ do

  f <- frame []
  p <- panel f []
  t <- treeCtrl p []

  root <- treeCtrlAddRoot t "root" (-1) (-1) objectNull
  treeCtrlAppendItem t root "item1" (-1) (-1) objectNull
  treeCtrlAppendItem t root "item2" (-1) (-1) objectNull
  treeCtrlAppendItem t root "item3" (-1) (-1) objectNull
  treeCtrlExpand t root

  cs <- treeCtrlGetChildren t root
  runTestTT $ TestCase $ assertEqual "tree children" 3 (length cs)
  close f
