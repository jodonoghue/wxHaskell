module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore

main :: IO ()
main  
  = start $
    do f <- frame [text := "Database browser"]
       p <- panel f []
       s1 <- splitterWindow p []

       dsns    <- dbGetDataSources
       dsnList <- singleListBox s1 False (reverse (map fst dsns))
                    [tooltip := "Data sources",
                     clientSize := sz 10 30]

       s2 <- splitterWindow s1 []
       tableList <- singleListBox s2 False [] [tooltip := "Tables"]
       tableView <- listCtrl s2 []

       set f [layout := container p $ margin 5 $ fill $ 
                        hsplit s1 5 80 (widget dsnList) 
                                      (hsplit s2 5 80 (widget tableList) (widget tableView))
             ,clientSize := sz 300 300
             ]
       
       set dsnList [on command := onDsnEvent f dsnList tableList tableView]
       set tableList [on command := onTableEvent f dsnList tableList tableView]
   where
      onDsnEvent f dsnList tableList tableView
        = do listCtrlDeleteAllItems tableView
             set tableView [columns := []]            
             set tableList [items := []]
             i    <- get dsnList selection
             dsn  <- get dsnList (item i)
             info <- dbGetDataSourceInfo dsn "" "" 
             set tableList [items := map tableName (reverse (dbTables info))]
          `catchDbError` \err -> errorDialog f ("Database '" ++ dbDataSource err ++ "'") (dbErrorMsg err)

      onTableEvent f dsnList tableList tableView
        = do listCtrlDeleteAllItems tableView
             dsnIdx   <- get dsnList selection
             dsn      <- get dsnList (item dsnIdx)
             tableIdx <- get tableList selection
             tname    <- get tableList (item tableIdx)
             tableInfo <- dbGetDataSourceTableInfo dsn tname "" ""
             let columnInfos = tableColumns tableInfo
             
             let headers = map headerFromColumn columnInfos

             set tableView [columns := headers]
             dbWithConnection dsn "" "" $ \db ->
              do items <- dbQuery db ("SELECT * FROM " ++ tname) 
                            (\row -> mapM (dbRowGetString row) (map columnName columnInfos))
                 mapM_ insertRow  (zip [0..] items)
             return ()
          `catchDbError` \err -> errorDialog f ("Database '" ++ dbDataSource err ++ "'") (dbErrorMsg err)
        where
          insertRow (idx,[])
            = return ()
          insertRow (idx,fields)
            = do listCtrlInsertItemWithLabel tableView idx (head fields) imageNone
                 mapM_ (\(i,name) -> listCtrlSetItem tableView idx i name imageNone) (zip [0..] fields)

          imageNone :: Int
          imageNone
            = (-1)

          headerFromColumn :: ColumnInfo -> (String,Align,Int)
          headerFromColumn info
            = (columnName info, AlignLeft, 10 + 6*headerWidth (max (columnSize info) (length (columnName info))))
    
          headerWidth n
            | n <= 2    = 2
            | n  > 15   = 15
            | otherwise = n
             