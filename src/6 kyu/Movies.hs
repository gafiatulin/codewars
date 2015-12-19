-- I Liked the SQL Better...
-- http://www.codewars.com/kata/53d2c97d7152a59b64001033/

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Movies where

import Database.Persist.Sqlite (runSqlite, runMigrationSilent , insertMany_)
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Movies
    name String
    year Int
    rating Int
    deriving Show
|]

mkMoviesDB :: IO ()
mkMoviesDB = runSqlite "/tmp/movies.db" $ do
  runMigrationSilent migrateTables
  insertMany_ [ Movies "Rise of the Planet of the Apes" 2011 77, 
                Movies "Dawn of the Planet of the Apes" 2014 91, 
                Movies "Alien" 1979 97, 
                Movies "Aliens" 1986 98, 
                Movies "Mad Max" 1979 95, 
                Movies "Mad Max 2: The Road Warrior" 1981 100 ]
