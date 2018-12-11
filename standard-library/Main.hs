{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- uses the Gtk module
module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk

main :: IO ()
main = do 
    Gtk.init Nothing

    win <- new Gtk.Window [#title :="HelloWorld"]
    on win #destroy Gtk.mainQuit
    #showAll win

    Gtk.main

