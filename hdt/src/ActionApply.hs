

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ActionApply where

import HdtTypes
import CmdLineOpts


execApply :: MyOptions -> IO ()
execApply opts@ModeApply{..} = do
    putStrLn $ "Applying!!"
