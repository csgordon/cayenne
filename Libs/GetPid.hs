module Libs.GetPid(getPid) where
import System.Posix

getPid :: IO Int
getPid = 
    do
	p <- getProcessID
	return (read (show p))
