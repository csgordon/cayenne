-- Copyright (c) 1982-1999 Lennart Augustsson, Thomas Johnsson
-- See LICENSE for the full license.
--
module Libs.IOUtil(progArgs) where
-- Some utilities that are a little dirty, but not very.
--import IO
import System.IO.Unsafe
import System.Environment

progArgs :: [String]
--progArgs = hdl getArgs []
progArgs = unsafePerformIO getArgs

--hdl :: IO a -> a -> a
--hdl io def = unsafePerformIO (catch io (\err -> return def))
