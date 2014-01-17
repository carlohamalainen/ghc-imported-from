-- When.hs

module When where

import Control.Monad ( forM_, liftM, filterM, when, unless )
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Writer.Lazy





main = do
    when True $ do print "hah"
