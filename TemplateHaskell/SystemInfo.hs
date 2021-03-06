module SystemInfo where

import Control.Monad.Trans (MonadIO (liftIO))
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift (lift))
import System.Process (readProcess)

systemInfo :: Q Exp
systemInfo = do
  uname <- runIO $ do
    readProcess "uname" ["-a"] ""
  time <- runIO $ do
    readProcess "date" ["-u"] ""
  let msg = "> uname: " <> uname <> "\n> date: " <> time

  lift msg
