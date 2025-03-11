module SystemInfo where

import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift (lift))
import System.Process (readProcess)

systemInfo :: Q Exp
systemInfo = do
  uname <- runIO $ do
    readProcess "uname" ["-a"] ""
  time <- runIO $ do
    readProcess "date" ["-u"] ""
  let msg = "> uname: " <> uname <> "> date: " <> time

  lift msg
