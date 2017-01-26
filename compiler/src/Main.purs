module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)
import Prelude
import TDL.Check (inferModule, prettyError, runCheck)
import TDL.Extraction.PureScript (pursModule)
import TDL.Parse (parse)

main :: forall eff. Array String -> Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | eff) Unit
main [path] = read >>= compile
  where read = readTextFile UTF8 path
        compile s =
          s
           #  lmap show <<< parse
          >>~ lmap prettyError <<< runCheck <<< inferModule
          <#> unsafePartial pursModule
           #  either error log
main _ = log "Usage: tdlc <path>"

--------------------------------------------------------------------------------

tap :: forall m a. (Monad m) => m a -> (a -> m Unit) -> m a
tap a k = a >>= \a' -> k a' $> a'

infixl 1 tap as >>~
