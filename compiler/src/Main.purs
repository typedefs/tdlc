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
import Node.Process (PROCESS, exit)
import Prelude
import TDL.Check (inferModule, prettyError, runCheck)
import TDL.Extraction.Markdown (markdownModule)
import TDL.Extraction.PureScript (pursModule)
import TDL.Parse (parse)
import TDL.Syntax (Doc(..))

--------------------------------------------------------------------------------

main :: forall eff. Array String -> Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS, process :: PROCESS | eff) Unit
main [extractorName, path] = do
  extractor <- case extractorName of
    "--purescript" -> pure $ pursModule
    "--markdown"   -> pure $ \x -> case markdownModule x of Doc d -> d
    _ -> error ("Unknown extractor: " <> extractorName) *> exit 1
  text <- readTextFile UTF8 path
  compile extractor text
  where compile extractor s =
          s
           #  lmap show <<< parse
          >>~ lmap prettyError <<< runCheck <<< inferModule
          <#> extractor
           #  either (\e -> error e *> exit 2) log
main _ = error "Usage: tdlc <extractor> <path>"

--------------------------------------------------------------------------------

tap :: forall m a. (Monad m) => m a -> (a -> m Unit) -> m a
tap a k = a >>= \a' -> k a' $> a'

infixl 1 tap as >>~
