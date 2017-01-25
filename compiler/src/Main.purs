module Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Partial.Unsafe (unsafePartial)
import Prelude
import TDL.Check (inferModule, prettyError, runCheck)
import TDL.Extraction.PureScript (pursModule)
import TDL.Parse (parse)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main =
  """
    type T = int;             (* Integer *)
    type U = {a : T, b : {}}; (* Product *)
    type V = [a : T, b : []]; (* Sum *)
  """
   #  lmap show <<< parse
  >>~ lmap prettyError <<< runCheck <<< inferModule
  <#> unsafePartial pursModule
   #  either (log <<< show) log

--------------------------------------------------------------------------------

tap :: forall m a. (Monad m) => m a -> (a -> m Unit) -> m a
tap a k = a >>= \a' -> k a' $> a'

infixl 1 tap as >>~
