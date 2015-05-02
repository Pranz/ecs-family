module Main where

import System.Exit(exitSuccess)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Map
import Data.Ecs

-- You must specify the components of your system. The components are just
-- identifiers and shouldn't carry any data. To use all functions of the
-- library, you need to derive all the typeclasses listed
data Component = Printer | HasString
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

instance System Component IO where
    -- This is the actual data for the components.
    data ComponentData Component = Print | Str String
    data AdditionalState Component = NoState
    -- The system is just a function which takes a component, the
    -- corresponding componentdata and the instance and provide a
    -- StateT result.
    system Printer _ inst =
      -- This checks if the component has a HasString component, and in
      -- that case prints the string it holds, otherwise it defaults to
      -- a constant message
      mgetComponentData HasString inst >>= \case
        Just (Str str) -> lift . putStrLn $ str
        Nothing        -> lift . putStrLn $ "This entity had no message to print, but is still a printer!"
    system _ _ _ = return ()

main = flip execStateT (initWorld NoState) $ do
  ent1 <- createEntity [(Printer,Print)]
  ent2 <- createEntity [(Printer,Print), (HasString, Str "this is an individual message")]
  runSystem
  addComponent HasString ent1 $ Str "Whee now I have my own string too!"
  runSystem
