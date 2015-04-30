module Main where

import System.Exit(exitSuccess)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, when, void)
import Data.Map
import Data.Ecs

-- You must specify the components of your system. The components are just
-- identifiers and shouldn't carry any data. To use all functions of the
-- library, you need to derive all the typeclasses listed
data Component = Player | Printer
  deriving (Eq, Show, Read, Ord, Bounded, Enum)


instance System Component IO where
    -- This is the actual data for the components.
    data ComponentData Component = Str String | PlayerData Int
    
    -- The system is just a function which takes a component, the
    -- corresponding componentdata and the instance and provide a
    -- StateT result.
    system Printer (Str str) _           = lift . putStrLn $ str
    system Player (PlayerData gold) inst = do
      lift . putStrLn $ "You have " ++ show gold ++ " gold pieces"
      lift . putStrLn $ "(m)ake more gold or (t)hrow a gold piece on the ground:"
      lift getChar >>= \x -> case x of
        'm' -> modifyComponentOf Player inst (\(PlayerData g) -> PlayerData $ g + 1)
        't' -> when (gold > 0) $ do
          modifyComponentOf Player inst (\(PlayerData g) -> PlayerData $ g - 1)
          void $ createEntity [(Printer, Str "There is a piece of gold on the ground")]
        _   -> return ()

main = flip execStateT initWorld $ do
  player <- createEntity [(Player,PlayerData 0)]
  forever $ do
    runSystem
