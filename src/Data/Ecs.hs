module Data.Ecs where

import Prelude hiding (lookup)
import Data.Map
import qualified Data.List as L
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Control.Monad.Trans.State
import Control.Applicative (liftA2)

newtype Instance = Instance {getId :: Int}
                   deriving(Eq, Show, Read, Ord)

data World c cd s = World
    { components :: Map c (Map Instance cd)
    , instances  :: [Instance]
    , idCounter  :: Int
    , additionalState :: s
    }

type World' c = World c (ComponentData c) (AdditionalState c)

-- Define a system over components c
--
-- c should just be a set of identifiers, for example
-- c = Velocity | Position
class (Monad m) =>  System c m | c -> m where
    -- This is the accompanied data for a component. if c is the above,
    -- an appropriate definition for ComponentData would be
    -- data ComponentData c = Vel Float Float | Pos Float Float
    data ComponentData c

    data AdditionalState c
    -- The entire system of the world is a function
    -- system Velocity (Vel x y) inst = do
    --   modifyComponentOf Position inst (\(Pos xx yy) -> Pos (x + xx) (y
    --   + yy))
    system    :: c -> ComponentData c -> Instance -> StateT (World' c) m ()

    -- initialize the world
    initWorld :: (Enum c, Ord c, Bounded c) => AdditionalState c -> World' c
    initWorld = World (fromList (fmap (,fromList []) [minBound..maxBound])) [] 0

-- get a new unique ID
getNewId :: (Monad m) => StateT (World' c) m Int
getNewId = do
    id' <- fmap idCounter get
    modify (\w -> w {idCounter = idCounter w + 1})
    return id'

getComponentData :: (Monad m, Ord c) => c -> Instance -> StateT (World' c) m (ComponentData c)
getComponentData component inst = fmap ((! inst) . (! component) . components) get

mgetComponentData :: (Monad m, Ord c) => c -> Instance -> StateT (World' c) m (Maybe (ComponentData c))
mgetComponentData component inst = fmap (lookup inst . (! component) . components) get

getComponentDataList :: (Monad m, Ord c) => c -> StateT (World' c) m [(Instance, ComponentData c)]
getComponentDataList component = fmap (toList . (! component) . components) get

-- modifies the componentData of an instance. The function used to modify
-- should never change the form of the data, i.e you must use the same
-- constructor which accompanies c.
modifyComponentOf :: (Monad m, Ord c) => c -> Instance -> (ComponentData c -> ComponentData c) -> StateT (World' c) m ()
modifyComponentOf component inst f = do
    componentMap <- fmap components get
    let newCompData = f (componentMap ! component ! inst)
    modify (\w -> w {components = adjust (insert inst newCompData) component componentMap})

addComponent :: (Monad m, Ord c) => c -> Instance -> ComponentData c -> StateT (World' c) m ()
addComponent component inst compData = do
    componentMap <- fmap components get
    modify (\w -> w {components = adjust (insert inst compData) component componentMap})

modifyComponents :: (Monad m, Ord c) => c -> (Instance -> ComponentData c -> ComponentData c) -> StateT (World' c) m ()
modifyComponents component f = do
    componentMap <- fmap components get
    let newCompMap = mapWithKey f (componentMap ! component)
    modify (\w -> w {components = insert component newCompMap (components w) })

hasComponent :: (Monad m, Ord c) => c -> Instance -> StateT (World' c) m Bool
hasComponent component inst = fmap (member inst . (! component) . components) get

-- creates a new instance with a unique ID.
createEntity :: (Monad m, Ord c) => [(c, ComponentData c)] -> StateT (World' c) m Instance
createEntity ls = do
    inst <- fmap Instance getNewId
    forM_ ls $ \(cmp, componentData) ->
      modify (\w -> w {components = adjust (insert inst componentData) cmp . components $ w})
    return inst

deleteEntity :: (Monad m, Ord c) => Instance -> StateT (World' c) m ()
deleteEntity inst = do
    modify (\w -> w {components = fmap (delete inst) $ components w, instances = L.delete inst $ instances w})

runSystem :: (Monad m, Ord c, Enum c, Bounded c, System c m) => StateT (World' c) m ()
runSystem =
    forM_ (fmap (liftA2 fmap (,) getComponentDataList) [minBound..maxBound])
          $ \ls -> do
      (component, list) <- ls
      forM_ list $ \(inst, compData) -> system component compData inst

getAdditionalState :: (Monad m) => StateT (World' c) m (AdditionalState c)
getAdditionalState =
    fmap additionalState get

modifyAdditionalState :: (Monad m) => (AdditionalState c -> AdditionalState c) -> StateT (World' c) m ()
modifyAdditionalState f = modify (\w -> w {additionalState = f (additionalState w)})
