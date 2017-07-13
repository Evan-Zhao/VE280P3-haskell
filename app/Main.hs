{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import           World

import           Control.Monad.Writer
import           Data.Foldable
import           Data.List            (findIndex)
import           Data.Maybe           (fromJust, fromMaybe, isJust, isNothing)
import           System.Environment
import           System.FilePath      ((</>))

data Argument = Argument {
    summaryPath :: String,
    worldPath   :: String,
    simRound    :: Int,
    isVerbose   :: Bool
}

type Summary = (String, [String])

parseArgs :: [String] -> Argument
parseArgs [path1, path2, roundStr]
    | read roundStr < 0 = error negRoundError
    | otherwise = Argument path1 path2 (read roundStr) False
parseArgs [path1, path2, roundStr, verbStr]
    | verbStr == "v" || verbStr == "verbose" = Argument path1 path2 (read roundStr) True
    | otherwise = error defaultError
parseArgs args
    | length args <= 3 = error missingArgError
    | length args > 5  = error defaultError

parseSummary :: [String] -> Summary
parseSummary []               = error defaultError
parseSummary (folder:species) = (folder, species)

parseSpecies :: String -> [String] -> Species
parseSpecies specName contents = Species specName ops where
    ops = makeLine $ map readInst contEff
    contEff = takeWhile (/= "") contents

parseCreature :: [Species] -> Int -> String -> Creature
parseCreature specs label line = Creature {..} where
    c_progIdx = 0
    c_onHillActive = False
    (speciesS:dirS:rowS:colS:rest) = words line
    c_species = fromMaybe theError $ find (== dummySpec) specs where
        theError  = error $ unknownSpecError speciesS
        dummySpec = Species speciesS $ makeLine []
    c_direction = readDir dirS
    c_location = (read rowS, read colS)
    skills = let dummyCr = Creature {c_canFly = False, c_canArch = False, ..} in
        map (readAbility dummyCr) rest
    (c_canFly, c_canArch) = (Fly `elem` skills, Arch `elem` skills)

parseGridTerr :: (Int, Int) -> [String] -> Grid
parseGridTerr (r, c) = Grid r c . readTerr where
    readTerr grid = make2D $ map (map readGridCont) $ mesh2D grid
    readGridCont (loc, c) = GridCont (readTerrain loc c) Nothing
    mesh2D grid = zipWith (curry mesh1D) [0 ..] grid
    mesh1D (r, line) = zip (zip (repeat r) [0..]) line

placeCreature :: [Creature] -> Grid -> (Int, Creature) -> Grid
placeCreature crs g@Grid{..} (idx, Creature{..})
    | Nothing <- creatureIdx = adjustCont modifier c_location g
    | Just i  <- creatureIdx = error $ overlapError (crs !! i) Creature{..}
  where
    creatureIdx = g_creatureP $ contAt g c_location
    modifier gc = gc {g_creatureP = Just idx}

parseWorld :: [Species] -> [String] -> World
parseWorld specs (rStr:cStr:rest) = World {..} where
    (r, c) = (read rStr, read cStr)
    (terrainStrs, creaStrs) = splitAt r rest
    w_species = makeLine specs
    creatures = zipWith (parseCreature specs) [0..] creaStrs
    w_creature = makeLine creatures
    gridTerr = parseGridTerr (r, c) terrainStrs
    w_grid = foldl' (placeCreature creatures) gridTerr $ zip [0..] creatures

ifEmpty, ifEnemy, ifSame, ifWall, isInvis :: World -> Creature -> Bool
ifEmpty w@World{..} cr@Creature{..} =
    not (outOfBound w_grid assumed) && not (isInvis w cr) && hasCr where
        assumed = pointMoveBy c_location c_direction 1
        hasCr = isNothing $ creatureAt w w_grid assumed
ifWall w@World{..} cr@Creature{..} =
    not (outOfBound w_grid assumed) && te_there == Lake && not c_canFly where
        assumed = pointMoveBy c_location c_direction 1
        te_there = g_terrain $ contAt w_grid assumed
isInvis w@World{..} cr@Creature{..} = let assumed = pointMoveBy c_location c_direction 1
    in g_terrain (contAt w_grid assumed) == Forest && not c_canArch
ifEnemy = ifSameOrEnemy (/=)
ifSame = ifSameOrEnemy (==)

ifSameOrEnemy :: (Species -> Species -> Bool) -> World -> Creature -> Bool
ifSameOrEnemy pred w@World{..} cr@Creature{c_species = csp, ..} =
    not (outOfBound w_grid assumed) && not (isInvis w cr) && sameSpec where
        assumed = pointMoveBy c_location c_direction 1
        maybeCr = creatureAt w w_grid assumed
        sameSpec = maybe False id $ ((`pred` csp) . c_species) <$> maybeCr

left, right :: Int -> World -> World
left n World{..} = World{w_creature = adjustLine modifier n w_creature, ..} where
    modifier cr = cr {c_direction = toEnum newDirNum}
    newDirNum = (fromEnum (c_direction cr) - 1 + maxBound) `mod` maxBound
    cr = w_creature `at` n
right n World{..} = World{w_creature = adjustLine modifier n w_creature, ..} where
    modifier cr = cr {c_direction = toEnum newDirNum}
    newDirNum = (fromEnum (c_direction cr) + 1) `mod` maxBound
    cr = w_creature `at` n

hop :: Creature -> World -> World
hop cr@Creature{..} w@World{..}
    | canHop = w {w_grid = swapCont c_location assumed w_grid}  -- FIXME: modify creature
    | otherwise = w
  where
    assumed = pointMoveBy c_location c_direction 1
    locthis = contAt w_grid c_location
    locthat = contAt w_grid assumed
    canHop = not overBound && not drown && not overlap where
        overBound = outOfBound w_grid assumed
        drown = g_terrain locthat == Lake && not c_canFly
        overlap = isJust $ g_creatureP locthat

infect :: Creature -> World -> World
infect cr@Creature{c_species = thisSpec, ..} w
    | Nothing <- target = w
    | Just p  <- target = let
            newCr c = c {c_species = thisSpec, c_progIdx = 0}
            cr = w_creature w `at` crP
            crP = fromJust $ g_creatureP $ contAt (w_grid w) p
        in w {w_creature = adjustLine newCr crP $ w_creature w}
  where
    target = infectFindTar c_canArch w cr

infectFindTar :: Bool -> World -> Creature -> Maybe Location
infectFindTar True w@World{..} Creature{c_species = thisSp, ..} =
  find canInfectAt rangeList where
    rangeList = takeWhile (not . outOfBound w_grid) $ map movingN [1..]
    movingN n = pointMoveBy c_location c_direction n
    canInfectAt p
        | Just cr' <- creatureAt w w_grid p = c_species cr' == thisSp
        | otherwise = False
infectFindTar False w@World{..} cr@Creature{..} =
    if ifEnemy w cr then Just assumed else Nothing where
        assumed = pointMoveBy c_location c_direction 1

setHillActive :: Terrain -> Creature -> Creature
setHillActive Hill cr
    | c_canFly cr = cr {c_onHillActive = False}
    | otherwise   = cr {c_onHillActive = not (c_onHillActive cr)}
setHillActive _ cr = cr {c_onHillActive = False}

predList = [ifEmpty, ifEnemy, ifSame, ifWall, const (const True)] -- Last for OpGo
iterInst :: World -> Int -> ([Instruction], Int)
iterInst w@World{..} n
    | commandNum < nullaryCount = [inst]
    | otherwise = undefined
  where
    thisCreature = w_creature w `at` n
    inst = s_prog (c_species thisCreature) `at` c_progIdx thisCreature
    commandNum = fromEnum $ i_op inst
simulateCreatureN :: World -> Int -> Writer [History] World
simulateCreatureN w@World{..} n
    | notActive = return w {w_creature = adjustLine setHillActive n w_creature}
    | otherwise = undefined
  where
    notActive = not $ c_onHillActive thisCreature
    thisCreature = w_creature w `at` n
    inst = s_prog (c_species thisCreature) `at` c_progIdx thisCreature

simulateRound :: World -> (World, [History])
simulateRound world = runWriter $ foldlM simulateCreatureN world [0 .. n0 world - 1] where
    n0 (World _ cr _) = length cr

mainThrow :: IO ()
mainThrow = do
    arg <- parseArgs <$> getArgs
    sum <- (parseSummary . lines) <$> readFile (summaryPath arg)
    let paths = map (fst sum </>) $ snd sum
    speciesFiles <- map lines <$> mapM readFile paths
    let species = zipWith parseSpecies (snd sum) speciesFiles
    world <- parseWorld species . lines <$> readFile (worldPath arg)
    return ()

main :: IO ()
main = undefined
