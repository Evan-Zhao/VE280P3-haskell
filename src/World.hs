{-# LANGUAGE RecordWildCards #-}
module World where
import           Data.List
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Text.Printf

maxSpecies, maxProgram, maxCreatures, maxHeight, maxWidth :: Int
maxSpecies = 10
maxProgram = 40
maxCreatures = 50
maxHeight = 20
maxWidth = 20

defaultError    = "Error somewhere and didn't care."

missingArgError     = "Error: Missing arguments!\
\Usage: ./p3 <species-summary> <world-file> <rounds> [v|verbose]"
negRoundError       = "Error: Number of simulation rounds is negative!"
openFileError name  = printf "Error: Cannot open file %s!" name

overSpeciesError :: String
overSpeciesError        = printf "Error: Too many species!\
\Maximal number of species is %d." maxSpecies
overInstError spec      = printf "Error: Too many instructions for species %s!\
\Maximal number of instructions is %d." spec maxProgram
unknownInstError inst   = printf "Error: Instruction %s is not recognized!" inst

heightError = "Error: The grid height is illegal!"
widthError  = "Error: The grid width is illegal!"

unknownTerError (r,c) ch    =
    printf "Error: Terrain square (%c %d %d) is invalid!" ch r c

creatureTuple :: Creature -> String
creatureTuple Creature{..} =
    printf "(%s %s %d %d)" (show c_species) (showDirLong c_direction) r c where
        (r,c) = c_location

overCreatureError :: String
overCreatureError       = printf "Error: Too many creatures!\
\Maximal number of creatures is %d." maxCreatures
unknownSpecError spec   = printf "Error: Species %s not found!" spec
unknownDirError dir     = printf "Error: Direction %s is not recognized!" dir
outOfBoundError cr r c  = printf "Error: Creature %s is out of bound!\
\The grid size is %d-by-%d." (creatureTuple cr) r c
unknownAbError cr ab    = printf "Error: Creature %s has an invalid ability %s!"
    (creatureTuple cr) ab
overlapError cr1 cr2    = printf "Error: Creature %s overlaps with creature %s!"
    (creatureTuple cr2) (creatureTuple cr1)
drownError cr           = printf "Error: Creature %s is in a lake square!\
\The creature cannot fly!" (creatureTuple cr)

data Direction = East | South | West | North deriving (Eq, Ord, Enum, Bounded)
dirReadName = ["east", "south", "west", "north"]
dirShowName = "eswn"
readDir :: String -> Direction
readDir str = maybe (error $ unknownDirError str) toEnum maybeIdx where
    maybeIdx = str `elemIndex` dirReadName
showDirShort, showDirLong :: Direction -> String
showDirShort d = [dirShowName !! fromEnum d]
showDirLong  d = dirReadName !! fromEnum d

type Location = (Int, Int)
pointMoveBy :: Location -> Direction -> Int -> Location
pointMoveBy (r,c) East n  = (r,c+n)
pointMoveBy (r,c) South n = (r+n,c)
pointMoveBy (r,c) West n  = (r,c-n)
pointMoveBy (r,c) North n = (r-n,c)

data Terrain = Plain | Lake | Forest | Hill deriving (Enum, Eq, Ord)
terrainReadName = "PLFH"
readTerrain :: Location -> Char -> Terrain
readTerrain loc ch = maybe theError toEnum maybeIdx where
    maybeIdx = elemIndex ch terrainReadName
    theError = error $ unknownTerError loc ch

data Ability = Fly | Arch deriving (Eq, Ord, Enum)
abilityReadName = "fa"
readAbility :: Creature -> String -> Ability
readAbility cr [ch] = maybe theError toEnum maybeIdx where
    maybeIdx = ch `elemIndex` abilityReadName
    theError = error $ unknownAbError cr [ch]
readAbility cr str = error $ unknownAbError cr str

data Opcode = OpHop | OpLeft | OpRight | OpInfect
            | OpIfempty | OpIfenemy | OpIfsame | OpIfwall | OpGo deriving (Enum, Eq)
data Instruction = Inst {
    i_op  :: Opcode,
    i_arg :: Int
}
opName = ["hop", "left", "right", "infect",
    "ifempty", "ifenemy", "ifsame", "ifwall", "go"]
nullaryCount = 4
readInst :: String -> Instruction
readInst str
    | Just i  <- maybeIdx = let op = toEnum i in
        if i < nullaryCount then Inst op 0
        else Inst op argInt
    | otherwise = error $ unknownInstError command
  where
    maybeIdx = command `elemIndex` opName
    argInt = read $ head $ tail strWords
    command = head strWords
    strWords = words str

data Species = Species {
    s_name :: String,
    s_prog :: Seq Instruction
}
instance Show Species where
    show (Species name _) = name
instance Eq Species where
    a == b = (s_name a == s_name b)

data Creature = Creature {
    c_progIdx      :: Int,
    c_location     :: Location,
    c_direction    :: Direction,
    c_species      :: Species,
    c_canFly       :: Bool,
    c_canArch      :: Bool,
    c_onHillActive :: Bool
}
instance Show Creature where
    show Creature{..} = uncurry printLoc c_location where
        printLoc = printf "%s %s %d %d" (show c_species) (showDirShort c_direction)

type CreatureLabel = Int
data GridCont = GridCont {
    g_terrain   :: Terrain,
    g_creatureP :: Maybe CreatureLabel
}
printSingleGrid :: Seq Creature -> Maybe CreatureLabel -> String
printSingleGrid _ Nothing = replicate 4 '_'
printSingleGrid crs (Just x) = printCreature (crs `S.index` x) where
    printCreature cr = printf "%s_%s"
        (take 2 $ show $ c_species cr) (showDirShort $ c_direction cr)

data Grid = Grid {
    g_height   :: Int,
    g_width    :: Int,
    g_contents :: Seq (Seq GridCont)
}
outOfBound :: Grid -> Location -> Bool
outOfBound Grid{..} (r,c) = r >= 0 && r < g_height && c >= 0 && c < g_width

creatureAt :: World -> Grid -> (Int, Int) -> Maybe Creature
creatureAt w grid (r,c) = (w_creature w `S.index`) <$> g_creatureP (contAt grid (r,c))

contAt :: Grid -> (Int, Int) -> GridCont
contAt grid (r,c) = (g_contents grid `S.index` r) `S.index` c

adjustCont :: (GridCont -> GridCont) -> (Int, Int) -> Grid -> Grid
adjustCont f (r,c) grid =
    grid { g_contents = S.adjust (S.adjust f c) r $ g_contents grid}

swapCont :: (Int, Int) -> (Int, Int) -> Grid -> Grid
swapCont p1 p2 grid = let (place1, place2) = (contAt grid p1, contAt grid p2)
    in adjustCont (const place1) p2 $ adjustCont (const place2) p1 grid

at :: Seq a -> Int -> a
at = S.index

adjustLine :: (a -> a) -> Int -> Seq a -> Seq a
adjustLine = S.adjust

makeLine :: [a] -> Seq a
makeLine = S.fromList

make2D :: [[a]] -> Seq (Seq a)
make2D = S.fromList . map S.fromList

data World = World {
    w_species  :: Seq Species,
    w_creature :: Seq Creature,
    w_grid     :: Grid
}
instance Show World where
    show (World _ cr grid) = concatMap (printLine cr) (g_contents grid) where
        printLine cr contLine = concatMap (printSingle cr) contLine ++ "\n"
        printSingle cr cont = printSingleGrid cr (g_creatureP cont) ++ " "

data History = History Creature [Instruction]
printHistory :: Bool -> History -> String
printHistory False (History cr insts) = printf "Creature %s takes action: %s"
    (creatureTuple cr) $ (opName !!) $ fromEnum $ i_op $ last insts
