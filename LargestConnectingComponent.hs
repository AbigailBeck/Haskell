import Data.List
import Data.Ord
import Control.Applicative

type Color = Int
type Index = (Int, Int)
type Cells = [(Index, Color)] 

example :: Cells
example = [((0,0), 1), ((0,1), 4),((0,2), 4), ((0,3), 4), ((0,4), 4), ((0,5), 3),((0,6), 3), ((0,7), 1), ((1,0), 2), ((1,1), 1),((1,2), 1), ((1,3), 4), ((1,4), 3), ((1,5), 3),((1,6), 1), ((1, 7), 1), ((2,0), 3), ((2,1), 2),((2,2), 1), ((2,3), 1), ((2,4), 2), ((2,5), 3),((2,6), 2), ((2,7), 1), ((3,0), 3), ((3,1), 3),((3,2), 3), ((3,3), 1), ((3,4), 2), ((3,5), 3),((3,6), 2), ((3,7), 3), ((4,0), 3), ((4,1), 1),((4,2), 3), ((4,3), 1), ((4,4), 1), ((4,5), 4),((4,6), 4), ((4,7), 4), ((5,0), 1), ((5,1), 1),((5,2), 3), ((5,3), 1), ((5,4), 1), ((5,5), 4),((5,6), 4), ((5,7), 4)]


--get a list of colors in the cells
getListOfColor :: Cells -> [Color]
getListOfColor clls = sort $ nub $ map (\x -> snd x) clls

--get a list of index group by colors : eg. [[color1], [color2], [color3]]
getIndexGroupByColors :: [Color] -> Cells -> [[Index]]
getIndexGroupByColors colorList grid = map (\c -> [(x, y) | ((x, y), col) <- grid, col==c] ) colorList 

--give an index and return a list of valid neighbours
getAdjacent :: Index -> [Index]
getAdjacent (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

--give a list of neighbours and return a list of nighbours of same color (return connected component of a)
getMatchedIndex :: [Index] -> [Index] -> [Index]
getMatchedIndex indexList indexPool = concatMap (\(i, d) -> [(x, y) | (x, y) <- indexPool, (x, y)==(i, d)]) indexList 

--give cell and return CC of each index
getCCByIndex :: Cells -> [[[Index]]]
getCCByIndex clls = 
        let listOfColors = getListOfColor clls
            indexByColors = getIndexGroupByColors listOfColors clls
            in map (\x-> map (\y -> y : (getMatchedIndex (getAdjacent y) x)) x) indexByColors

exists :: Eq a => [a] -> [a] -> Bool
exists x y = any id $ (==) <$> x <*> y

myExist :: Eq a => [a] -> [[a]] -> [a]
myExist [] _ = []
myExist idx [] = idx
myExist idx (d:dls) =  if (exists idx d) then nub $ myExist (idx ++ d) dls else myExist idx dls

getLCCByIndex :: Cells -> [[[Index]]]
getLCCByIndex clls = map (\color -> map (\cc -> myExist cc color) color) (getCCByIndex example)

getLCCByColor :: Cells -> [[Index]]
getLCCByColor clls = map (\x -> maximumBy (comparing length) x) (getLCCByIndex clls)

getLCC :: Cells -> [Index]
getLCC clls = sort $ maximumBy (comparing length) (getLCCByColor clls)

