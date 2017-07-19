import System.Console.ANSI 
import Data.Char (chr) 
import Data.List (intercalate)
import Control.Monad.State.Lazy

type Position  = (Int, Int)

data GameState = GameState TileMap Character
data Character =  Character Char Position
data Tileset   = Tileset [Char]
data TileMap   = TileMap [[Int]] Tileset

class Actor a where
  move       :: a -> Position -> a 
  position   :: a -> Position
  char       :: a -> Char
  atPosition :: a -> Position -> Bool

instance Actor Character where
  move     (Character c (x,y)) (xSpeed, ySpeed) = Character c (x+xSpeed, y+ySpeed)
  position (Character _ (x,y))                  = (x, y)
  char     (Character c _    )                  = c
  atPosition    (Character _ (x,y)) pos              = (x, y) == pos

-- Map that will be drawn. Each index corresponds to an indexin the tileset.
tileMapArray = [
  [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5],
  [4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3]]

-- Inital Game States
tileset          = Tileset ['╔', '═', '╗', '╝', '╚', '║']
tileMap          = TileMap tileMapArray tileset
character        = Character 'O' (3, 3)
initialGameState = GameState tileMap character

tileAt :: Tileset -> Int -> Char
tileAt (Tileset t) n 
  | n >= 0 && n < (length t) = t !! n
  | otherwise                = ' '

getCharAtPosition :: Actor a => TileMap -> a -> Position -> Char
getCharAtPosition (TileMap tileMap tileSet) actor (x,y) 
  | (position actor) == (x, y) = char actor 
  | otherwise                  = tileAt tileSet ((tileMap !! y) !! x)


getMap :: Actor a => TileMap -> a -> Position -> [Char]
getMap tmap@(TileMap tileMap tileset) actor (x, y)
  | (y < (length tileMap) && ( x <  (length (tileMap !! y))) && (atPosition actor (x, y)) ) = (char actor):  (getMap tmap actor (x+1, y) )
  |  y < (length tileMap) && ( x <  (length (tileMap !! y)))                                = (getCharAtPosition tmap actor (x, y)) : (getMap tmap actor (x+1, y))
  |  y < (length tileMap) && ( x >= (length (tileMap !! y)))                                = '\n' :  (getMap tmap actor (0, y+1))
  | otherwise                                                                               = "" 
 
moveActor :: GameState -> Position ->  (Position, GameState)
moveActor (GameState tileMap actor) (xSpeed, ySpeed) = (pos, gameState ) where
  character  = move actor (xSpeed, ySpeed)
  pos        = position character
  gameState  = GameState tileMap character

main = do
  mainLoop initialGameState

mainLoop gameState = do
  displayScene gameState
  input <- getChar
  newState <- handleInput gameState input
  cursorBackward 1
  mainLoop $ snd newState


handleInput gameState input = do
  case input of 
    'h' -> return $ moveActor gameState ((-1), 0)
    'l' -> return $ moveActor gameState (1, 0)
    'j' -> return $ moveActor gameState (0, 1)
    'k' -> return $ moveActor gameState (0, (-1))
    otherwise -> return $ moveActor gameState (0, 0)

  
displayScene (GameState tileMap actor) = do
  clearScreen
  putStrLn "Movement Demo \n"
  putStrLn "Use the following keys to move.\n(Up: k, Down: j, Left: h, Right: l)\n"
  putStrLn $ getMap tileMap actor (0,0)
