import System.IO (Handle, hPutStrLn, withFile, IOMode(WriteMode))
import Text.Printf (printf)

data Color = Red | Black

data RedBlackTree a
     = Empty
     | Node Color (RedBlackTree a) a (RedBlackTree a)

size :: RedBlackTree a -> Integer
size Empty = 0
size (Node _ left value right) =
     1 + size left + size right

height :: RedBlackTree a -> Integer
height Empty = 0
height (Node _ left value right) =
       1 + max (height left) (height right)

search :: Eq a => RedBlackTree a -> a -> Bool
search Empty _ = False
search (Node _ _ value _) goal | value == goal = True
search (Node _ left _ right) goal =
         search left goal || search right goal

insert :: Ord a => RedBlackTree a -> a -> RedBlackTree a
insert t x = Node Black a z b
  where Node _ a z b = ins t
        ins Empty = Node Red Empty x Empty
        ins (Node Black a y b) =
            if x < y then balance (ins a) y b
            else balance a y (ins b)
        ins (Node Red a y b) =
            if x < y then Node Red (ins a) y b
            else Node Red a y (ins b)

balance :: RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
balance (Node Red a x b) y (Node Red c z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Red a x (Node Red b y c)) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance a x (Node Red b y (Node Red c z d)) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance a x (Node Red (Node Red b y c) z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance l x r = Node Black l x r


style :: Color -> String
style Red = "style=filled,fillcolor=red"
style Black = "style=filled,fillcolor=black,fontcolor=yellow"

toDot :: Show a => Handle -> String -> RedBlackTree a -> IO ()
toDot h k Empty = hPutStrLn h $ k ++ " [label=\"\",shape=box,width=0.1,height=0.1,style=filled,fillcolor=black]"
toDot h k (Node c left value right) = do
    hPutStrLn h $ k ++ " [" ++ style c ++ ",label=" ++ show value ++ "]"
    let kl = 'l' : k
    let kr = 'r' : k
    hPutStrLn h $ k ++ " -> " ++ kl
    hPutStrLn h $ k ++ " -> " ++ kr
    toDot h kl left
    toDot h kr right

writeDot :: Show a => FilePath -> RedBlackTree a -> IO ()
writeDot path tree = withFile path WriteMode go
  where go h = do
          hPutStrLn h "digraph {"
          toDot h "r" tree
          hPutStrLn h "}"

twice x = 2*x

t1, t2, t3 :: RedBlackTree Int
t1 = foldl insert Empty $ map twice [1..7]
t2 = foldl insert Empty $ map twice [4,2,1,3,6,5,7]
t3 = foldl insert Empty $ [2,4,6,8,5,3,10]

animate :: [Int] -> IO (RedBlackTree Int)
animate [] = return $ Empty
animate (x:xs) = do
    t0 <- animate xs
    let t = insert t0 x
    let k = size t
    let path = printf "a%03d.dot" k
    writeDot path t
    return t

main = do
  writeDot "r1.dot" t1
  writeDot "r2.dot" t2
  writeDot "r3.dot" t3
  let xs = [5..9] ++ [0..4]
  let ys = xs ++ [30..40] ++ [10..15]
  writeDot "r4.dot" $ foldl insert Empty ys
  animate xs
