
import System.IO (Handle, hPutStrLn, withFile, IOMode(WriteMode))

data BinaryTree a
     = Empty
     | Node (BinaryTree a) a (BinaryTree a)
     deriving Show

size :: BinaryTree a -> Integer
size Empty = 0
size (Node left value right) =
     1 + size left + size right

height :: BinaryTree a -> Integer
height Empty = 0
height (Node left value right) =
       1 + max (height left) (height right)

search :: Eq a => BinaryTree a -> a -> Bool
search Empty _ = False
search (Node _ value _) goal | value == goal = True
search (Node left _ right) goal =
         search left goal || search right goal

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node left value right) =
    inorder left ++ [value] ++ inorder right

add :: BinaryTree a -> a -> BinaryTree a
add tree value = (Node tree value Empty)

insert :: Ord a => BinaryTree a -> a -> BinaryTree a
insert Empty new = (Node Empty new Empty)
insert (Node left current right) new =
    if new < current
    then Node (insert left new) current right
    else Node left current (insert right new)

toDot :: Show a => Handle -> String -> BinaryTree a -> IO ()
toDot h k Empty = hPutStrLn h $ k ++ " [label=\"\",shape=box,width=0.1,height=0.1]"
toDot h k (Node left value right) = do
    hPutStrLn h $ k ++ " [label=" ++ show value ++ "]"
    let kl = 'l' : k
    let kr = 'r' : k
    hPutStrLn h $ k ++ " -> " ++ kl
    hPutStrLn h $ k ++ " -> " ++ kr
    toDot h kl left
    toDot h kr right

writeDot :: Show a => FilePath -> BinaryTree a -> IO ()
writeDot path tree = withFile path WriteMode go
  where go h = do
          hPutStrLn h "digraph {"
          toDot h "r" tree
          hPutStrLn h "}"

twice x = 2*x

t1, t2, t3 :: BinaryTree Int
t1 = foldl insert Empty $ map twice [1..7]
t2 = foldl insert Empty $ map twice [4,2,1,3,6,5,7]
t3 = foldl insert Empty $ [2,4,6,8,5,3,10]

main = do
  writeDot "t1.dot" t1
  writeDot "t2.dot" t2
  writeDot "t3.dot" t3

