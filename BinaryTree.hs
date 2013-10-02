
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

search :: Ord a => BinaryTree a -> a -> Bool
search Empty _ = False
search (Node _ x _) y | y == x = True
search (Node l x _) y | y < x = search l y
search (Node _ x r) y = search r y

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node left value right) =
    inorder left ++ [value] ++ inorder right

add :: BinaryTree a -> a -> BinaryTree a
add tree value = (Node tree value Empty)

insert :: Ord a => BinaryTree a -> a -> BinaryTree a
insert Empty y = (Node Empty y Empty)
insert (Node l x r) y | y < x = Node (insert l y) x r
insert (Node l x r) y = Node l x (insert r y)

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
t0 = Node (Node (Node Empty 2 Empty) 4
                (Node Empty 6 Empty)) 8 Empty

main = do
  writeDot "t1.dot" t1
  writeDot "t2.dot" t2
  writeDot "t3.dot" t3
  writeDot "t0.dot" t0
  writeDot "t5.dot" $ insert t0 5
