import Data.List (break)

--  tree zipper

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Empty, _) = Nothing  
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Empty, _) = Nothing  
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)

goUp :: Zipper a -> Maybe (Zipper a) 
goUp (_, []) = Nothing 
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)

topMost :: Maybe (Zipper a) -> Maybe (Zipper a)
topMost Nothing = Nothing
topMost (Just (t, [])) = Just (t,[])
topMost (Just z) = topMost $ goUp z

(-:) :: a -> (a -> b) -> b
x -: f = f x

modify :: (a -> a) -> Zipper a -> Maybe(Zipper a)
modify f (Node x l r, bs) = pure (Node (f x) l r, bs)
modify f (Empty, bs) = pure (Empty, bs)

attach :: Tree a -> Zipper a -> Maybe(Zipper a)
attach t (_, bs) = pure (t, bs)


demoTree :: Tree Char
demoTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        ) 
        
focus = pure (demoTree, []) >>= goLeft >>= goRight >>= modify (\_ -> 'P')
focus' = focus >>= goLeft >>= attach (Node 'Z' Empty Empty)

-- list zipper

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

-- filesystem rep
type Name = String
type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe(FSZipper)
fsUp (_,[]) = Nothing
fsUp (item, (FSCrumb name before after):bs) = pure (Folder name (before <> [item] <> after), bs)

fsTo :: Name -> FSZipper -> Maybe(FSZipper)
fsTo name (Folder folderName [], bs) = Nothing
fsTo name (Folder folderName items, bs) = pure (item, FSCrumb folderName ls rs:bs)
    where (ls, item:rs) = break (nameIs name) items

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> Maybe(FSZipper)
fsRename newName (Folder name items, bs) = pure (Folder newName items, bs)
fsRename newName (File name dat, bs) = pure (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> Maybe(FSZipper)
fsNewFile item (Folder folderName [], bs) = Nothing
fsNewFile item (Folder folderName items, bs) = pure (Folder folderName (item:items), bs)

myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ] 

newFocus = pure (myDisk,[]) >>= fsTo "pics" >>= fsTo "skull_man(scary).bmp"          
newFocus' = newFocus >>= fsUp >>= fsTo "watermelon_smash.gif"