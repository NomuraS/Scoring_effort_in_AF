module AF where 
import Data.List
import Utility 
-- import Input_WBAF
import Data.Ord
import Data.Ratio
import Data.Tree
import System.IO
import System.Process
import System.IO
import System.Process
import Input_Models
-- import Data.Char
-- import Data.String
-- import Network.HTTP.Client
-- import Algebra.Graph.Labelled
-- import Data.EdgeTree
-- import Data.LabeledTree
-- import Text.Parsec
-- import Text.Parsec.String

------------



-- test_extractLeafs =extractLeafs [("a","b"),("x","root"),("b","root"),("a","b")]

extractLeafs::ATT-> [AR]
extractLeafs att=  
    let 
      snds =[y | (_,y) <-att]
      fsts =[le | (le,_) <-att]
      rootsStr = nub $ filter  (\x-> x `notElem` snds)  fsts
    in 
       rootsStr

isLeaf::AFe->AR->Bool
isLeaf afe@(AFe _ att _) ar = 
 -- let 
 --  extractLeafs::ATT-> [AR]
 --  extractLeafs attx= nub $ filter  (\x-> x `notElem` [y | (_,y) <-attx])  [le | (le,_) <-attx]
 -- in
  ar `elem` (extractLeafs att)

-- isLeaf test_afe "root"


haveBraches:: AFe-> AR->[(AR,AR)]
haveBraches afe@(AFe _ att _) ar = 
 nub $ filter (\(x,y)-> y==ar) att

haveBraches2:: AFe-> AR->[AR]
haveBraches2 afe@(AFe _ att _) ar = 
 [ x | (x,y) <-haveBraches afe ar]

-- data AFe =AFe [AR] ATT EV
-- test_haveBraches =haveBraches (AFe [] [("a","b"),("x","root"),("b","root"),("a","b")] (\x->1) ) "b"

sortPair :: Ord a => [(a,a)]->[(a,a)]
sortPair  = sortBy (\(a1,a2) (b1,b2)-> 
 if (a1 == b1 )
 then  compare a2 b2
 else  compare a1 b1
 )


-- ttest_att = [("x","root"),("b","root"),("a","b")]
-- ttest_afe = AFe ["x","root","b","a"] ttest_att (\x->1)

test_ar =["a"++ (show x) | x <- [1..7]] ++["b"++ (show x) | x <- [1..8]]
test_att = [ ("c2","c1"),("c1","a1"),
              ("a3","b2"),("b2","a2"),("a2","b1"),("b1","a1"),
              ("b7","a7"),("a7","b6"),("b6","a6"),("a6","b5"),("b5","a1"),
              ("b8","a1")]
test_ev a =  case a of 
  "b8" -> 3
  "b2" -> 3
  "a3" -> 2
  "a1" -> 2
  "a7" -> 2
  "b7" -> 2
  "c1" -> 1
  "c2" -> 1
  "b5" -> 1
  _->0
test_afe = AFe  test_ar test_att test_ev

-----------------------------------------------------
-----------------------------------------------------
-----------------------------------------------------

extractKeys:: SSUP-> [AR]
extractKeys list=  
    let 
      snds =[y | (_,y) <-list]
      fsts =[xx | (xx,_) <-list]
      checkroot x = forall fsts (\yy->  x `notElem` yy)
      rootsStr = nub $ filter  checkroot  snds
    in 
       rootsStr

wbaf2forest ::ExBAFe -> Forest AR 
wbaf2forest  exbafe@(ExBAFe _ _ ssup ev) =
   let
    -- bunkai ::(Eq a) =>SUP a->[(Arg a ,Arg a)]
    bunkai xsup =concat $ nub $ map (\(xs,y)->  map (\z-> (z,y)) xs) xsup
    -- transREL2Node::(Eq a) =>[(Arg a ,Arg a )]-> Forest a  
    transREL2Node ysup = 
            map (\(x,y)-> Node x [Node y []]) ysup
    aa = map (\x-> Node x []) (extractKeys ssup)
    bb = transREL2Node $ bunkai ssup
   in
    makeForest bb aa 

makeForest ::(Eq a) => Nodes a ->[Tree a] -> Forest a  
makeForest [] res = res
makeForest (n:nodes) res = 
  let
  insertEdge ::(Eq a) =>  Tree a -> Tree a -> Tree a  
  insertEdge (Node b [Node a []]) (Node x trlist)  
    | x == a = Node a (trlist ++[Node b []])
    | x /= a = Node x (map (\z-> insertEdge (Node b [Node a []]) z) trlist)
  aa = map (insertEdge n) res
  in 
    if aa == res
    then makeForest (nodes++[n]) res
    else makeForest nodes aa

tree2paths ::(Eq a) =>  Tree a -> [[a]]
tree2paths  (Node x []) = [[x]]
tree2paths  (Node x list) = (x:) <$> concat [tree2paths l|l<-list]  

forest2paths ::(Eq a) =>  [Tree a] -> [[[a]]]
forest2paths fr =  map tree2paths fr


-- data ExBAFe = ExBAFe [AR] ATT SSUP EV

afe2exbafe ::AFe->  ExBAFe 
afe2exbafe exbafe@(AFe ars att ev) = 
 let
  ssup = map (\(x,y)->([x],y)) att
 in
  (ExBAFe ars att ssup ev)
 
allpaths :: AFe -> [[AR]]
allpaths afe = map reverse 
              $ concat 
              $ forest2paths 
              $ wbaf2forest 
              $ afe2exbafe afe


allpaths2 afe = 
              wbaf2forest 
              $ afe2exbafe afe

-----------------------------------------------------
-----------------------------------------------------
-----------------------------------------------------

path2rel :: [AR] -> [(AR,AR) ] -> [(AR,AR) ]
path2rel arlist out =
  case (length arlist ) of 
    0 -> undefined
    1 -> out
    _ -> 
      let
        aa = last arlist
        bb = last $init  arlist
      in
        path2rel (init arlist) ((bb,aa): out)

-- [["c2","c1","a1"],["a3","b2","a2","b1","a1"],["b7","a7","b6","a6","b5","a1"],["b8","a1"]]

divide_PRO_CON ::AFe->(AFe,AFe)
divide_PRO_CON xAF@(AFe ar att ev) =
  let
    pathlist= allpaths xAF
    aaa x = map (\li->path2rel li []) x
    connRoots = concat $ aaa $ filter (\x-> length x `mod` 2 == 0) pathlist
    extract_ars roots =nub$concat [ [x,y] | (x,y)<- roots]
    proRoots = concat $ aaa $ filter (\x-> length x `mod` 2 == 1) pathlist
    conAF =(AFe (extract_ars connRoots) connRoots ev)
    proAF =(AFe (extract_ars proRoots) proRoots ev)
  in
    (proAF,conAF)


-- [[("c1","a1")],
-- [("b2","a2"),("a2","b1"),("b1","a1")],
-- [("a7","b6"),("b6","a6"),("a6","b5"),("b5","a1")],[]]


--- totyu
isPRO :: AFe->AR->Bool
isPRO afe a = 
  let 
    paths = allpaths afe
    onepath = head $ filter (\x-> a `elem` x) paths
  in
   case length $ split a onepath of 
    1 -> True
    _ -> 
      let 
        cutpath = last $ split a onepath
      in 
        if length cutpath `mod` 2 ==0
        then True
        else False

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

addpair :: (Int,Int) ->(Int,Int)->(Int,Int)
addpair (n,m) (x,y) =(n+x,m+y)

minpair::[(Int,Int)]->(Int,Int)
minpair xs = head $ sortPair$ xs



test_ar_pro =["c2","c1","a1","a3","b2","a2","b1"]
test_att_pro = [("c2","c1"),("c1","a1"),("a3","b2"),("b2","a2"),("a2","b1"),("b1","a1")]
test_ev_pro a =  case a of 
  "b8" -> 3
  "b2" -> 3
  "a3" -> 2
  "a1" -> 2
  "a7" -> 2
  "b7" -> 2
  "c1" -> 1
  "c2" -> 1
  "b5" -> 1
  _->0
test_afe_pro = AFe  test_ar_pro test_att_pro test_ev_pro


test_ar_con =["b7","a7","b6","a6","b5","a1","b8"]
test_att_con = [("b8","a1")]
-- test_att_con = [("b7","a7"),("a7","b6"),("b6","a6"),("a6","b5"),("b5","a1"),("b8","a1")]
test_ev_con a =  case a of 
  "a1" -> 2
  "b8" -> 3
  "b5" -> 1
  "a6" -> 0
  "b6" -> 0
  "a7" -> 2
  "b7" -> 2
  "b2" -> 3
  "a3" -> 2
  "c1" -> 1
  "c2" -> 1
  _->0
test_afe_con = AFe  test_ar_con test_att_con test_ev_con

-----------------------------------------------------


alpha :: AFe->AR->(Int,Int)
alpha afe@(AFe _ _ ev) a = 
 let 
  conSide = snd $ divide_PRO_CON afe
  br = haveBraches2 conSide a
 in 
  case (isLeaf conSide a) of 
    True  -> (ev a,1)
    False -> 
      case (isPRO conSide a) of 
         True   -> minpair [alpha conSide x | x <- br ]
         False  -> (minpair [alpha conSide x | x <- br ]) `addpair` (ev a,1)



beta :: AFe->AR->(Int,Int)
beta afe@(AFe _ _ ev) a = 
 let 
  conSide = fst $ divide_PRO_CON afe
  br = haveBraches2 conSide a
 in 
  case (isLeaf conSide a) of 
    True  -> (ev a,1)
    False -> 
      case (isPRO conSide a) of 
         True   -> (foldr (addpair)  (0,0) [beta conSide x | x <- br ]) `addpair` (ev a,1)
         False   -> foldr (addpair)  (0,0) [beta conSide x | x <- br ]



trans_baf2af :: BAFe->AFe
trans_baf2af bafe@(BAFe ar att sup  ev)  = 
  let 
    -- newar = ["x"++ show y | y <-[0..1]]
    att2 = att ++ (nub.concat) [ [(x,x ++ "00"),(x ++ "00",y)]|(x,y)<-sup ]
    ar2 =  ar ++ (nub.concat) [ [x,y] |  (x,y)<-att2]
    ev2 = ev
  in
    AFe ar2 att2 ev2

trans_exbaf2af :: ExBAFe->AFe
trans_exbaf2af bafe@(ExBAFe ar att ssup  ev)  = 
  let 
    att2 = att ++ (nub.concat) [   (nub.concat) [[( x,  x ++ "00"), (  x ++ "00",y)] | x <- xs] | (xs,y) <-ssup ]
    -- att2 = att ++ (nub.concat) [ map (\x-> [(x,x ++ "00"),(x ++ "00",y)]) xs |(xs,y)<-ssup ]
    ar2 =  ar ++ (nub.concat) [ [x,y] |  (x,y)<-att2]
    ev2 = ev
  in
    AFe ar2 att2 ev2

--------------------------------------------------------------------------------------------
-- graphviz
--------------------------------------------------------------------------------------------
drawArgForest :: Forest String -> IO ()
drawArgForest tr = 
  let
    aaa :: Tree String -> String
    aaa tt = case tt of 
      Node a [] -> a 
      Node a list -> intercalate "\n" (map  (\x -> x ++"->"++a) (map aaa list))
  in 
   do writeFile "model.dot" ("digraph  model{\n" ++ (intercalate "\n" $ map aaa tr) ++ "}")  

test_graph_forest = graphviz$drawArgForest$ wbaf2forest $ afe2exbafe $ Input_Models.sample_af
test_graph_forest2 = graphviz$drawArgForest$ wbaf2forest $ afe2exbafe $ trans_baf2af $ Input_Models.sample_baf
test_graph_forest3 = graphviz$drawArgForest$ wbaf2forest $ afe2exbafe $ trans_exbaf2af $ Input_Models.sample_exbaf




graphviz :: (Show a) =>   IO a -> IO ProcessHandle
graphviz x =    do x 
                   runCommand $ "dot -Tpdf model.dot -o model.pdf"
                                ++   " && "
                                ++   " open  model.pdf"

--------------------------------------------------------
--------------------------------------------------------

show_graph_af  =graphviz.drawArgForest. wbaf2forest . afe2exbafe 
show_graph_baf  =graphviz.drawArgForest. wbaf2forest . afe2exbafe .trans_baf2af
show_graph_exbaf  =graphviz.drawArgForest. wbaf2forest . afe2exbafe .trans_exbaf2af

cal_af :: AFe -> AR -> String
cal_af af root =  "PRO="++ show (alpha af root)++ " " ++"CON=" ++ show (beta af root)

-- cal_af sample_af "a1"

-- cal_baf baf root = 
--  let
--   af = trans_baf2af baf
--  in 
--   (alpha af root,beta af root)
-- cal_exbaf

cal_exbaf :: ExBAFe -> AR -> String
cal_exbaf exbaf root = 
 let
  af = trans_exbaf2af exbaf
 in 
  "PRO="++ show (alpha af root)++ " " ++"CON=" ++ show (beta af root)

-- cal_exbaf sample_exbafe "生活安定"
