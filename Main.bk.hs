module Arg where 
import Data.List
import Utility 
import Input_WBAF
import WBAF
import Data.Ratio
import Data.Tree
import System.IO
import System.Process
import System.IO
import System.Process
import Debug.Trace
import Split
-- import Data.Char
-- import Data.String
-- import Network.HTTP.Client
-- import Algebra.Graph.Labelled
-- import Data.EdgeTree
-- import Data.LabeledTree
-- import Text.Parsec
-- import Text.Parsec.String

ratiotest ::Ratio Integer
ratiotest = (3 % 3) - (4 % 2)

isPartiallyOrderedSet::(Ord a)=> [(a,a)]->Bool
isPartiallyOrderedSet rel = True
------------------------------------------------------------------------------------------
----- make forest from sup
------------------------------------------------------------------------------------------

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

sup2forest ::(Eq a) => WBAF a-> Forest a
sup2forest  wbaf@(WBAF _ _ sup _) =
   let
    bunkai ::(Eq a) =>SUP a->[(Arg a ,Arg a)]
    bunkai xsup =concat $ nub $ map (\(xs,y)->  map (\z-> (z,y)) xs) xsup
    transREL2Node::(Eq a) =>[(Arg a ,Arg a )]-> Forest a  
    transREL2Node ysup = 
            map (\(x,y)-> Node x [Node y []]) ysup
    aa = map (\x-> Node x []) (extractKeys4sup wbaf)
    bb = transREL2Node $ bunkai sup
   in
    makeForest bb aa 

test_sup2forest = sup2forest input_WBAF

wbaf2forest_allpath ::(Eq a) => WBAF a-> Forest a
wbaf2forest_allpath  wbaf@(WBAF _ att sup _) =
   let
    bunkai ::(Eq a) =>SUP a->[(Arg a ,Arg a)]
    bunkai xsup =concat $ nub $ map (\(xs,y)->  map (\z-> (z,y)) xs) xsup
    transREL2Node::(Eq a) =>[(Arg a ,Arg a )]-> Forest a  
    transREL2Node ysup = 
            map (\(x,y)-> Node x [Node y []]) ysup
    aa = map (\x-> Node x []) (extractKeys4sup wbaf)
    aa' = map (\x-> Node x []) (extractKeys4att wbaf)
    bb = transREL2Node $ nub$ (bunkai sup)++att
   in
    makeForest bb (aa++aa') 

sup2forestM ::(Eq a) => NormalRel a-> Forest (Arg a)
sup2forestM sup =
   let
    transREL2Node::[(Arg a,Arg a)]-> Forest (Arg a)
    transREL2Node ysup = 
            map (\(x,y)-> Node x [Node y []]) ysup
    aa = map (\x-> Node x []) (extractKeys4supM sup)
    bb = transREL2Node $ sup
   in
    makeForest bb aa 

tree2paths ::(Eq a) =>  Tree (Arg a) -> [[Arg a]]
tree2paths  (Node x []) = [[x]]
tree2paths  (Node x list) = (x:) <$> concat [tree2paths l|l<-list]  

forest2paths ::(Eq a) =>  [Tree (Arg a)] -> [[[Arg a]]
forest2paths fr =  map tree2paths fr

test_aaa =  forest2paths $ sup2forest $ input_WBAF

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

-- definition2 new
support_paths::(Eq a)=>WBAF a->[[Arg a]]
support_paths wbaf@(WBAF _ _ sup _) =
  []

attack_paths::(Eq a)=>WBAF a->[[Arg a]]
attack_paths wbaf@(WBAF _ _ sup _) =
  []

-- definition3 new (maximal support path)
maximalSupportPaths :: WBAF String-> [ArgPATH String]
maximalSupportPaths wbaf@(WBAF _ _ sup _) = 
  [["g","d","b","a"],["h","e","b","a"]]

maximalAttackPaths :: WBAF  String-> [ArgPATH String]
maximalAttackPaths wbaf@(WBAF _ _ sup _) =
  [["m","d"],["l","m"]]

-----------------------------------------------
-- p3
-----------------------------------------------
-- makeLowerSubWBAF :: WBAF -> Arg-> ArgPATH -> ArgPATH -> Maybe WBAF
-- makeLowerSubWBAF (WBAF args att sup w) xarg xargpath xsuppath


-- definition 4 new (s-bundle)
extractKeys4sup::(Eq a)=>WBAF a-> [Arg a]
extractKeys4sup wbaf@(WBAF _ _ sup _)=  
    let 
      snds =[y | (_,y) <-sup]
      fsts =[xx | (xx,_) <-sup]
      checkroot x = forall fsts (\yy->  x `notElem` yy)
      rootsStr = nub $ filter  checkroot snds
    in 
       rootsStr

extractKeys4att::(Eq a)=>WBAF a-> [Arg a]
extractKeys4att wbaf@(WBAF _ att _ _)=  
    let 
      snds =[y | (_,y) <-att]
      fsts =[x | (x,_) <-att]
      checkroot x = forall fsts (\yy->  x /= yy)
      rootsStr = nub $ filter  checkroot snds
    in 
       rootsStr


test_extracKeys1 = extractKeys4att input_WBAF
test_extracKeys2 = extractKeys4att input_mWBAF

extractKeys4supM::(Eq a)=>NormalRel a-> [a]
extractKeys4supM sup=  
    let 
      snds =[y | (_,y) <-sup]
      fsts =[x | (x,_) <-sup]
      checkroot x =  x `notElem` fsts
      rootsStr = nub $ filter  checkroot  snds
    in 
       rootsStr

extractLeafs::(Eq a)=>WBAF a-> [a]
extractLeafs wbaf@(WBAF _ _ sup _)=  
    let 
      snds =[y | (_,y) <-sup]
      bunkai = concat $ nub $ map (\(xs,y)->  map (\z-> (z,y)) xs) sup
      fsts =[le | (le,_) <-bunkai]
      rootsStr = nub $ filter  (\x-> x `notElem` snds)  fsts
    in 
       rootsStr

test_extracLeafs = extractLeafs input_WBAF

isLeaf::(Eq a)=>WBAF a->Arg a->Bool
isLeaf wbaf@(WBAF _ _ sup _) arg = 
  arg `elem` (extractLeafs wbaf)

isKey::(Eq a)=>WBAF a->Arg a->Bool
isKey wbaf@(WBAF _ _ sup _) arg = 
  arg `elem` (extractKeys4sup wbaf)

-- definition 4 new (s-bundle)
sup_bundle :: (Eq a)=>WBAF a-> Arg a->[ArgPATH a]
sup_bundle wbaf@(WBAF _ _ sup _) arg = 
  let 
   allpaths = concat $ forest2paths$  sup2forest wbaf
   slicepath :: (Eq a)=>[Arg a]->Arg a->[Arg a]
   slicepath  args ar = case args of 
    []->  []
    x:xs -> if x == ar
            then  args
            else  slicepath xs ar 
  in
   filter (\y-> y /= []) $ map (\x ->  slicepath x arg) allpaths

test_sup_bundle = sup_bundle input_WBAF "a"

-- strength_of_an_argument def5 
str_sup ::  (Eq a) =>   WBAF  a -> Arg a   -> Float
str_sup wbaf@(WBAF _ _ sup wei) arg
   | isLeaf wbaf arg =  wei arg / rr  -- ok
   | isKey  wbaf arg =  bunshi + wei arg
   | otherwise  =  (bunshi+ wei arg) / rr
   where 
    calcuRR ::(Eq a)=>WBAF a -> Arg a -> Arg a -> Int
    calcuRR wbaf targetArg key= 
            length 
          $ head
          $ endBy [targetArg]
          $ head 
          $ filter (\x -> targetArg `elem` x)  
          $ sup_bundle wbaf key
    -- paths:: (Eq a)=>[[Arg a]]
    paths =  concat $ forest2paths $ sup2forest $ wbaf
    -- the_key::(Eq a1)=> Arg  a1
    the_key = head $head $ filter (\x-> arg `elem` x ) paths
    bunshi = (maximum [value_sup wbaf sa | (sa,_) <- (filter (\(_,y)-> y==arg ) sup) ])
    rr = (fromIntegral $ calcuRR wbaf arg the_key) 

-- v_s
value_sup :: Eq a => WBAF a -> [Arg a] -> Float
value_sup wbaf xsa = 
    sum[(str_sup wbaf  xa) / (fromIntegral $ length xsa)| xa <- xsa]

stra = str_sup input_WBAF  "a" -- 5.375
strb = str_sup input_WBAF  "b" -- 3.75
strc = str_sup input_WBAF  "c" -- 5.0
strd = str_sup input_WBAF  "d" -- 
stre = str_sup input_WBAF  "e" -- 
strf = str_sup input_WBAF  "f" -- 
strg = str_sup input_WBAF  "g" -- 
strh = str_sup input_WBAF  "h" -- 


strength_of_sbundle ::  (Eq a) =>   WBAF  a -> Arg [[a]]   -> (Arg [[a]],Float)
strength_of_sbundle wbaf@(WBAF _ _ _ wei) paths =
  let
   value_sup1 onepath= (value_sup wbaf onepath) + wei (head onepath)
  in 
   (paths, maximum $ map value_sup1 paths)

test_strength_of_sbundle = 
  strength_of_sbundle input_WBAF [["a","b","d","g"],["a","c","f"],["a","c","e"]]

-- type Weight a=(Arg a->Float)

-- definition 6 old
-- valueOfMaxSupportPath ::(Eq a)=> ArgPATH a-> Weight a-> Float
-- valueOfMaxSupportPath [] wei =  0
-- valueOfMaxSupportPath path wei = 
--     let 
--        pathw = map wei (tail path)
--        pathcal = map 
--                   (\(x,y)-> (x / y)) 
--                   (zip pathw [1..])
--     in sum pathcal

-- test_valueOfMaxSupportPath0 = valueOfMaxSupportPath ["a","b","d","e"] input_weight
-- test_valueOfMaxSupportPath1 = valueOfMaxSupportPath ["a","b","d","g"] input_weight
-- test_valueOfMaxSupportPath2 = valueOfMaxSupportPath ["a","c","e"] input_weight
-- test_valueOfMaxSupportPath3 = valueOfMaxSupportPath ["a","c","f"] input_weight


-- -- value_of_a_set_spoorter 
-- valuePathWithRootpair ::(Eq a)=>  [Arg a] ->Weight a-> ([Arg a],Float)
-- valuePathWithRootpair sts weight  = 
--     let 
--       score = valueOfMaxSupportPath sts weight 
--       scoreWithRootWight = score + (weight (head sts))
--     in
--       (sts,scoreWithRootWight)

-- -- -- test_valuePathWithRootpair3 = valuePathWithRootpair ["a","b","d","g"] input_weight --loop
-- -- -- test_valuePathWithRootpair4 = valuePathWithRootpair ["a","c","e"] input_weight

-- maxStrenghtSupportPATH :: (Ord a)=> WBAF a->  Arg [[a]]-> ([Arg a],Float)
-- maxStrenghtSupportPATH wbaf@(WBAF _ _ _ wei) paths =   
--        maximum $ map (\x-> valuePathWithRootpair x wei) paths 

-- test_maxStrenghtSupportPATH = 
--   maxStrenghtSupportPATH input_WBAF [["a","b","d","g"],["a","c","f"],["a","c","e"]]


-- 3.3 meta new
makeMetaWBAF::(Ord a)=> WBAF a->  (WBAF [[a]])
makeMetaWBAF wbaf@(WBAF _ atta _ _) = 
  let
    -- mAR
    mAR = --map (\y-> sort$nub$ concat y) $
          map (\x-> sup_bundle wbaf x) (extractKeys4sup wbaf)
    -- mAtta
    -- from2 arg =head $  filter (\marg-> arg `elem` marg) mAR
    supPath = forest2paths $ sup2forest wbaf
    supPathAll = concat supPath
    fil z list= if filter (\x-> z `elem` x ) list ==[]
                then [] -- error
                else head $ filter (\x-> z `elem` x ) list
    from fr= fil (fil fr supPathAll) supPath
    to t= fil (fil t supPathAll) supPath
    mATT = [(from x, to y) | (x,y) <- atta ] 
    -- mATT = [(from2 x, from2 y) | (x,y) <- atta ] 
    --mWeight
    mWeight x = snd $ strength_of_sbundle wbaf x
  in
   (WBAF mAR mATT [] mWeight)

input_mWBAF = makeMetaWBAF input_WBAF

tesai = extractKeys4att $ input_mWBAF
-- tesai = extractKeys4att $ 
tesai2 = extractLeafs $ input_mWBAF

-- definition 7 new

-- valueOfMaxAttackPath
value_att ::(Eq a) => [Arg a] -> WBAF a -> Float  --([Float],Float)
value_att attPath wbaf@(WBAF _ _ _ wei) =
  let
   attPathtail = tail attPath
   pathdepth x = fromIntegral $ removeMaybe $ fmap (1+) (elemIndex x attPathtail) 
   bunbo x =   fromIntegral $ ceiling $ (pathdepth x)/2  
   calc x =  case  odd $ round $ pathdepth x  of 
     True  -> -   (wei x / bunbo x)
     False ->     (wei x / bunbo x)
   result = map calc attPathtail
  in 
   sum result
   -- (result, sum result)

text_value_att1=value_att ["e","f","g","h"] input_WBAF
text_value_att2=value_att ["e","j","k"] input_WBAF

-- definition 8 new
att_bundle ::(Eq a) => WBAF a-> Arg a->[ArgPATH a]
att_bundle whaf@(WBAF _ att _ _) marg = 
  let 
   allpaths = concat $ forest2paths$  sup2forestM att
   slicepath ::(Eq a) => [Arg a]->Arg a ->[Arg a]
   slicepath  args ar = case args of 
    []->  []
    x:xs ->  if x == ar
           then  args
           else  slicepath xs ar 
  in
   filter (\y-> y /= []) $ map (\x->slicepath x marg) allpaths

test_att_bundle =att_bundle input_WBAF "e"  -- [["e","j","k"],["e","f","g","h"]]


str_att ::(Eq a)=> WBAF  a-> Arg  a -> Float
str_att wbaf@(WBAF _ _ _ wei) ee =
  let
    abundle = att_bundle wbaf ee --[["e","j","k"],["e","f","g","h"]]
    allpaths  =concat $forest2paths $ wbaf2forest_allpath wbaf 
    rr = head $ filter (\xx-> ee `elem` xx) allpaths
    depthlength = length$head$endBy  [ee] rr
    r_even = sum[ (value_att pa wbaf)| pa <- abundle]     + (wei ee)
    r_odd  = maximum[(value_att pa wbaf)| pa <- abundle]  - (wei ee) 
  in 
   case odd depthlength of 
    True -> r_odd
    False  -> r_even
   
str_att_e = str_att input_WBAF "e"
str_att_f = str_att input_WBAF "f"

-- mwbafgraphvis::MWBAF->  IO ()
-- mwbafgraphvis mwbaf@(MWBAF ar at su we) = let
--     
--     nodes = [showMArg a| a <-ar]
--     at2 = map (\(x,y)-> ("red",showMArg  x++"_"++ show (round(we x)),showMArg y++"_"++ show (round(we y))) ) at
--     su2 = map (\(x,y)-> ("blue",showMArg  x++"_"++ show (round(we x)),showMArg y++"_"++ show (round(we y))) ) su
--     ff = intercalate "\n" $ map (\(a,x,y)-> x ++"->"++y++"[color=\""++a++"\"];" )  (at2++su2) 
--   in 
--    do writeFile "model.dot" ("digraph  model{rankdir=LR;label=<>\n" ++ ff ++ "}")  

--------------------------------------------------------------------------------------------
-- graphviz
--------------------------------------------------------------------------------------------

drawForestIO :: (Eq a, Show a)=> Forest a -> IO ()
drawForestIO tr = 
  let
    aaa ::  (Eq a, Show a)=> Tree a -> String
    aaa tt = case tt of 
      Node a [] -> show a 
      Node a list -> intercalate "\n" (map  (\x -> x ++"->"++ show a) (map aaa list))
    arrows ::  String
    arrows = (intercalate "\n" $ map aaa tr)
  in
   do writeFile "model.dot" ("digraph  model{\n" ++ arrows ++ " \n}")  

graphviz :: (Show a) =>  IO a -> IO ProcessHandle
graphviz x =    do x 
                   runCommand $  "dot -Tpdf model.dot -o model.pdf;"
                              ++ " open  model.pdf"

bb = wbaf2forest_allpath input_mWBAF

showGraph_PDF  = graphviz.(drawForestIO.wbaf2forest_allpath) 
showGraph_CUI  = (drawForest.wbaf2forest_allpath)

test_graph_forest1    =  showGraph_PDF input_WBAF
test_graph_forest_all = showGraph_PDF input_WBAF
-- graphviz $ drawForestIO $ wbaf2forest_allpath


