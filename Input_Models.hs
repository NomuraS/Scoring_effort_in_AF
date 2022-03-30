module Input_Models where 

import Data.Tree

type Nodes a= [Tree a] 
type AR  = String 
type ATT= [(AR,AR)]
type EV = AR -> Int
type SUP = [(AR,AR)]
type SSUP = [([AR],AR)]
-- type NormalRel= [(AR,AR)]

data AFe =AFe [AR] ATT EV
data BAFe = BAFe [AR] ATT SUP EV
data ExBAFe = ExBAFe [AR] ATT SSUP EV
  deriving (Eq,Show)

instance Show AFe where
  show afe@(AFe ars att ev) = 
    "AR: " ++ show ars  ++"\n"++ 
    "ATT: " ++ show att  ++"\n" ++ 
    "ev: " ++ unwords(["ev(" ++ x ++")=" ++ show ( ev x) ++"\n"| x<-ars])

instance Show (a -> b) where
  show _  =  "<function>"

instance Eq (a -> b) where
   _ ==_  =  1==1

-- ttest_att = [("x","root"),("b","root"),("a","b")]
-- ttest_afe = AFe ["x","root","b","a"] ttest_att (\x->1)

sample_ar =["a"++ (show x) | x <- [1..7]] ++["b"++ (show x) | x <- [1..8]]
sample_att = [ ("c2","c1"),("c1","a1"),
              ("a3","b2"),("b2","a2"),("a2","b1"),("b1","a1"),
              ("b7","a7"),("a7","b6"),("b6","a6"),("a6","b5"),("b5","a1"),
              ("b8","a1")]
sample_ev a =  case a of 
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

sample_af = AFe  sample_ar sample_att sample_ev



sample_baf_ar =["a"++ (show x) | x <- [1..7]] ++["b"++ (show x) | x <- [1..8]]
sample_baf_att = [ ("c2","c1"),("c1","a1"),
              ("a3","b2"),("b2","a2"),("a2","b1"),("b1","a1"),
              ("b7","a7"),("a7","b6"),("b6","a6"),("a6","b5"),("b5","a1")]

sample_baf_sup = [("b8","a1")      ]

sample_baf_ev a =  case a of 
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



sample_baf = BAFe  sample_baf_ar sample_baf_att sample_baf_sup sample_baf_ev





sample_exbaf_ar =["生活安定","動力政策","脱炭素","原発再稼働","安全原発","原発撤去",	"代替資源","教育支援","環境構築","魅力増加","費用問題","奨学金拡充","授業料減","無償化","予算確保"]
sample_exbaf_att = [
					("原発撤去","原発再稼働"),
					("費用問題","環境構築")
					]

sample_exbaf_ssup = [(["魅力増加"],"環境構築"),(["環境構築"],"教育支援") ,(["教育支援"],"生活安定"),
					(["奨学金拡充","授業料減"],"費用問題"),
					(["予算確保","無償化"],"費用問題"),
					(["代替資源"],"原発撤去"),(["安全原発"],"原発再稼働"),(["原発再稼働"],"動力政策"),(["脱炭素"],"動力政策"),(["動力政策"],"生活安定")
					  ]

sample_exbaf_ev a =  case a of 
	"生活安定"->0
	"動力政策"->2
	"脱炭素"->1
	"原発再稼働"->2
	"安全原発"->3
	"原発撤去"->2
	"代替資源"->3
	"教育支援"->0
	"環境構築"->2
	"魅力増加"->3
	"費用問題"->1
	"奨学金拡充"->1
	"授業料減"->2
	"無償化"->1
	"予算確保"->1
	_->0

sample_exbaf = ExBAFe  sample_exbaf_ar sample_exbaf_att sample_exbaf_ssup sample_exbaf_ev

	
				
					
						
					
				
			
				
				
				
					  

