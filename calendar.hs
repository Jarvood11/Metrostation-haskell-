module Calendar where 
import Prelude (Int, Char,String,Show(..),(++))
data Date=Date Year Month Day
data Year =Year Int 
data   Month =January|February|March|
       April | May | June | Jule | August |September |October | November
       | December
data Day=Day Int 
data Week=Monday|Tuesday
          |Wensday|Tursday|Friday|Saturday|Sunday
data Time=Time Hour Minute Second
data Hour =Hour Int 
data Minute=Minute Int 
data Second= Second Int 
instance Show Week where
	show Monday="Mon"
	show Tuesday ="Tue"
	show Wensday="Wed"
	show Tursday="Thu"
	show Friday="Fri"
	show Saturday="Sat"
	show Sunday="Sun"
	
instance Show Time where 
	show (Time h m s )=show h++ ":"++show m++":"++show s
instance Show Hour where 
	show (Hour h)=addZero(show h)
instance Show Minute where 
	show (Minute m)=addZero(show m)
instance Show Second where 
	show (Second s)=addZero(show s)
addZero::String->String
addZero(a:[])='0' : a :[]
addZero as=as
instance Show Month where 
	show January="Jan"
	show February="Feb"
	show March="Mar"
	show April="Apr"
	show May="May"
	show June="Jun"
	show Jule ="Jul"
	show August="Aug"
	show September="Sep"
	show October="Oct"
	show November="Nov"
	show December="Dec"
	