module Metro where 

data Station =St Way Name 
	deriving (Show,Eq)
data Way=Blue|Black|Green|Red|Orange
	deriving(Show,Eq)
data Name=Kosmodrom|UlBylichova|Zvezda|Zapad|Ineva|De|Krest|Rodnik|Vostok|Yug|Sirius|Til|TrollevMost|Prizrak|TainstvenniyLes|DnoBolota|PlBakha|Lao|Sever|PlShekspira
	deriving (Show,Eq)

data Point=Point 
	{
	px::Double,
	py::Double 
	}
	deriving(Show,Eq)
place::Name->Point 
place x=uncurry Point $ case x of 
	Kosmodrom ->(-3,7)
	UlBylichova->(-2,4)

	Zvezda->(0,1)
	Zapad->(1,7)
	Ineva->(0.5,4)
	De->(0,-1)
	Krest->(0,-3)
	Rodnik->(0,-5)
	Vostok->(-1,-7)
	Yug->(-7,-1)
	Sirius->(-3,0)
	Til->(3,2)
	TrollevMost->(5,4)
	Prizrak->(8,6)
	TainstvenniyLes->(11,7)
	DnoBolota->(-7,-4)
	PlBakha->(-3,-3)
	Lao->(3.5,0)
	Sever->(6,1)
	PlShekspira->(3,-3)

dist::Point->Point->Double
dist a b =sqrt$(px a-px b)^2+(py a-py b)^2

stationDist::Station->Station->Double
stationDist (St n a)(St m b)
	|n/=m&&a==b=penalty
	|otherwise =dist(place a)(place b)
	where penalty=1

metroMap::Station->[Station]
metroMap x=case x of
	St Black Kosmodrom->[St Black UlBylichova]
	St Black UlBylichova->[St Black Kosmodrom,St Black Zvezda,St Red UlBylichova]
	St Black Zvezda->[St Black UlBylichova,St Blue Zvezda,St Green Zvezda]

