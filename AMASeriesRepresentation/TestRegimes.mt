(* Wolfram Language Test file *)
Get["AMASeriesRepresentation/prepBmRSwitch.mth"]

chkRes00=checkLinMod[bmRSwitchLinMod,an0XVec,anEps0];
chkRes01=checkLinMod[bmRSwitchLinMod,an0XVec,anEps1];
chkRes10=checkLinMod[bmRSwitchLinMod,an1XVec,anEps0];
chkRes11=checkLinMod[bmRSwitchLinMod,an1XVec,anEps1];
Test[
	{1., 0.99, 0., 0.}==chkRes00[[1]]==chkRes01[[1]]==chkRes10[[1]]==chkRes11[[1]]
	,
	True
	,
	TestID->"TestRegimes-20151212-D2J2K3"
]
Test[
	 {0.33333333333333337, 0., 0., 0.}==chkRes00[[2]]==chkRes01[[2]]==chkRes10[[2]]==chkRes11[[2]]
	,
	result
	,
	TestID->"TestRegimes-20151212-A5M9U7"
]