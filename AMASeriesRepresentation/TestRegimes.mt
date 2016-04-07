(* Wolfram Language Test file *)
Get["AMASeriesRepresentation/prepBmRSwitch.mth"]

chkRes00=checkLinMod[bmRSwitchLinMod,an0XVec,anEps0];
chkRes01=checkLinMod[bmRSwitchLinMod,an0XVec,anEps1];
chkRes10=checkLinMod[bmRSwitchLinMod,an1XVec,anEps0];
chkRes11=checkLinMod[bmRSwitchLinMod,an1XVec,anEps1];
Test[
	(*{1., 0.99, 0., 0.}*){ 0.99, 0., 0.,0.}==chkRes00[[1]]==chkRes01[[1]]==chkRes10[[1]]==chkRes11[[1]]
	,
	True
	,
	TestID->"TestRegimes-20151212-D2J2K3"
]
Test[
	 {0.33333333333333337, 0., 0., 0.}==chkRes00[[2]]==chkRes01[[2]]==chkRes10[[2]]==chkRes11[[2]]
	,
	True
	,
	TestID->"TestRegimes-20151212-A5M9U7"
]

Test[
	{{0.}, {0.29552238805970155}, {0.198}, {0.09850746268656718}, {0}, {0}, {0},{0}}==chkRes00[[3]]==chkRes01[[3]]
	
	,
	True
	,
	TestID->"TestRegimes-20151212-G1Y1L4"
]

Test[
{{0.}, {0.29552238805970155}, {0.198}, {0.09850746268656718}, {0}, {0}, {0},{0}}==chkRes10[[3]]==chkRes11[[3]]
	,
	True
	,
	TestID->"TestRegimes-20151212-B4D5E1"
]
chkRes00=checkMod[bmRSwitchLinMod,nGSpec,bmRSwitchDist,an0XVec,anEps0,bmRSwitchEqnFuncs]
Test[
	chkRes00[[1]]=={{0}, {0.1}, {0.2}, {0.3}, {0.}, {0.3104477611940299}, {0.20800000000000002}, 
 {0.10348258706467664}, {0.}, {0.3073432835820896}, {0.20592000000000002}, 
 {0.10244776119402987}, {0.01}, {0}}
		,
	True
	,
	TestID->"TestRegimes-20151==23212-G1Y1L4"
]
Test[
	  Norm[{{0.}, {0.3104477611940299}, 
  {0.20800000000000002}, {0.3449419568822554}, {-4.163336332050206*^-17}, 
  {2.4495390647428346*^-19}, {-0.7243781094527362}, {0.}}-chkRes00[[2]]]
	,
	0.
	,
	TestID->"TestRegimes-20151212-T348J4"
]

chkRes10=checkMod[bmRSwitchLinMod,nGSpec,bmRSwitchDist,an1XVec,anEps0,bmRSwitchEqnFuncs]
Test[
	chkRes10[[1]]=={{1}, {0.1}, {0.2}, {0.3}, {0.}, {0.3104477611940299}, {0.20800000000000002}, 
 {0.10348258706467664}, {0.}, {0.3073432835820896}, {0.20592000000000002}, 
 {0.10244776119402987}, {0.01}, {0}}
		,
	True
	,
	TestID->"TestRegimes-20151==23i92-G1Y1L4"
]
Test[
	  Norm[{{0.}, {0.3104477611940299}, 
  {0.20800000000000002}, {0.3449419568822554}, {-4.163336332050206*^-17}, 
  {2.4495390647428346*^-19}, {-0.7243781094527362}, {0.}}-chkRes10[[2]]]
	,
	0.
	,
	TestID->"TestRegimes-20151212-T6A8J4"
]

(*
*)
chkRes01=checkMod[bmRSwitchLinMod,nGSpec,bmRSwitchDist,an0XVec,anEps1,bmRSwitchEqnFuncs]
Test[
	chkRes01[[1]]=={{0}, {0.1}, {0.2}, {0.3}, {1.}, {0.3104477611940299}, {0.20800000000000002}, 
 {0.10348258706467664}, {0.}, {0.3073432835820896}, {0.20592000000000002}, 
 {0.10244776119402987}, {0.01}, {1}}
		,
			True
	,
	TestID->"TestRegimes-20451==23212-G1Y1L4"
]

Test[
	  Norm[{{0.}, {0.3104477611940299}, 
  {0.20800000000000002}, {0.10348258706467663}, {0.}, {0.}, 
  {5.551115123125783*^-17}, {-1.}}-chkRes01[[2]]]
	,
	0.
	,
	TestID->"TestRegimes-2015ertu2-T6A8J4"
]

chkRes11=checkMod[bmRSwitchLinMod,nGSpec,bmRSwitchDist,an1XVec,anEps1,bmRSwitchEqnFuncs]
Test[
	chkRes11[[1]]=={{1}, {0.1}, {0.2}, {0.3}, {1.}, {0.3104477611940299}, {0.20800000000000002}, 
 {0.10348258706467664}, {0.}, {0.3073432835820896}, {0.20592000000000002}, 
 {0.10244776119402987}, {0.01}, {1}}
		,
	True
	,
	TestID->"TestRegimes-20561==23i92-G1Y1L4"
]

Test[
	  Norm[{{0.}, {0.3104477611940299}, 
  {0.20800000000000002}, {0.10348258706467663}, {0.}, {0.}, 
  {5.551115123125783*^-17}, {-1.}}-chkRes11[[2]]]
	,
	0.
	,
	TestID->"TestRegimes-20151eu2-T6A8J4"
]
