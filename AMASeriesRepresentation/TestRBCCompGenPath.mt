(* Wolfram Language Test file *)

(* Wolfram Language Test file *)
linMod={{{0., 0.6926315789473684, 0., 0.34202807765803783}, 
  {0., 0.36, 0., 0.17777143246056068}, {0., -5.337627424454331, 0., 0.}, 
  {0., 0., 0., 0.9499525011874801}}, 
 {{-0.04442366984873067, 0.658, 0., 0.3600475573573294}, 
  {0.04442366984873067, 0.342, 0., 0.18713718026779125}, 
  {0.3423421710570143, -5.070746053231615, 1., 0.}, {0., 0., 0., 1.}}, 
 {{0., 0., -0.044379268383314775, 0.}, {0., 0., 0.044379268383314775, 0.}, 
  {0., 0., 0.342, 0.}, {0., 0., 0., 0.}}, {{0.}, {0.}, {0.}, 
  {1.0009504513929297}}, {{-3.7735033942335736}, {-0.19718359057668058}, 
  {2.7774108713298378}, {0.05009757134342517}}, {{1., 0., 0., 0.}, {0., 1., 0., 
 0.}, {0., 0., 1., 0.}, {0., 0., 0., 1.}}, {{0}}};
 anXtm1EpsZ={1, .18, 1.0,1.1, 0.01, 0.01, -.02,0.0, .0001};
 	X0Z0=genX0Z0Funcs[linMod];
 

rbcComp=Compile[{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thtm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{tht,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thtp1,_Real},
{epsVal,_Real}},
{cct^(-1) - (0.342*nltp1)/kkt^(16/25), cct + kkt - 1.*kktm1^(9/25)*tht, 
 nlt - (1.*tht)/cct, tht - 1.*2.718281828459045^epsVal*thtm1^(19/20)}]
 probDims={4,1,4};

theLilFunc=Private`genLilXkZkFunc[linMod, {X0Z0},X0Z0@@anXtm1EpsZ];
theFR=Private`genFRFunc[probDims,theLilFunc,rbcComp];
theFP=Private`genFPFunc[linMod,{X0Z0},X0Z0@@anXtm1EpsZ,rbcComp];

{xzFunc01,iterXZFuncsPF01}=doIterPF[linMod,{X0Z0},X0Z0@@anXtm1EpsZ,rbcComp]
aPath01=genPath[xzFunc01,Drop[iterXZFuncsPF01,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];
cPath01=Private`genPathCompare[linMod,xzFunc01,Drop[iterXZFuncsPF01,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];



{xzFunc02,iterXZFuncsPF02}=doIterPF[linMod,iterXZFuncsPF01,X0Z0@@anXtm1EpsZ,rbcComp]
aPath02=genPath[xzFunc02,Drop[iterXZFuncsPF02,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];
cPath02=Private`genPathCompare[linMod,xzFunc02,Drop[iterXZFuncsPF02,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];



{xzFunc03,iterXZFuncsPF03}=doIterPF[linMod,iterXZFuncsPF02,X0Z0@@anXtm1EpsZ,rbcComp]
aPath03=genPath[xzFunc03,Drop[iterXZFuncsPF03,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];
cPath03=Private`genPathCompare[linMod,xzFunc03,Drop[iterXZFuncsPF03,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];

Test[
	Chop[Norm[rbcComp@@Append[Flatten[aPath01[[Range[12]]]],anXtm1EpsZ[[5]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-2MM51101-H4ZZR9"
]


Test[
	Chop[Norm[aPath01[[Range[12]]]-cPath01[[2,Range[12]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H6L9U2"
]

Print[{Length[aPath02],Length[cPath02[[2]]]}]

Test[
	Chop[Norm[aPath02[[Range[12]]]-cPath02[[2,Range[12]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151177-H6L9U2"
]

Test[
	Chop[Norm[aPath03[[Range[12]]]-cPath03[[2,Range[12]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-70151177-H6L9U2"
]


Test[
	Chop[Norm[rbcComp@@Append[Flatten[aPath02[[Range[12]]]],anXtm1EpsZ[[5]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H4ZZR9"
]




Test[
	Chop[Norm[rbcComp@@Append[Flatten[aPath02[[4+Range[12]]]],0]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H4V0R9"
]



Test[
	Chop[Norm[rbcComp@@Append[Flatten[aPath03[[Range[12]]]],anXtm1EpsZ[[5]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H4MMR9"
]



Test[
	Chop[Norm[rbcComp@@Append[Flatten[aPath03[[4+Range[12]]]],0]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-HTT0R9"
]





Test[
	Chop[Norm[rbcComp@@Append[Flatten[aPath03[[8+Range[12]]]],0]]]==0
	,
	True
	,
	TestID->"TestGenPath-201XX101-HTT0R9"
]