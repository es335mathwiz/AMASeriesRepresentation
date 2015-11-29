(* Wolfram Language Test file *)
linMod={{{0}},{{0., 0.6926315789473684, 0.34202807765803783}, 
  {0., 0.36, 0.17777143246056068}, {0., 0., 0.9499525011874801}}, 
 {{-0.04442366984873067, 0.658, 0.3600475573573294}, 
  {0.04442366984873067, 0.342, 0.18713718026779125}, {0., 0., 1.}}, 
 {{0.342, 0., -0.12313626461620665}, {-0.342, 0., 0.12313626461620665}, 
  {0., 0., 0.}}, {{0.}, {0.}, {1.0009504513929297}}, 
 {{-0.9988685455324166}, {-0.19718359057668058}, {0.05009757134342517}}, 
 {{1., 0., 0.}, {0., 1., 0.}, {0., 0., 1.}}, {{0}}};
 anXtm1EpsZ={1, .18, 1.1, 0.01, 0.01, -.02, .0001};
 	X0Z0=genX0Z0Funcs[linMod];
 	rbcEqnsFunctionalNext=Compile[
{
{ctm1,_Real},{kktm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*((1.*thetatp1)/cctp1))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)}];


theLilFunc=Private`genLilXkZkFunc[linMod, {X0Z0},X0Z0@@anXtm1EpsZ];
theFR=Private`genFRFunc[{3,1,3},theLilFunc,rbcEqnsFunctionalNext];
theFP=Private`genFPFunc[linMod,{X0Z0},X0Z0@@anXtm1EpsZ,rbcEqnsFunctionalNext];

{xzFunc01,iterXZFuncsPF01}=doIterPF[linMod,{X0Z0},X0Z0@@anXtm1EpsZ,rbcEqnsFunctionalNext]
aPath01=genPath[xzFunc01,Drop[iterXZFuncsPF01,1],Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]}];
cPath01=Private`genPathCompare[linMod,xzFunc01,Drop[iterXZFuncsPF01,1],Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]}];



{xzFunc02,iterXZFuncsPF02}=doIterPF[linMod,iterXZFuncsPF01,X0Z0@@anXtm1EpsZ,rbcEqnsFunctionalNext]
aPath02=genPath[xzFunc02,Drop[iterXZFuncsPF02,1],Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]}];
cPath02=Private`genPathCompare[linMod,xzFunc02,Drop[iterXZFuncsPF02,1],Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]}];



{xzFunc03,iterXZFuncsPF03}=doIterPF[linMod,iterXZFuncsPF02,X0Z0@@anXtm1EpsZ,rbcEqnsFunctionalNext]
aPath03=genPath[xzFunc03,Drop[iterXZFuncsPF03,1],Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]}];
cPath03=Private`genPathCompare[linMod,xzFunc03,Drop[iterXZFuncsPF03,1],Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]}];

Test[
	Chop[Norm[rbcEqnsFunctionalNext@@Append[Flatten[aPath01[[Range[9]]]],anXtm1EpsZ[[4]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-2MM51101-H4ZZR9"
]


Test[
	Chop[Norm[aPath01[[Range[9]]]-cPath01[[2,Range[9]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H6L9U2"
]

Print[{Length[aPath02],Length[cPath02[[2]]]}]

Test[
	Chop[Norm[aPath02[[Range[9]]]-cPath02[[2,Range[9]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151177-H6L9U2"
]

Test[
	Chop[Norm[aPath03[[Range[9]]]-cPath03[[2,Range[9]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-70151177-H6L9U2"
]


Test[
	Chop[Norm[rbcEqnsFunctionalNext@@Append[Flatten[aPath02[[Range[9]]]],anXtm1EpsZ[[4]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H4ZZR9"
]




Test[
	Chop[Norm[rbcEqnsFunctionalNext@@Append[Flatten[aPath02[[3+Range[9]]]],0]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H4V0R9"
]



Test[
	Chop[Norm[rbcEqnsFunctionalNext@@Append[Flatten[aPath03[[Range[9]]]],anXtm1EpsZ[[4]]]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H4MMR9"
]



Test[
	Chop[Norm[rbcEqnsFunctionalNext@@Append[Flatten[aPath03[[3+Range[9]]]],0]]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-HTT0R9"
]





Test[
	Chop[Norm[rbcEqnsFunctionalNext@@Append[Flatten[aPath03[[6+Range[9]]]],0]]]==0
	,
	True
	,
	TestID->"TestGenPath-201XX101-HTT0R9"
]