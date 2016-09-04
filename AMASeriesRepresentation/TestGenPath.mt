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

aGSpec={{1}, 1, {{4, 0.018732441104784652, 0.7492976441913861}, {4, 9/10, 11/10}, 
  {4, -0.03, 0.09}}}
thePFDist={{{ee, PerfectForesight}}}

{xzFunc01,iterXZFuncsPF01}=parallelDoIterREInterp[{genFRFunc},linMod,{X0Z0,2},rbcEqnsFunctionalNext,aGSpec,thePFDist]
aPath01=genPath[xzFunc01,{iterXZFuncsPF01,2},Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]},3];
iPath01=iterateDRPF[xzFunc01,Transpose[{anXtm1EpsZ[[Range[4]]]}],1,3]
intPath01=iterateDRREIntegrate[xzFunc01,Transpose[{anXtm1EpsZ[[Range[4]]]}],thePFDist,3]



{xzFunc02,iterXZFuncsPF02}=parallelDoIterREInterp[{genFRFunc},linMod,{iterXZFuncsPF01,2},rbcEqnsFunctionalNext,aGSpec,thePFDist]
aPath02=genPath[xzFunc02,{iterXZFuncsPF02,2},Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]},3];

iPath02=iterateDRPF[xzFunc02,Transpose[{anXtm1EpsZ[[Range[4]]]}],1,3]
intPath02=iterateDRREIntegrate[xzFunc02,Transpose[{anXtm1EpsZ[[Range[4]]]}],thePFDist,3]




{xzFunc03,iterXZFuncsPF03}=parallelDoIterREInterp[{genFRFunc},linMod,{iterXZFuncsPF02,2},rbcEqnsFunctionalNext,aGSpec,thePFDist]
aPath03=genPath[xzFunc03,{iterXZFuncsPF03,2},Transpose[{anXtm1EpsZ[[Range[3]]]}],{anXtm1EpsZ[[{4}]]},3];

iPath03=iterateDRPF[xzFunc03,Transpose[{anXtm1EpsZ[[Range[4]]]}],1,3]
intPath03=iterateDRREIntegrate[xzFunc03,Transpose[{anXtm1EpsZ[[Range[4]]]}],thePFDist,3]





Test[
	Chop[Norm[aPath01-iPath01]]==0
	,
	True
	,
	TestID->"TestGenPath-20151101-H6L9U2"
]



Test[
	Chop[Norm[intPath01-iPath01]]==0
	,
	True
	,
	TestID->"TestGenPath-2015134101-H6L9U2"
]



Test[
	Chop[Norm[aPath01-intPath01]]==0
	,
	True
	,
	TestID->"TestGenPath-2018851101-H6L9U2"
]



Test[
	Chop[Norm[aPath02-iPath02]]==0
	,
	True
	,
	TestID->"TestGenPath-20251102-H6L9U2"
]



Test[
	Chop[Norm[intPath02-iPath02]]==0
	,
	True
	,
	TestID->"TestenPath-20251102-H6L9U2"
]



Test[
	Chop[Norm[aPath02-intPath02]]==0
	,
	True
	,
	TestID->"TestGenPath-20251102-H6LU2"
]



Test[
	Chop[Norm[aPath03-iPath03]]==0
	,
	True
	,
	TestID->"TestGenPath-20351103-H6L9U2"
]



Test[
	Chop[Norm[intPath03-iPath03]]==0
	,
	True
	,
	TestID->"TestGenPath-2051103-H6L9U2"
]



Test[
	Chop[Norm[aPath03-intPath03]]==0
	,
	True
	,
	TestID->"TestGenPath-20351103-HL9U2"
]



