(* Wolfram Language Test file *)

(* Wolfram Language Test file *)
linMod={{{0}},{{0., 0.6926315789473684, 0., 0.34202807765803783}, 
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
 
aGSpec={{1,3}, 1, {{4, 0.018732441104784652, 0.7492976441913861}, {4, 9/10, 11/10}, 
  {4, -0.03, 0.09}}}
thePFDist={{{ee, PerfectForesight}}}


rbcComp=Compile[{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thtm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{tht,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thtp1,_Real},
{epsVal,_Real}},
{cct^(-1) - (0.342*nltp1)/kkt^(16/25), cct + kkt - 1.*kktm1^(9/25)*tht, 
 nlt - (1.*tht)/cct, tht - 1.*2.718281828459045^epsVal*thtm1^(19/20)}]
 probDims={4,1,4};


{xzFunc01,iterXZFuncsPF01}=doIterREInterp[{genFRFunc},linMod,{X0Z0,2},rbcComp,aGSpec,thePFDist]
aPath01=genPath[xzFunc01,{iterXZFuncsPF01,2},Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]},3];
iPath01=iterateDRPF[xzFunc01,Transpose[{anXtm1EpsZ[[Range[5]]]}],1,3]
intPath01=iterateDRREIntegrate[xzFunc01,Transpose[{anXtm1EpsZ[[Range[5]]]}],thePFDist,3]



{xzFunc02,iterXZFuncsPF02}=doIterREInterp[{genFRFunc},linMod,{iterXZFuncsPF01,2},rbcComp,aGSpec,thePFDist]
aPath02=genPath[xzFunc02,{iterXZFuncsPF02,2},Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]},3];

iPath02=iterateDRPF[xzFunc02,Transpose[{anXtm1EpsZ[[Range[5]]]}],1,3]
intPath02=iterateDRREIntegrate[xzFunc02,Transpose[{anXtm1EpsZ[[Range[5]]]}],thePFDist,3]




{xzFunc03,iterXZFuncsPF03}=doIterREInterp[{genFRFunc},linMod,{iterXZFuncsPF02,2},rbcComp,aGSpec,thePFDist]
aPath03=genPath[xzFunc03,{iterXZFuncsPF03,2},Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]},3];

iPath03=iterateDRPF[xzFunc03,Transpose[{anXtm1EpsZ[[Range[5]]]}],1,3]
intPath03=iterateDRREIntegrate[xzFunc03,Transpose[{anXtm1EpsZ[[Range[5]]]}],thePFDist,3]


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
	TestID->"TestGenPath-20151101-H6L92"
]



Test[
	Chop[Norm[aPath01-intPath01]]==0
	,
	True
	,
	TestID->"TestGenPath-2051101-H6L9U2"
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
	TestID->"TestGenPath-20251102-H6L92"
]



Test[
	Chop[Norm[aPath02-intPath02]]==0
	,
	True
	,
	TestID->"TestGenPath-2025102-H6L9U2"
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
	TestID->"TestGenPath-2035103-H6L9U2"
]



Test[
	Chop[Norm[aPath03-intPath03]]==0
	,
	True
	,
	TestID->"TestGenPath-2031103-H6L9U2"
]



