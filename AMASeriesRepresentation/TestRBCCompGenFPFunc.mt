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



theLilFuncMoreX0Z0=Private`genLilXkZkFunc[linMod, {X0Z0,X0Z0,X0Z0,X0Z0},X0Z0@@anXtm1EpsZ];
theFRMoreX0Z0=Private`genFRFunc[probDims,theLilFuncMoreX0Z0,rbcComp];
theFPMoreX0Z0=Private`genFPFunc[linMod,{X0Z0,X0Z0,X0Z0,X0Z0},X0Z0@@anXtm1EpsZ,rbcComp];

With[{frXtZt=theFR@@anXtm1EpsZ[[Range[5]]],
	fpXtZt=theFP@@anXtm1EpsZ[[Range[5]]]},Print[Chop[Norm[frXtZt-fpXtZt]]]]

(*fixed point irrelevant until more than one XZ*)
Test[
With[{frXtZt=theFR@@anXtm1EpsZ[[Range[5]]],
	fpXtZt=theFP@@anXtm1EpsZ[[Range[5]]]},Chop[Norm[frXtZt-fpXtZt]]==0],
	True
	,
	TestID->"TestGenFRFunc-201101-R6T3M7"
]

(*satisfies system equations*)
Test[
With[{fpXtZt=theFP@@anXtm1EpsZ[[Range[5]]]},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXtm1EpsZ[[Range[5]]],fpXtZt[[4+Range[4]]]]]]},
	Chop[Norm[rbcComp @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-151101-R6T8M7"
]

With[{frXtZt=theFRMoreX0Z0@@anXtm1EpsZ[[Range[5]]],
	fpXtZt=theFPMoreX0Z0@@anXtm1EpsZ[[Range[5]]]},Print[Chop[Norm[frXtZt-fpXtZt]]]]


(*fixed point irrelevant until more than one XZ and Z's non zero*)
Test[
With[{frXtZt=theFRMoreX0Z0@@anXtm1EpsZ[[Range[5]]],
	fpXtZt=theFPMoreX0Z0@@anXtm1EpsZ[[Range[5]]]},Chop[Norm[frXtZt-fpXtZt]]==0],
	True
	,
	TestID->"TestGenFRFunc-20151101-MMT3M7"
]

(*satisfies system equations*)
Test[
With[{fpXtZt=theFPMoreX0Z0@@anXtm1EpsZ[[Range[5]]]},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXtm1EpsZ[[Range[5]]],fpXtZt[[4+Range[4]]]]]]},
	Chop[Norm[rbcComp @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-NNT8M7"
]
