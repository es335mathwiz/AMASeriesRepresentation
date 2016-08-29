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
theLilFunc=genLilXkZkFunc[linMod, {X0Z0,2},X0Z0@@anXtm1EpsZ];
theFR=genFRFunc[probDims,theLilFunc,rbcComp];


theLilFuncMoreX0Z0=genLilXkZkFunc[linMod, {X0Z0,4},X0Z0@@anXtm1EpsZ];
theFRMoreX0Z0=genFRFunc[probDims,theLilFuncMoreX0Z0,rbcComp];

Test[
Norm[theFR@@anXtm1EpsZ[[Range[5]]]-(*produces xtzt for a given xtm1,eps and conditional z functions*)
{{0.3926985637359889}, {0.2037377776886802}, {2.8158316288775245}, {1.1057730363825737}, {-0.005480210873745038}, {-0.000915880995108243}, {-0.003442312382082275}, {0.0007182092189910172}}]<10^(-8),
True
	,
	TestID->"TestGenFRFunc-20151101-L8W9H4"
]


Test[
With[{frXtZt=theFR@@anXtm1EpsZ[[Range[5]]]},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXtm1EpsZ[[Range[5]]],frXtZt[[4+Range[4]]]]]]},
	Chop[Norm[rbcComp @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-R6T3M7"
]





Test[
With[{frXtZt=theFRMoreX0Z0@@anXtm1EpsZ[[Range[5]]]},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXtm1EpsZ[[Range[5]]],frXtZt[[4+Range[4]]]]]]},
	Chop[Norm[rbcComp @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-R63TM7"
]