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
 
theDist={{{ee,NormalDistribution[0,0.02]}}};
thePFDist = {{{ee, PerfectForesight}}};


aGSpec={{1,3},1,{{4,.2,.5},{3,.9,1.1},{3,-3*.01,3*.01}}};

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
theFP=genFPFunc[linMod,{X0Z0},X0Z0@@anXtm1EpsZ,rbcComp];
{xzFunc,iterXZFuncsPF}=parallelDoIterREInterp[{genFRFunc},linMod,{X0Z0,2},rbcComp,aGSpec,thePFDist]
anXZFuncPF=genXZFuncRE[probDims,xzFunc,thePFDist];

Off[InterpolatingFunction::dmval]
Test[
	Chop[Norm[iterXZFuncsPF @@anXtm1EpsZ-xzFunc @@Append[anXtm1EpsZ[[Range[4]]],0]]]==0
	,
	True
	,
	TestID->"TestparallelDoIterPF-20151101-C2H9V8"
]


Test[
	tRes=anXZFuncPF @@anXtm1EpsZ;
Norm[tRes-{{0.39053169474454}, {0.20295994949600366}, {2.790200259595386}, 
 {1.0948251498115775}, {-0.00031948810871954003}, {0.0021299382616663393}, 
 {-0.015395838724676217}, {-0.0002201728380758491}}]<10^(-8),
 True,
	TestID->"TestparallelDoIterPF-20151101-A7A1F3"
]



Test[
	tRes=iterXZFuncsPF @@anXtm1EpsZ;
	Norm[tRes-{{0.39053169474454}, {0.20295994949600366}, {2.790200259595386}, 
     {1.0948251498115775}, {-0.00031948810871954003}, 
     {0.0021299382616663393}, {-0.015395838724676217}, 
     {-0.0002201728380758491}}]<10^(-8),
     True
,
	TestID->"TestparallelDoIterPF-20151101-C3L9F3"
]
