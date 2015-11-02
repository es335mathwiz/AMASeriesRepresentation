(* Wolfram Language Test file *)
linMod={{{0., 0.6926315789473684, 0.34202807765803783}, 
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
{xzFunc,iterXZFuncsPF}=doIterPF[linMod,{X0Z0},X0Z0@@anXtm1EpsZ,rbcEqnsFunctionalNext]
anXZFuncPF=genXZFuncPF[{3,1,3},theFP];


Test[
	Chop[Norm[iterXZFuncsPF[[1]] @@anXtm1EpsZ-xzFunc @@Append[anXtm1EpsZ[[Range[3]]],0]]]==0
	,
	True
	,
	TestID->"TestDoIterPF-20151101-C2H9V8"
]


Test[
	anXZFuncPF @@anXtm1EpsZ
	,
	{{0.3883382263359695}, {0.20216347433348605}, {1.09477041083488}, {0.004769815399172405}, {-0.0008300529768195824}, {-0.0002749118147733487}}
	,
	TestID->"TestDoIterPF-20151101-A7A1F3"
]



Test[
	iterXZFuncsPF[[1]] @@anXtm1EpsZ
	,
	{{0.3883382263359695}, {0.20216347433348605}, {1.09477041083488}, {0.004769815399172405}, {-0.0008300529768195824}, {-0.0002749118147733487}}
	,
	TestID->"TestDoIterPF-20151101-C3L9F3"
]
