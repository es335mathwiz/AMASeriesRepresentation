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

(*fixed point irrelevant until more than one XZ*)
Test[
With[{frXtZt=theFR@@anXtm1EpsZ[[Range[4]]],
	fpXtZt=theFP@@anXtm1EpsZ[[Range[4]]]},Chop[Norm[frXtZt-fpXtZt]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-R6T3M7"
]

(*satisfies system equations*)
Test[
With[{fpXtZt=theFP@@anXtm1EpsZ[[Range[4]]]},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXtm1EpsZ[[Range[4]]],fpXtZt[[3+Range[3]]]]]]},
	Chop[Norm[rbcEqnsFunctionalNext @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-R6T8M7"
]
