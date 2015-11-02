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
	iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[4]]],1,5]
	,
	{{1}, {0.18}, {1.1}, {0.3921887121940096}, {0.2042476292306595}, {1.1057730363825737}, {0.41078392534332286}, {0.21401979618704192}, {1.1068456916955154}, {0.4187836324784842}, {0.21821716328989457}, {1.1096258211181955}, {0.4229558873001222}, {0.22042095872723413}, {1.1129237777310605}, {0.42588599798923116}, {0.221983535659673}, {1.1166485805582018}}
	,
	TestID->"TestIterDRPF-20151101-A5U5A6"
]




Test[
	iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[4]]],1,5]
	,
	{{1}, {0.18}, {1.1}, {0.3921887121940096}, {0.2042476292306595}, {1.1057730363825737}, {0.41078392534332286}, {0.21401979618704192}, {1.1068456916955154}, {0.4187836324784842}, {0.21821716328989457}, {1.1096258211181955}, {0.4229558873001222}, {0.22042095872723413}, {1.1129237777310605}, {0.42588599798923116}, {0.221983535659673}, {1.1166485805582018}}
	,
	TestID->"TestIterDRPF-2015tt01-A5U5A6"
]


Test[
	iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[4]]],1,5]
	,
	{{1}, {0.18}, {1.1}, {0.3921887121940096}, {0.2042476292306595}, {1.1057730363825737}, {0.41078392534332286}, {0.21401979618704192}, {1.1068456916955154}, {0.4187836324784842}, {0.21821716328989457}, {1.1096258211181955}, {0.4229558873001222}, {0.22042095872723413}, {1.1129237777310605}, {0.42588599798923116}, {0.221983535659673}, {1.1166485805582018}}
	,
	TestID->"TestIterDRPF-2015nn01-A5U5A6"
]