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


pathErrs01=pathErrsDRPF[xzFunc01,anXtm1EpsZ[[Range[4]]],1,rbcEqnsFunctionalNext,4]

pathErrs02=pathErrsDRPF[xzFunc02,anXtm1EpsZ[[Range[4]]],1,rbcEqnsFunctionalNext,4]

pathErrs03=pathErrsDRPF[xzFunc03,anXtm1EpsZ[[Range[4]]],1,rbcEqnsFunctionalNext,4]

Test[
	iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[4]]],1,5]
	,
	{{1}, {0.18}, {1.1}, {0.3921887121940096}, {0.2042476292306595}, {1.1057730363825737}, {0.4083650720424685}, {0.21270303019186895}, {1.1002280068432309}, {0.41242588086217097}, {0.21477561410488719}, {1.0949859864343616}, {0.41202242400906564}, {0.2145232303113454}, {1.09002920247021}, {0.41010090591393356}, {0.21348599599441512}, {1.0853410428199433}},
	TestID->"TestIterDRPF-20151101-A5U5A6"
]



Test[
	iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[4]]],1,5]
	,
	{{1}, {0.18}, {1.1}, {0.39235731006153807}, {0.20407903136313102}, {1.1057730363825737}, {0.4084489435233346}, {0.21243455027946306}, {1.1002280068432309}, {0.412427174987713}, {0.21448920291609516}, {1.0949859864343616}, {0.41199363496089375}, {0.21425110309903198}, {1.09002920247021}, {0.41006499941738067}, {0.2132370149160828}, {1.0853410428199433}}
			,
	TestID->"TestIterDRPF-2015tt01-A775A6"
]


Test[
	Chop[Norm[iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[4]]],1,5]-{{1}, {0.18}, {1.1}, {0.3924247522246667}, {0.20401158920000237}, {1.1057730363825737}, {0.40846434276302584}, {0.21234527703299938}, {1.1002280068432309}, {0.41242274398296164}, {0.21439877750624484}, {1.0949859864343616}, {0.41198321619989303}, {0.2141664635169346}, {1.09002920247021}, {0.4100534714765217}, {0.21315988721375612}, {1.0853410428199433}}]]
	,
0,
	TestID->"TestIterDRPF-2015nn01-A5U886"
]







Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[4]]],1,5],oSet=0},
	Chop[Norm[pathErrs01[[1]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],anXtm1EpsZ[[4]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-2MM51101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[4]]],1,5],oSet=0},
	Chop[Norm[pathErrs02[[1]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],anXtm1EpsZ[[4]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-2M881101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[4]]],1,5],oSet=0},
	Chop[Norm[pathErrs03[[1]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],anXtm1EpsZ[[4]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20981101-H4uuR9"
]


Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[4]]],1,5],oSet=3},
	Chop[Norm[pathErrs01[[2]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],0*anXtm1EpsZ[[4]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[4]]],1,5],oSet=3},
	Chop[Norm[pathErrs02[[2]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],0*anXtm1EpsZ[[4]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-oi094101-H4uuR9"
]
Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[4]]],1,5],oSet=3},
	Chop[Norm[pathErrs03[[2]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],0*anXtm1EpsZ[[4]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H488R9"
]


Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[4]]],1,5],oSet=6},
	Chop[Norm[pathErrs01[[3]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],0*anXtm1EpsZ[[4]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4u099"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[4]]],1,5],oSet=6},
	Chop[Norm[{pathErrs02[[3]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],0*anXtm1EpsZ[[4]]]}]]==0]
	,
	True
	,
	TestID->"TestGenPath-oi094101-uyuuR9"
]
Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[4]]],1,5],oSet=6},
	Chop[Norm[pathErrs03[[3]]-rbcEqnsFunctionalNext@@Append[Flatten[pathNow[[oSet+Range[9]]]],0*anXtm1EpsZ[[4]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4llR9"
]


Test[
	pathNow=pathErrsDRPF[xzFunc01,anXtm1EpsZ[[Range[4]]],1,rbcEqnsFunctionalNext,3]
	,
	{{0.0031988823523936283, 0., 0.}, {0.003593666816470442, 0., 0.}}
	,
	TestID->"TestGenPath-ttuiM59971-H487R9"
]
