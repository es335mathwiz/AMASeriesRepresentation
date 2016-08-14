(* Wolfram Language Test file *)
Off[InterpolatingFunction::dmval]
{xzFunc01,iterXZFuncsPF01}=doIterREInterp[{genFRFunc},linModBetter,{genX0Z0Funcs[linModBetter],2},rbcEqnsFunctionalNextBetter,aGSpecBetter,theDistBetter];
aPath01=genPath[xzFunc01,{iterXZFuncsPF01,1},anXBetter,{Last[anXEpsBetter]}];
cPath01=genPathCompare[linModBetter,xzFunc01,{iterXZFuncsPF01,1},anXBetter,{Last[anXEpsBetter]}];
	

{xzFunc02,iterXZFuncsPF02}=doIterREInterp[{genFRFunc},linModBetter,{iterXZFuncsPF01,2},rbcEqnsFunctionalNextBetter,aGSpecBetter,theDistBetter];
aPath02=genPath[xzFunc02,{iterXZFuncsPF01,1},anXBetter,{Last[anXEpsBetter]}];
cPath02=genPathCompare[linModBetter,xzFunc02,{iterXZFuncsPF01,1},anXBetter,{Last[anXEpsBetter]}];
	

	

{xzFunc03,iterXZFuncsPF03}=doIterREInterp[{genFRFunc},linModBetter,{iterXZFuncsPF02,2},rbcEqnsFunctionalNextBetter,aGSpecBetter,theDistBetter];
aPath03=genPath[xzFunc03,{iterXZFuncsPF02,1},anXBetter,{Last[anXEpsBetter]}];
cPath03=genPathCompare[linModBetter,xzFunc03,{iterXZFuncsPF02,1},anXBetter,{Last[anXEpsBetter]}];
	



pathErrs01=pathErrsDRPF[xzFunc01,anXEpsBetter,1,rbcEqnsFunctionalNextBetter,4]

pathErrs02=pathErrsDRPF[xzFunc02,anXEpsBetter,1,rbcEqnsFunctionalNextBetter,4]
pathErrs03=pathErrsDRPF[xzFunc03,anXEpsBetter,1,rbcEqnsFunctionalNextBetter,4]

Test[
	iterateDRPF[xzFunc01,anXEpsBetter,1,5]
	,
{{0.2}, {0.18}, {1.}, {1.1}, {0.3823752007368995}, {0.19688000940496103}, 
 {3.104763112520999}, {1.1057730363825737}, {0.40155041573282096}, 
 {0.2077160365221825}, {2.7871798879345566}, {1.1004011178568065}, 
 {0.40910360869710827}, {0.21163240839534148}, {2.682652858131731}, 
 {1.0953134352836789}, {0.40964271963301846}, {0.21179814615507933}, 
 {2.6703926035505376}, {1.09049494932922}, {0.40799812657533935}, 
 {0.2109868992130031}, {2.670058536251604}, {1.0859314164122982}},
 	TestID->"TestIterDRPF-20151101-A5U5A6"
]



Test[
	Norm[iterateDRPF[xzFunc02,anXEpsBetter,1,5]-{{0.2}, {0.18}, {1.}, {1.1}, {0.37765005486427794}, {0.20160515527758266}, 
 {3.2400064184214816}, {1.1057730363825737}, {0.40582431725913853}, 
 {0.2123856114069768}, {2.7115852262475126}, {1.1004011178568065}, 
 {0.4098981920718119}, {0.2149522426301472}, {2.6798216622077464}, 
 {1.0953134352836789}, {0.4094656871175734}, {0.21488745559049105}, 
 {2.6725720154148997}, {1.09049494932922}, {0.40766297050406297}, 
 {0.2140207665328347}, {2.6731802229321175}, {1.0859314164122982}}]<10^(-7)
	,
	True
 		,
	TestID->"TestIterDRPF-2015tt01-A775A6"
]


Test[
Norm[iterateDRPF[xzFunc03,anXEpsBetter,1,5]-{{0.2}, {0.18}, {1.}, {1.1}, {0.3772913272676964}, {0.20196388287416417}, 
 {3.257327436870234}, {1.1057730363825737}, {0.4060001171267794}, 
 {0.21252735883721025}, {2.7106915324144403}, {1.1004011178568065}, 
 {0.4099797084956437}, {0.21499562154206728}, {2.6794100298360033}, 
 {1.0953134352836789}, {0.4095087072487338}, {0.21488248898702597}, 
 {2.6723424610784585}, {1.09049494932922}, {0.40769225845489965}, 
 {0.21398713993320978}, {2.6730000683069655}, {1.0859314164122982}}]<10^(-6)
	,
True,
	TestID->"TestIterDRPF-2015nn01-A5U886"
]







Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXEpsBetter,1,5],oSet=0},
	Norm[pathErrs01[[1]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],Last[anXEpsBetter]]]<10^(-6)]
	,
	True
	,
	TestID->"TestGenPath-2MM51101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXEpsBetter,1,5],oSet=0},
	Norm[pathErrs02[[1]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],Last[anXEpsBetter]]]<10^(-7)]
	,
	True
	,
	TestID->"TestGenPath-2M881101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXEpsBetter,1,5],oSet=0},
	Norm[pathErrs03[[1]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],Last[anXEpsBetter]]]<10^(-7)]
	,
	True
	,
	TestID->"TestGenPath-20981101-H4uuR9"
]


Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXEpsBetter,1,5],oSet=4},
	Norm[pathErrs01[[2]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],0*Last[anXEpsBetter]]]<10^(-7)]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXEpsBetter,1,5],oSet=4},
	Norm[pathErrs02[[2]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],0*Last[anXEpsBetter]]]<10^(-7)]
	,
	True
	,
	TestID->"TestGenPath-oi094101-H4uuR9"
]
Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXEpsBetter,1,5],oSet=4},
	Norm[pathErrs03[[2]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],0*Last[anXEpsBetter]]]<10^(-7)]
	,
	True
	,
	TestID->"TestGenPath-20094101-H488R9"
]


Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXEpsBetter,1,5],oSet=8},
	Norm[pathErrs01[[3]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],0*Last[anXEpsBetter]]]<10^(-7)]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4u099"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXEpsBetter,1,5],oSet=8},
	Norm[pathErrs02[[3]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],0*Last[anXEpsBetter]]]<10^(-7)]
	,
	True
	,
	TestID->"TestGenPath-oi094101-uyuuR9"
]
Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXEpsBetter,1,5],oSet=8},
	Norm[pathErrs03[[3]]-rbcEqnsFunctionalNextBetter@@Join[Flatten[pathNow[[oSet+Range[12]]]],0*Last[anXEpsBetter]]]<10^(-7)]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4llR9"
]


Test[
	pathNow=pathErrsDRPF[xzFunc01,anXEpsBetter,1,rbcEqnsFunctionalNextBetter,3]
	,
	{{{-0.08189880493023338}, {-0.017181131282808515}, {0.21290968099204965}, {0.}}, {{-0.018127621279607986}, {-0.0037379686747589513}, {0.046798917717907784}, {0.00017311101357564418}}}
	,
	TestID->"TestGenPath-ttuiM59971-H487R9"
]
On[InterpolatingFunction::dmval]