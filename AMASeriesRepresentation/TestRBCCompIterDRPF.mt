(* Wolfram Language Test file *)
linMod={{{0., 0.6926315789473684, 0., 0.34202807765803783}, 
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

{xzFunc01,iterXZFuncsPF01}=doIterPF[linMod,{X0Z0},X0Z0@@anXtm1EpsZ,rbcComp]
aPath01=genPath[xzFunc01,Drop[iterXZFuncsPF01,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];
cPath01=Private`genPathCompare[linMod,xzFunc01,Drop[iterXZFuncsPF01,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];



{xzFunc02,iterXZFuncsPF02}=doIterPF[linMod,iterXZFuncsPF01,X0Z0@@anXtm1EpsZ,rbcComp]
aPath02=genPath[xzFunc02,Drop[iterXZFuncsPF02,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];
cPath02=Private`genPathCompare[linMod,xzFunc02,Drop[iterXZFuncsPF02,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];



{xzFunc03,iterXZFuncsPF03}=doIterPF[linMod,iterXZFuncsPF02,X0Z0@@anXtm1EpsZ,rbcComp]
aPath03=genPath[xzFunc03,Drop[iterXZFuncsPF03,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];
cPath03=Private`genPathCompare[linMod,xzFunc03,Drop[iterXZFuncsPF03,1],Transpose[{anXtm1EpsZ[[Range[4]]]}],{anXtm1EpsZ[[{5}]]}];


pathErrs01=pathErrsDRPF[xzFunc01,anXtm1EpsZ[[Range[5]]],1,rbcComp,4]

pathErrs02=pathErrsDRPF[xzFunc02,anXtm1EpsZ[[Range[5]]],1,rbcComp,4]

pathErrs03=pathErrsDRPF[xzFunc03,anXtm1EpsZ[[Range[5]]],1,rbcComp,4]

Test[
	iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[5]]],1,5]
	,
{{1}, {0.18}, {1.}, {1.1}, {0.3926985637359888}, {0.20373777768868015}, {2.8158316288775245}, {1.1057730363825737}, {0.4088435447977255}, {0.21166598990562666}, {2.691073445680955}, {1.1002280068432309}, {0.4126099944448989}, {0.2134889198106097}, {2.653803836980466}, {1.0949859864343616}, {0.4119982499217554}, {0.21319352449665901}, {2.6457131861050938}, {1.09002920247021}, {0.40997700668092724}, {0.21221563378219063}, {2.6473217403253826}, {1.0853410428199433}}
,
	TestID->"TestIterDRPF-20151101-A5U5A6"
]



Test[
	iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[5]]],1,5]
	,
{{1}, {0.18}, {1.}, {1.1}, {0.3926355137173878}, {0.2038008277072812}, {2.8162837994794576}, {1.1057730363825737}, {0.40855841389127145}, {0.21202024365816496}, {2.692951532595364}, {1.1002280068432309}, {0.4124371771025212}, {0.2140387675675392}, {2.6549158204576124}, {1.0949859864343616}, {0.41195793180359735}, {0.2138130382725672}, {2.6459721207404394}, {1.09002920247021}, {0.41001263052421083}, {0.2128302907526106}, {2.647091728448241}, {1.0853410428199433}}
			,
	TestID->"TestIterDRPF-2015tt01-A775A6"
]


Test[
	iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[5]]],1,5]
	,
{{1}, {0.18}, {1.}, {1.1}, {0.3925267047550365}, {0.20390963666963258}, {2.817064477365054}, {1.1057730363825737}, {0.4084925204558689}, {0.21220539409318234}, {2.6933859293565527}, {1.1002280068432309}, {0.41241956150728654}, {0.21425327739182115}, {2.6550292193524285}, {1.0949859864343616}, {0.411968480365401}, {0.2140281906841981}, {2.645904369925082}, {1.09002920247021}, {0.4100354805378267}, {0.21303299616388688}, {2.6469442141844555}, {1.0853410428199433}}
,
	TestID->"TestIterDRPF-2015nn01-A5U886"
]







Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[5]]],1,5],oSet=0},
	Chop[Norm[pathErrs01[[1]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-2MM51101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[5]]],1,5],oSet=0},
	Chop[Norm[pathErrs02[[1]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-2M881101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[5]]],1,5],oSet=0},
	Chop[Norm[pathErrs03[[1]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20981101-H4uuR9"
]


Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[5]]],1,5],oSet=4},
	Chop[Norm[pathErrs01[[2]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[5]]],1,5],oSet=4},
	Chop[Norm[pathErrs02[[2]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-oi094101-H4uuR9"
]
Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[5]]],1,5],oSet=4},
	Chop[Norm[pathErrs03[[2]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H488R9"
]


Test[
	With[{pathNow=iterateDRPF[xzFunc01,anXtm1EpsZ[[Range[5]]],1,5],oSet=8},
	Chop[Norm[pathErrs01[[3]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4u099"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,anXtm1EpsZ[[Range[5]]],1,5],oSet=8},
	Chop[Norm[{pathErrs02[[3]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]}]]==0]
	,
	True
	,
	TestID->"TestGenPath-oi094101-uyuuR9"
]
Test[
	With[{pathNow=iterateDRPF[xzFunc03,anXtm1EpsZ[[Range[5]]],1,5],oSet=8},
	Chop[Norm[pathErrs03[[3]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4llR9"
]


Test[
	pathNow=pathErrsDRPF[xzFunc01,anXtm1EpsZ[[Range[5]]],1,rbcComp,3]
	,
{{-0.0012032873783485165, -1.1102230246251565*^-16, 0., 0.}, {-0.005838140921684332, 0., 4.440892098500626*^-16, 0.}}	
	,
	TestID->"TestGenPath-ttuiM59971-H487R9"
]
