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

 
aGSpec={{1,3}, 1, {{4, 0.018732441104784652, 0.7492976441913861}, {4, 9/10, 11/10}, 
  {4, -0.03, 0.09}}}
thePFDist={{{ee, PerfectForesight}}}
Off[InterpolatingFunction::dmval]
{xzFunc01,iterXZFuncsPF01}=doIterREInterp[{genFRFunc},linMod,{X0Z0,2},rbcComp,aGSpec,thePFDist]

{xzFunc02,iterXZFuncsPF02}=doIterREInterp[{genFRFunc},linMod,{iterXZFuncsPF01,2},rbcComp,aGSpec,thePFDist]

{xzFunc03,iterXZFuncsPF03}=doIterREInterp[{genFRFunc},linMod,{iterXZFuncsPF02,2},rbcComp,aGSpec,thePFDist]

theArg=Transpose[{anXtm1EpsZ[[Range[5]]]}]
pathErrs01=pathErrsDRPF[xzFunc01,theArg,1,rbcComp,4]

pathErrs02=pathErrsDRPF[xzFunc02,theArg,1,rbcComp,4]

pathErrs03=pathErrsDRPF[xzFunc03,theArg,1,rbcComp,4]

Test[
	Norm[iterateDRPF[xzFunc01,theArg,1,5]-	{{1}, {0.18}, {1.}, {1.1}, {0.3824216214170495}, {0.19689170878387824}, 
 {3.104715535873146}, {1.1058839850509055}, {0.4015331155259529}, 
 {0.20772227267444884}, {2.7870121089292548}, {1.1003400466454987}, 
 {0.4090110381488956}, {0.211605145492969}, {2.6827068034499066}, 
 {1.0950922906421303}, {0.4094696707716632}, {0.21173640332845622}, 
 {2.6705742259849097}, {1.0901248936299213}, {0.4077490339851453}, 
 {0.2108922116396362}, {2.6703697353375224}, {1.0854228775565624}}]<10^-6
	,
True
,
	TestID->"TestIterDRPF-20151101-A5U5A6"
]



Test[
	Chop[Norm[iterateDRPF[xzFunc02,theArg,1,5]-{{1}, {0.18}, {1.}, {1.1}, {0.3776858090285792}, {0.20162752117234856}, 
 {3.2399793457743167}, {1.1058839850509055}, {0.40563605112553497}, 
 {0.21255936477268908}, {2.7128512636647244}, {1.1003400466454987}, 
 {0.40989279777014853}, {0.21498454437249753}, {2.6794796907025398}, 
 {1.0950922906421303}, {0.40946403904105627}, {0.2147055582057089}, 
 {2.6716451883774965}, {1.0901248936299213}, {0.4076224954202947}, 
 {0.21361128404322352}, {2.671939794568024}, {1.0854228775565624}}]]
		,
	0	,
	TestID->"TestIterDRPF-2015tt01-A775A6"
]


Test[
	Chop @ Norm[iterateDRPF[xzFunc03,theArg,1,5]-{{1}, {0.18}, {1.}, {1.1}, {0.37732436722815965}, {0.20198896297276814}, 
 {3.257300667080478}, {1.1058839850509055}, {0.4058275339770785}, 
 {0.21268781408767046}, {2.7118472138803478}, {1.1003400466454987}, 
 {0.40997057026790984}, {0.21501992723323185}, {2.679081056808328}, 
 {1.0950922906421303}, {0.4094974520202447}, {0.21470317380009796}, 
 {2.6714763201636007}, {1.0901248936299213}, {0.407639727539998}, 
 {0.21359196996670526}, {2.671848292895173}, {1.0854228775565624}}]
	,
0,
	TestID->"TestIterDRPF-2015nn01-A5U886"
]







Test[
	With[{pathNow=iterateDRPF[xzFunc01,theArg,1,5],oSet=0},
	Chop[Norm[pathErrs01[[1]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-2MM51101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,theArg,1,5],oSet=0},
	Chop[Norm[pathErrs02[[1]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-2M881101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc03,theArg,1,5],oSet=0},
	Chop[Norm[pathErrs03[[1]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20981101-H4uuR9"
]


Test[
	With[{pathNow=iterateDRPF[xzFunc01,theArg,1,5],oSet=4},
	Chop[Norm[pathErrs01[[2]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4uuR9"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,theArg,1,5],oSet=4},
	Chop[Norm[pathErrs02[[2]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-oi094101-H4uuR9"
]
Test[
	With[{pathNow=iterateDRPF[xzFunc03,theArg,1,5],oSet=4},
	Chop[Norm[pathErrs03[[2]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H488R9"
]


Test[
	With[{pathNow=iterateDRPF[xzFunc01,theArg,1,5],oSet=8},
	Chop[Norm[pathErrs01[[3]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4u099"
]

Test[
	With[{pathNow=iterateDRPF[xzFunc02,theArg,1,5],oSet=8},
	Chop[Norm[pathErrs02[[3]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-oi094101-uyuuR9"
]
Test[
	With[{pathNow=iterateDRPF[xzFunc03,theArg,1,5],oSet=8},
	Chop[Norm[pathErrs03[[3]]-rbcComp@@Append[Flatten[pathNow[[oSet+Range[12]]]],0*anXtm1EpsZ[[5]]]]]==0]
	,
	True
	,
	TestID->"TestGenPath-20094101-H4llR9"
]


Test[
	Norm[Flatten[pathNow=pathErrsDRPF[xzFunc01,theArg,1,rbcComp,3]-
{{{-0.08195133526251208}, {-0.017182855166077093}, {0.2129230139098417}, 
 {0.0001109486683317229}}, {{-0.018022567898507535}, {-0.0037281243540544518}, 
 {0.046665163437087376}, {7.167377351624538*^-6}}}]]<10^(-8),
 True
 	,
	TestID->"TestGenPath-ttuiM59971-H487R9"
]
