(* Wolfram Language Test file *)
Get["AMASeriesRepresentation/prepBetter.mth"]
firArg=Transpose[{{1, .228,1,1, .07}}];
secArg=Transpose[{{1., .58,1,1.1, .07}}];
Test[
	Chop[Norm[
		iterateDRREIntegrate[simpRBCExactDRBetter, anXEpsBetter, theDistBetter, 5]-
{{0.2}, {0.18}, {1.}, {1.1}, {0.39245511265743227}, {0.20398122876723684}, 
 {2.8175783693963115}, {1.1057730363825737}, {0.4084912681688996}, 
 {0.21231613026407847}, {2.6935288593830613}, {1.1002830196188984}, 
 {0.41246839320517137}, {0.2143832682008641}, {2.654973740565234}, 
 {1.095092752772848}, {0.41205452466236536}, {0.21416815719533275}, 
 {2.645729178355036}, {1.0901846789724177}, {0.4101516309906241}, 
 {0.21317911519573474}, {2.6466855266466753}, {1.0855423854733952}}
		]],
	0,
	TestID->"TestApprox-20151108-P0W0W4"
]
(*

Test[
	Chop[Norm[condExpRE @@ Append[Flatten[anXEpsBetter], 5]-iterateDRREIntegrate[simpRBCExactDRBetter, anXEpsBetter, {{{ee, 
    NormalDistribution[0, 0.01]}}}, 5]]],
	0	,
	TestID->"TestApprox-20151108-F9V5S9"
]*)
Test[
Chop[Norm[evalPathErrDRREIntegrate[simpRBCExactDRBetter, firArg, theDistBetter, rbcEqnsFunctionalNextBetter]]]
	,
	0
	,
	TestID->"TestApprox-20151109-A5G2F6"
]

Test[
Chop[Norm[evalPathErrDRREIntegrate[simpRBCExactDRBetter, secArg, theDistBetter, rbcEqnsFunctionalNextBetter]]]
	,
	0
	,
	TestID->"TestApprox-201xx109-A5G2F6"
]


Test[
Chop[Norm[evalPathErrDRREIntegrate[simpRBCExactDRBetter, secArg, theDistBetter, rbcEqnsFunctionalNextBetter]]]
	,
	0
	,
	TestID->"TestApprox-201xx109-bbG2F6"
]


Test[
	Chop[evalBadPathErrDRREIntegrate[simpRBCExactDRBetter, Drop[firArg,-1], theDistBetter, rbcEqnsFunctionalNextBetter][[1]]]
	,
	0
	,
	TestID->"TestApprox-20151109-A4K9W1"
]




Test[
	Chop[Norm[rbcEqnsFunctionalNextBetter@@ Flatten[worstPathForErrDRREIntegrate[simpRBCExactDRBetter,Drop[firArg,-1], theDistBetter, rbcEqnsFunctionalNextBetter]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-AoptW1"
]


Test[
	Chop[Norm[
  rbcEqnsFunctionalNextBetter @@ 
   Flatten[worstPathForErrDRREIntegrate[
     simpRBCExactDRBetter, Drop[firArg,-1], theDistBetter, 
     rbcEqnsFunctionalNextBetter]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-F0Y7K9"
]

notExactDR = 
 Function[{cc, kk,nl, tt, ee}, 
  1.2*Drop[(simpRBCExactDRBetter @@ {cc, kk,nl, tt, ee})]]
  
theRes=Chop[evalPathErrDRREIntegrate[notExactDR, secArg, theDistBetter, rbcEqnsFunctionalNextBetter]]
Test[
Identity[Chop @Norm[theRes-Transpose[{{-0.2624612021450772, 0, 0.36980265458585415, 0.23483004444308975}}]]]
	,
	0
	,
	TestID->"TestApprox-341xx109-bbG2F6"
]

chk=	evalBadPathErrDRREIntegrate[notExactDR,Drop[firArg,-1],theDistBetter,rbcEqnsFunctionalNextBetter];

chkPath=worstPathForErrDRREIntegrate[notExactDR,Drop[firArg,-1],theDistBetter,rbcEqnsFunctionalNextBetter];
Test[
Chop[Norm @@{chk[[1]]-Norm[Transpose[{(rbcEqnsFunctionalNextBetter@@ Flatten[chkPath])}],Infinity]}]
	,
	0
	,
	TestID->"TestApprox-20151109-A4K551"
]


Test[
	Chop[Norm[
  rbcEqnsFunctionalNextBetter @@ 
   Flatten[worstPathForErrDRREIntegrate[
     simpRBCExactDRBetter, Drop[firArg,-1], theDistBetter, 
     rbcEqnsFunctionalNextBetter]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-F767K9"
]
  
 (* 
quickFunc01 = 
  condApproxExpREFunc[hmatSymbRE, linMod,simpRBCExactDRBetter, 1];
quickFunc02 = 
  condApproxExpREFunc[hmatSymbRE, linMod,simpRBCExactDRBetter, 2];
quickFunc03 = 
  condApproxExpREFunc[hmatSymbRE, linMod,simpRBCExactDRBetter, 3];
quickFunc10 = 
  condApproxExpREFunc[hmatSymbRE, linMod,simpRBCExactDRBetter,10];
Test[
	quickFunc01 @@ anXEpsBetter
	,
	{{0.3921815069815494}, {0.2042548344431197}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-D8M5G2"
]

Test[
	quickFunc02 @@ anXEpsBetter
	,
	{{0.3923530184605553}, {0.20408332296411383}, {1.1057730363825737}}
	,
TestID->"TestApprox-20151108-xxM5G2"
]

Test[
	quickFunc03 @@ anXEpsBetter
	,
	{{0.3924233363175051}, {0.20401300510716405}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-D8Mcc2"
]

Test[
	quickFunc10 @@ anXEpsBetter
	,
	{{0.39245510442410614}, {0.203981237000563}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-D8Myy2"
]
*)
Test[
	simpRBCExactDRBetter @@ anXEpsFlatBetter
	,
{{0.39245511265743227}, {0.20398122876723684}, {2.8175783693963115}, 
 {1.1057730363825737}}
 	,
	TestID->"TestApprox-20151108-I3Q8B7"
]

hip = iterateDRREIntegrate[simpRBCExactDRBetter, 
   anXEpsBetter, {{{ee, NormalDistribution[0, 0.01]}}}, 5];
dip=	(evalBadPathErrDRREIntegrate[simpRBCExactDRBetter, 
 Identity[hip[[ { 5, 6,7,8}]]], theDistBetter, rbcEqnsFunctionalNextBetter]);

Test[
Chop[Identity[rbcEqnsFunctionalNextBetter@@Flatten[worstPathForErrDRREIntegrate[simpRBCExactDRBetter, 
 Identity[hip[[ { 5, 6,7,8}]]], theDistBetter, rbcEqnsFunctionalNextBetter]]]]
	,
	{0,0,0,0}
	,
	TestID->"TestApprox-20151108-M1O4N9"
]


bip = 
 evalBadPathErrDRREIntegrate[notExactDR, 
  Identity[hip[[{ 5, 6,7,8}]]], {{{ee, NormalDistribution[0, 0.01]}}}, 
  rbcEqnsFunctionalNextBetter];

golly = 
 worstPathForErrDRREIntegrate[notExactDR, 
  Identity[hip[[{ 5, 6,7,8}]]], {{{ee, NormalDistribution[0, 0.01]}}}, 
  rbcEqnsFunctionalNextBetter];

Test[
	Chop@Norm[bip[[1]]-Norm[Transpose[{(rbcEqnsFunctionalNextBetter@@ Flatten[golly])}],Infinity]]
	,
0
	,
	TestID->"TestApprox-20151108-U8F3C3"
]


Test[
	Chop[Norm[Flatten[pathErrsDRREIntegrate[simpRBCExactDRBetter, anXEpsBetter, theDistBetter, \
rbcEqnsFunctionalNextBetter, 2]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-P7G8T1"
]