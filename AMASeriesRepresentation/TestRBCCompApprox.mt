(* Wolfram Language Test file *)
Get["prepBetter.m"]
 anXEps={1,.2,1,1.1,0.01}   
 Print[iterateDRREIntegrate[betterExactDR, anXEps, theDist, 5]]
Test[
	Identity[Identity[iterateDRREIntegrate[betterExactDR, anXEps, theDist, 5]
]]
	,
{{1}, {0.2}, {1}, {1.1}, {0.40762676076186627}, {0.21186679662698824}, {2.7127096226848595}, {1.1057730363825737}, {0.4141073590355788}, {0.21523513189995136}, {2.656999436526274}, {1.1002830196188984}, {0.414500955817105}, {0.2154397065189208}, {2.6419547106088377}, {1.095092752772848}, {0.4127843633983106}, {0.21454749586963867}, {2.64105129854562}, {1.0901846789724177}, {0.41041301148832826}, {0.21331496949697304}, {2.644999927114359}, {1.0855423854733952}}
	,
	TestID->"TestApprox-20151108-P0W0W4"
]
(*

Test[
	Chop[Norm[condExpRE @@ Append[Flatten[anXEps], 5]-iterateDRREIntegrate[betterExactDR, anXEps, {{{ee, 
    NormalDistribution[0, 0.01]}}}, 5]]],
	0	,
	TestID->"TestApprox-20151108-F9V5S9"
]*)
Test[
Chop[Norm[evalPathErrDRREIntegrate[betterExactDR, {1, .228,1,1, .07}, theDist, rbcEqnsCompiled]]]
	,
	0
	,
	TestID->"TestApprox-20151109-A5G2F6"
]

Test[
Chop[Norm[evalPathErrDRREIntegrate[betterExactDR, {1, .58,1,1.02, .07}, theDist, rbcEqnsCompiled]]]
	,
	0
	,
	TestID->"TestApprox-201xx109-A5G2F6"
]


Test[
Chop[Norm[evalPathErrDRREIntegrate[betterExactDR, {1, .58,1,1.1, .07}, theDist, rbcEqnsCompiled]]]
	,
	0
	,
	TestID->"TestApprox-201xx109-bbG2F6"
]


Test[
	Chop[evalBadPathErrDRREIntegrate[betterExactDR, {1, .228,1,1}, theDist, rbcEqnsCompiled][[1]]]
	,
	0
	,
	TestID->"TestApprox-20151109-A4K9W1"
]




Test[
	Chop[Norm[rbcEqnsCompiled@@ Flatten[Private`worstPathForErrDRREIntegrate[betterExactDR, {1., .228, 1,
  1.}, theDist, rbcEqnsCompiled]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-AoptW1"
]


Test[
	Chop[Norm[
  rbcEqnsCompiled @@ 
   Flatten[Private`worstPathForErrDRREIntegrate[
     betterExactDR, {3, .228,1, 1.1}, theDist, 
     rbcEqnsCompiled]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-F0Y7K9"
]

notExactDR = 
 Function[{cc, kk,nl, tt, ee}, 
  1.2*Drop[(betterExactDR @@ {cc, kk,nl, tt, ee})]]
  
theRes=Chop[evalPathErrDRREIntegrate[notExactDR, {1., .58,1,1.1, .07}, theDist, rbcEqnsCompiled]]
Test[
Identity[Identity @Identity[theRes]]
	,
	{{-0.2624612021450772, 0, 0.36980265458585415, 0.23483004444308975}}
	,
	TestID->"TestApprox-341xx109-bbG2F6"
]

chk=	evalBadPathErrDRREIntegrate[notExactDR,{1., .228,1,1.},theDist,rbcEqnsCompiled];

chkPath=Private`worstPathForErrDRREIntegrate[notExactDR,{1., .228,1,1.},theDist,rbcEqnsCompiled];
Test[
Chop[Norm @@{chk[[1]]-Norm[Transpose[{(rbcEqnsCompiled@@ Flatten[chkPath])}],Infinity]}]
	,
	0
	,
	TestID->"TestApprox-20151109-A4K551"
]


Test[
	Chop[Norm[
  rbcEqnsCompiled @@ 
   Flatten[Private`worstPathForErrDRREIntegrate[
     betterExactDR, {1, .228,1, 1}, theDist, 
     rbcEqnsCompiled]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-F767K9"
]
  
 (* 
quickFunc01 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod,betterExactDR, 1];
quickFunc02 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod,betterExactDR, 2];
quickFunc03 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod,betterExactDR, 3];
quickFunc10 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod,betterExactDR,10];
Test[
	quickFunc01 @@ anXEps
	,
	{{0.3921815069815494}, {0.2042548344431197}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-D8M5G2"
]

Test[
	quickFunc02 @@ anXEps
	,
	{{0.3923530184605553}, {0.20408332296411383}, {1.1057730363825737}}
	,
TestID->"TestApprox-20151108-xxM5G2"
]

Test[
	quickFunc03 @@ anXEps
	,
	{{0.3924233363175051}, {0.20401300510716405}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-D8Mcc2"
]

Test[
	quickFunc10 @@ anXEps
	,
	{{0.39245510442410614}, {0.203981237000563}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-D8Myy2"
]
*)
Test[
	betterExactDR @@ anXEps
	,
{{0.40762676076186627}, {0.21186679662698824}, {2.7127096226848595}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-I3Q8B7"
]

hip = iterateDRREIntegrate[betterExactDR, 
   anXEps, {{{ee, NormalDistribution[0, 0.01]}}}, 5];
dip=	(evalBadPathErrDRREIntegrate[betterExactDR, 
 Flatten[hip[[ { 5, 6,7,8}]]], theDist, rbcEqnsCompiled]);

Test[
Chop[Identity[rbcEqnsCompiled@@Flatten[Private`worstPathForErrDRREIntegrate[betterExactDR, 
 Flatten[hip[[ { 5, 6,7,8}]]], theDist, rbcEqnsCompiled]]]]
	,
	{0,0,0,0}
	,
	TestID->"TestApprox-20151108-M1O4N9"
]


bip = 
 evalBadPathErrDRREIntegrate[notExactDR, 
  Flatten[hip[[{ 5, 6,7,8}]]], {{{ee, NormalDistribution[0, 0.01]}}}, 
  rbcEqnsCompiled];

golly = 
 Private`worstPathForErrDRREIntegrate[notExactDR, 
  Flatten[hip[[{ 5, 6,7,8}]]], {{{ee, NormalDistribution[0, 0.01]}}}, 
  rbcEqnsCompiled];

Test[
	Chop@Norm[bip[[1]]-Norm[Transpose[{(rbcEqnsCompiled@@ Flatten[golly])}],Infinity]]
	,
0
	,
	TestID->"TestApprox-20151108-U8F3C3"
]


Test[
	Chop[Norm[pathErrsDRREIntegrate[betterExactDR, anXEps, theDist, \
rbcEqnsCompiled, 2]]]
	,
	0
	,
	TestID->"TestApprox-20151109-P7G8T1"
]