(* Wolfram Language Test file *)

Get["prepBetter.m"]
Test[
	Chop[Norm[evalPathErrDRREIntegrate[betterExactDR, anXEps, theDist,rbcEqnsCompiled]]]
	,
	0
	,
	TestID->"TestAssessDR-201511236K9A9F1"
]
bp=evalBadPathErrDRREIntegrate[betterExactDR,anX,theDist,rbcEqnsCompiled];
badXEps=Append[anX,bp[[2,-1,-1]]];
theNorm=Norm[evalPathErrDRREIntegrate[betterExactDR,badXEps,theDist,rbcEqnsCompiled],Infinity];
Test[
	Chop[bp[[1]]]
	,
	0
	,
	TestID->"TestAssessDR-20151123-G5545P3"
]

Test[
theNorm==bp[[1]]
 ,
	True
	,
	TestID->"TestAssessDR-20151123-T4553M1"
]
theRes=pathErrsDRREIntegrate[betterExactDR,anXEps,theDist,rbcEqnsCompiled,5];

Test[
	Chop[Norm[theRes[[1]]]]
	,
	0
	,
	TestID->"TestAssessDR-20151ew23-V0G0H0"
]

Test[
	Chop[Norm[#[[{1,2}]]&/@theRes]]
	,
	0
	,
	TestID->"TestAssessDR-20151123-D34U5R0"
]


{betterExactEvalInterp,betterExactEval} = makeDREvalInterp[betterExactDR, theDist,rbcEqnsCompiled,aGSpec];
tps = Private`gridPts[aGSpec[[3]]];

Test[
	Norm[Flatten[(betterExactEvalInterp@@ Private`fillIn[{{}, {1, 3}, #}]) & /@ tps ]]// Chop
	,
	0
	,
	TestID->"TestAssessDR-20151123-W9V5H6"
]