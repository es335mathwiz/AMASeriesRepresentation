(* Wolfram Language Test file *)

 Get["AMASeriesRepresentation/prepSimple.mth"]   
 anInit01=Transpose[{{1, .228,1, .07}}];
 anInit02=Transpose[{{1, .58,1}}];
Test[
	Chop[Norm[iterateDRREIntegrate[simpRBCExactDR, anXEps, theDist, 5]-{{0.2}, {0.18}, {1.1}, {0.39245511265743227}, {0.20398122876723684}, {1.1057730363825737}, {0.4084912681688996}, {0.21231613026407847}, {1.1002830196188984}, {0.41246839320517137}, {0.2143832682008641}, {1.095092752772848}, {0.41205452466236536}, {0.21416815719533275}, {1.0901846789724177}, {0.4101516309906241}, {0.21317911519573474}, {1.0855423854733952}}]]
	,
0
	,
	TestID->"TestApprox-20151108-P0W0W4"
]

(*
Test[
	Chop[Norm[simpRBCExactDR @@ Append[Flatten[anXEps], 5]-iterateDRREIntegrate[simpRBCExactDR, anXEps, {{{ee, 
    NormalDistribution[0, 0.01]}}}, 5]]],
	0	,
	TestID->"TestApprox-20151108-F9V5S9"
]*)
Test[
Chop[Norm[evalPathErrDRREIntegrate[simpRBCExactDR, anInit01, theDist, rbcEqnsFunctionalNext]]]
	,
	0
	,
	TestID->"TestApprox-20151109-A5G2F6"
]

Test[
Chop[Norm[evalPathErrDRREIntegrate[simpRBCExactDR, anInit01, theDist, rbcEqnsFunctionalNext]]]
	,
	0
	,
	TestID->"TestApprox-201xx109-A5G2F6"
]


Test[
Chop[Norm[evalPathErrDRREIntegrate[simpRBCExactDR, anInit01, theDist, rbcEqnsFunctionalNext]]]
	,
	0
	,
	TestID->"TestApprox-201xx109-bbG2F6"
]


Test[
	Chop[evalBadPathErrDRREIntegrate[simpRBCExactDR, anInit02, theDist, rbcEqnsFunctionalNext][[1]]]
	,
	0
	,
	TestID->"TestApprox-20151109-A4K9W1"
]




Test[
	Chop[Norm[rbcEqnsFunctionalNext@@ Flatten[worstPathForErrDRREIntegrate[simpRBCExactDR, anInit02, theDist, rbcEqnsFunctionalNext]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-AoptW1"
]


Test[
	Chop[Norm[
  rbcEqnsFunctionalNext @@ 
   Flatten[worstPathForErrDRREIntegrate[
     simpRBCExactDR, anInit02, theDist, 
     rbcEqnsFunctionalNext]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-F0Y7K9"
]

simpRBCExactDRNot = 
 Function[{cc, kk, tt, ee}, 
  1.2*Drop[(simpRBCExactDR @@ {cc, kk, tt, ee}), 0]]
  
theRes=Chop[evalPathErrDRREIntegrate[simpRBCExactDRNot, anInit01, theDist, rbcEqnsFunctionalNext]]
Test[
Norm[theRes-{{0}, {0}, {0.21450163625084318}}]
	,
	0.
	,
	TestID->"TestApprox-341xx109-bbG2F6"
]

chk=	evalBadPathErrDRREIntegrate[simpRBCExactDRNot,anInit02,theDist,rbcEqnsFunctionalNext];

chkPath=worstPathForErrDRREIntegrate[simpRBCExactDRNot,anInit02,theDist,rbcEqnsFunctionalNext];
Test[
Chop[chk[[1]]-(rbcEqnsFunctionalNext@@ Flatten[chkPath])[[-1]]]
	,
	0
	,
	TestID->"TestApprox-20151109-A4K551"
]



theRes=rbcEqnsFunctionalNext@@Flatten[worstPathForErrDRREIntegrate[simpRBCExactDRNot, anInit02, theDist, rbcEqnsFunctionalNext]];
Test[
	Chop[theRes[[-1]]-evalBadPathErrDRREIntegrate[simpRBCExactDRNot, anInit02, theDist, rbcEqnsFunctionalNext][[1]]]
	,
0
	,
	TestID->"TestApprox-20151109-A23tW1"
]


Test[
	Chop[Norm[
  rbcEqnsFunctionalNext @@ 
   Flatten[worstPathForErrDRREIntegrate[
     simpRBCExactDR, anInit02, theDist, 
     rbcEqnsFunctionalNext]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-F767K9"
]
  
 (* 
quickFunc01 = 
  condApproxExpREFunc[hmatSymbRE, linMod,simpRBCExactDR, 1];
quickFunc02 = 
  condApproxExpREFunc[hmatSymbRE, linMod,simpRBCExactDR, 2];
quickFunc03 = 
  condApproxExpREFunc[hmatSymbRE, linMod,simpRBCExactDR, 3];
quickFunc10 = 
  condApproxExpREFunc[hmatSymbRE, linMod,simpRBCExactDR,10];
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
	simpRBCExactDR @@ anXEpsFlat
	,
	{{0.3924551126574322}, {0.20398122876723684}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-I3Q8B7"
]

hip = iterateDRREIntegrate[simpRBCExactDR, 
   anXEps, {{{ee, NormalDistribution[0, 0.01]}}}, 5];
dip=	(evalBadPathErrDRREIntegrate[simpRBCExactDR, 
 Identity[hip[[ {4, 5, 6}]]], theDist, rbcEqnsFunctionalNext]);

Test[
Chop[Identity[rbcEqnsFunctionalNext@@Flatten[worstPathForErrDRREIntegrate[simpRBCExactDR, 
 Identity[hip[[ {4, 5, 6}]]], theDist, rbcEqnsFunctionalNext]]]]
	,
	{0,0,0}
	,
	TestID->"TestApprox-20151108-M1O4N9"
]

simpRBCExactDRNot = 
 Function[{cc, kk, tt, ee}, 
  1.02*(simpRBCExactDR @@ {cc, kk, tt, ee})];

hip = iterateDRREIntegrate[simpRBCExactDR, 
   anXEps, {{{ee, NormalDistribution[0, 0.01]}}}, 5];
bip = 
 evalBadPathErrDRREIntegrate[simpRBCExactDRNot, 
  Identity[hip[[{4, 5, 6}]]], {{{ee, NormalDistribution[0, 0.01]}}}, 
  rbcEqnsFunctionalNext];

golly = 
 worstPathForErrDRREIntegrate[simpRBCExactDRNot, 
  Identity[hip[[{4, 5, 6}]]], {{{ee, NormalDistribution[0, 0.01]}}}, 
  rbcEqnsFunctionalNext];
Print["golly",golly];
Print["hip",hip];
Print["bip",bip];
Test[
	Chop[bip[[1]]-(rbcEqnsFunctionalNext@@ Flatten[golly])[[-1]]]
		,
0
	,
	TestID->"TestApprox-20151108-U8F3C3"
]


Test[
	Chop[Norm[pathErrsDRREIntegrate[simpRBCExactDR, anXEps, theDist, \
rbcEqnsFunctionalNext, 2][[1]]]]
	,
	0
	,
	TestID->"TestApprox-20151109-P7G8T1"
]