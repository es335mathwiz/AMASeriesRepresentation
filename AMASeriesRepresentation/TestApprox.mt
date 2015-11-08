(* Wolfram Language Test file *)
Switch[$System,
 "Mac OS X x86 (64-bit)", 
 SetDirectory[
  "/Users/garyanderson/git/ProjectionMethodTools/\
ProjectionMethodToolsJava/code"],
 "Linux x86 (64-bit)", 
 SetDirectory[
  "~/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"],
 "Microsoft Windows (64-bit)", 
 SetDirectory[
  "g:/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"]]; \
Needs["simpleRBCModel`"]
Get["exactCalcsRBC.mth"]
Test[
	iterateDRREIntegrate[condExpREFunc, anXEps, {{{ee, 
    NormalDistribution[0, 0.01]}}}, 5]
	,
	{{1}, {0.18}, {1.1}, {0.3924551126574322}, {0.20398122876723684}, 
 {1.1057730363825737}, {0.4084912681688997}, {0.2123161302640786}, 
 {1.1002830196188993}, {0.4124683932051717}, {0.2143832682008644}, 
 {1.0950927527728487}, {0.4120545246623659}, {0.21416815719533305}, 
 {1.0901846789724172}, {0.4101516309906242}, {0.2131791151957348}, 
 {1.0855423854733948}}
	,
	TestID->"TestApprox-20151108-P0W0W4"
]


Test[
	Chop[Norm[condExpRE @@ Append[Flatten[anXEps], 5]-iterateDRREIntegrate[condExpREFunc, anXEps, {{{ee, 
    NormalDistribution[0, 0.01]}}}, 5]]],
	0	,
	TestID->"TestApprox-20151108-F9V5S9"
]

quickFunc01 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod, 1];
quickFunc02 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod, 2];
quickFunc03 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod, 3];
quickFunc10 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod, 10];
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