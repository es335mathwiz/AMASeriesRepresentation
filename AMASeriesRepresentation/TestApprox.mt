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
theDist={{{ee, 
    NormalDistribution[0, 0.01]}}};
Test[
	iterateDRREIntegrate[condExpREFunc, anXEps, theDist, 5]
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
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod,condExpREFunc, 1];
quickFunc02 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod,condExpREFunc, 2];
quickFunc03 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod,condExpREFunc, 3];
quickFunc10 = 
  Private`condApproxExpREFunc[Private`hmatSymbRE, linMod,condExpREFunc,10];
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

Test[
	condExpREFunc @@ anXEps
	,
	{{0.3924551126574322}, {0.20398122876723684}, {1.1057730363825737}}
	,
	TestID->"TestApprox-20151108-I3Q8B7"
]


hip = iterateDRREIntegrate[condExpREFunc, 
   anXEps, {{{ee, NormalDistribution[0, 0.01]}}}, 5];
dip=	(evalBadPathErrDRREIntegrate[condExpREFunc, 
 Flatten[hip[[-3 + {4, 5, 6}]]], theDist, Private`rbcEqnsFunctionalNext]);
Test[
{dip[[1]],dip[[2,1,2]]}
	,
	{4.440892098500626*^-16, -3.52019560245794*^-8}
	,
	TestID->"TestApprox-20151108-M1O4N9"
]

condExpRENotFunc = 
 Function[{cc, kk, tt, ee}, 
  1.02*Drop[(condExpRE @@ Append[{cc, kk, tt, ee}, 1]), 3]];

bip = 
 evalBadPathErrDRREIntegrate[condExpRENotFunc, 
  Flatten[hip[[{4, 5, 6}]]], {{{ee, NormalDistribution[0, 0.01]}}}, 
  Private`rbcEqnsFunctionalNext];

golly = 
 Private`worstPathForErrDRREIntegrate[condExpRENotFunc, 
  Flatten[anX], {{{ee, NormalDistribution[0, 0.01]}}}, 
  Private`rbcEqnsFunctionalNext];

Test[
	{bip[[1]],bip[[2,1,2]]}
	,
	{0.022674336313503685,  0.029984015219674816}
	,
	TestID->"TestApprox-20151108-U8F3C3"
]


Test[
	golly
	,
	{{1}, {0.18}, {1.1}, {0.4083843613941644}, {0.21226056473678456}, 
 {1.150654943063924}, {0.43895357025769877}, {0.2281491201035456}, 
 {1.16552002226664}, {0.029983997860106897}}
	,
	TestID->"TestApprox-20151108-Q6R9Q6"
]

Test[
	Norm[pathErrsDRREIntegrate[condExpREFunc, anXEps, theDist, \
Private`rbcEqnsFunctionalNext, 2]]
	,
	0
	,
	TestID->"TestApprox-20151109-P7G8T1"
]