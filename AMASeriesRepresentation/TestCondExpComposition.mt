(* Wolfram Language Test file *)
Print["before switch",$System]
Switch[$System,
 "Mac OS X x86 (64-bit)", Print["mac"];
 SetDirectory[
  "/Users/garyanderson/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"];Print[Directory[]],
 "Linux x86 (64-bit)", Print["linux"];
 SetDirectory[
  "~/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"];Print[Directory[]],
 "Microsoft Windows (64-bit)", Print["windows"];
 SetDirectory[
  "g:/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"];Print[Directory[]],
  _,Print["unknown:",$System]]; 
$ContextPath=DeleteCases[$ContextPath,"simpleRBCModel`"] 
Print["after switch",$System];
Get["prepBetter.m"];Get["betterRBC.m"];
Print["inTestCondExpComposition.mt",$Path,Directory[],{linMod,X0Z0}//InputForm];
what=fSum[linMod,{X0Z0}];
hat=fSum[linMod,{X0Z0},Table[{0},{4}]]
Save["whyPblm.mth",{linMod,X0Z0,what,hat}]
Test[
	ffs=fSum[linMod,{X0Z0},Table[{0},{4}]]
	,
	{{0.}, {0.}, {0.}, {0.}}
	,
	TestID->"TestCondExpComposition-20151117-R6S5H9"
]

Test[
	ffs=fSum[linMod,{X0Z0,X0Z0},Table[{0},{4}]]
	,
	{{0.}, {0.}, {0.}, {0.}}
	,
	TestID->"TestCondExpComposition-22251117-R6S5H9"
]

Test[
	ffs=fSum[linMod,{X0Z0,X0Z0,X0Z0},Table[{0},{4}]]
	,
	{{0.}, {0.}, {0.}, {0.}}
	,
	TestID->"TestCondExpComposition-36251117-R6S5H9"
]



Test[
	ffs=fSum[linMod,{X0Z0,X0Z0,X0Z0,X0Z0},Table[{0},{4}]]
	,
	{{0.}, {0.}, {0.}, {0.}}
	,
	TestID->"TestCondExpComposition-22251117-R640H9"
]