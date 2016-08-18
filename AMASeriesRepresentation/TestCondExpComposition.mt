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

Test[
	ffs=fSum[linMod,{X0Z0,1},Table[{0},{4}],1]
	,
	{{0.}, {0.}, {0.}, {0.}}
	,
	TestID->"TestCondExpComposition-20151117-R6S5H9"
]

Test[
	ffs=fSum[linMod,{X0Z0,2},Table[{0},{4}],2]
	,
	{{0.}, {0.}, {0.}, {0.}}
	,
	TestID->"TestCondExpComposition-22251117-R6S5H9"
]

Test[
	ffs=fSum[linMod,{X0Z0,3},Table[{0},{4}],3]
	,
	{{0.}, {0.}, {0.}, {0.}}
	,
	TestID->"TestCondExpComposition-36251117-R6S5H9"
]



Test[
	ffs=fSum[linMod,{X0Z0,4},Table[{0},{4}],4]
	,
	{{0.}, {0.}, {0.}, {0.}}
	,
	TestID->"TestCondExpComposition-22251117-R640H9"
]