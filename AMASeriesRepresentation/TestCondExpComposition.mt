(* Wolfram Language Test file *)
Get["prepBetter.m"]
Print[{linMod,X0Z0}//InputForm];
Save["whyPblm.mth",{linMod,X0Z0,fSum[linMod,{X0Z0}]}]
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