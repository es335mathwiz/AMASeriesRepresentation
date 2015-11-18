(* Wolfram Language Test file *)
Get["prepBetter.m"]
Print["inTestCondExpComposition.mt",Directory[],{linMod,X0Z0}//InputForm];
what=fSum[linMod,{X0Z0}];
Save["whyPblm.mth",{linMod,X0Z0,what}]
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