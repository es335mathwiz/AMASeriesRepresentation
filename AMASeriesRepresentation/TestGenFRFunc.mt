(* Wolfram Language Test file *)
X0Z0=genX0Z0Funcs[linModBetter];
theLilFunc=genLilXkZkFunc[linModBetter, {X0Z0,2},anXEpsBetter];
theFR=genFRFunc[{4,1,4},theLilFunc,rbcEqnsFunctionalNextBetter];


theLilFuncMoreX0Z0=genLilXkZkFunc[linModBetter, {X0Z0,4},anXEpsBetter];
theFRMoreX0Z0=genFRFunc[{4,1,4},theLilFuncMoreX0Z0,rbcEqnsFunctionalNextBetter];


Test[
Norm[(theFR@@anXEpsFlatBetter)-{{0.3926985637359888}, {0.20373777768868023}, {2.8158316288775245}, 
 {1.1057730363825737}, {-0.005480210873744438}, {-0.000915880995108243}, 
 {-0.003442312382082326}, {0.0007182092189910172}}]<10^(-7)
,(*produces xtzt for a given xtm1,eps and conditional z functions*)
True
	,
	TestID->"TestGenFRFunc-20151101-L8W9H4"
]


Test[
With[{frXtZt=theFR@@anXEpsFlatBetter},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXEpsFlatBetter,frXtZt[[4+Range[4]]]]]]},
	Chop[Norm[rbcEqnsFunctionalNextBetter @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-R6T3M7"
]





Test[
With[{frXtZt=theFRMoreX0Z0@@anXEpsFlatBetter},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXEpsFlatBetter,frXtZt[[4+Range[4]]]]]]},
	Chop[Norm[rbcEqnsFunctionalNextBetter @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-R63TM7"
]