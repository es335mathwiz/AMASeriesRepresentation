(* Wolfram Language Test file *)
X0Z0=genX0Z0Funcs[linModBetter];
theLilFunc=genLilXkZkFunc[linModBetter, {X0Z0,2},anXEpsBetter];
theFR=genFRFunc[{4,1,4},theLilFunc,rbcEqnsFunctionalNextBetter];
theFP=genFPFunc[{genFRFunc},linModBetter,{X0Z0,2},rbcEqnsFunctionalNextBetter];



theLilFuncMoreX0Z0=genLilXkZkFunc[linModBetter, {X0Z0,4},anXEpsBetter];
theFRMoreX0Z0=genFRFunc[{4,1,4},theLilFuncMoreX0Z0,rbcEqnsFunctionalNextBetter];
theFPMoreX0Z0=genFPFunc[{genFRFunc},linModBetter,{X0Z0,4},rbcEqnsFunctionalNextBetter];



(*fixed point irrelevant until more than one XZ*)
Test[
With[{frXtZt=theFR@@anXEpsFlatBetter,
	fpXtZt=theFP@@anXEpsFlatBetter},Chop[Norm[frXtZt-fpXtZt]==0]],
	True
	,
	TestID->"TestGenFRFunc-201101-R6T3M7"
]

(*satisfies system equations*)
Test[
With[{fpXtZt=theFP@@anXEpsFlatBetter},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXEpsFlatBetter,fpXtZt[[4+Range[4]]]]]]},
	Chop[Norm[rbcEqnsFunctionalNextBetter @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-151101-R6T8M7"
]




(*fixed point irrelevant until more than one XZ and Z's non zero*)
Test[
With[{frXtZt=theFRMoreX0Z0@@anXEpsFlatBetter,
	fpXtZt=theFPMoreX0Z0@@anXEpsFlatBetter},Chop[Norm[frXtZt-fpXtZt]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-MMT3M7"
]

(*satisfies system equations*)
Test[
With[{fpXtZt=theFPMoreX0Z0@@anXEpsFlatBetter},
	With[{aPath=Flatten[theLilFunc@@ Flatten[Join[anXEpsFlatBetter,fpXtZt[[4+Range[4]]]]]]},
	Chop[Norm[rbcEqnsFunctionalNextBetter @@ aPath]]==0]],
	True
	,
	TestID->"TestGenFRFunc-20151101-NNT8M7"
]
