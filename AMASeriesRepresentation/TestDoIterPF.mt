(* Wolfram Language Test file *)

{xzFunc,iterXZFuncsPF}=
parallelDoIterREInterp[{genFRFunc},linModBetter,{anX0Z0=genX0Z0Funcs[linModBetter],2},rbcEqnsFunctionalNextBetter,aGSpecBetter,thePFDistBetter];
anXZFuncPF=genXZFuncRE[{4,1,4},xzFunc,thePFDistBetter];


Test[
	Chop[Norm[iterXZFuncsPF@@anXEpsFlatBetter-xzFunc @@Append[anXEpsFlatBetter[[Range[4]]],0]]]==0
	,
	True
	,
	TestID->"TestparallelDoIterPF-20151101-C2H9V8"
]


Test[
	Norm[(anXZFuncPF @@anXEpsFlatBetter)-{{0.3785806682419918}, {0.19499631148396843}, {3.1045844175564032}, 
 {1.094933541150874}, {-0.026270130006907123}, {-0.017844036339470744}, 
 {0.20658936544802525}, {-0.00011178149877949184}}]<10^(-7)
	,
	True
	,
	TestID->"TestparallelDoIterPF-20151101-A7A1F3"
]



Test[
	Norm[(iterXZFuncsPF @@anXEpsFlatBetter)-	{{0.3785806682419918}, {0.19499631148396843}, {3.1045844175564032}, 
 {1.094933541150874}, {-0.026270130006907123}, {-0.017844036339470744}, 
 {0.20658936544802525}, {-0.00011178149877949184}}]<10^(-7)
	,
True
	,
	TestID->"TestparallelDoIterPF-20151101-C3L9F3"
]
