(* Wolfram Language Test file *)

Test[
	{x1z1Funcpar, X1Z1Funcpar} = 
  parallelDoIterREInterp[{genFRFunc},linModBetter, {genX0Z0Funcs[linModBetter],2},rbcEqnsFunctionalNextBetter,aGSpecBetter,theDistBetter];
{x1z1Func, X1Z1Func} = 
  parallelDoIterREInterp[{genFRFunc},linModBetter, {genX0Z0Funcs[linModBetter],2},rbcEqnsFunctionalNextBetter,aGSpecBetter,theDistBetter];
	Norm[(x1z1Funcpar @@ anXEpsFlatBetter)-
		(x1z1Func @@ anXEpsFlatBetter)]<10^-8
		,
		True
	,
	TestID->"TestParallelSeriel-20160904-I5E5Y5"
]


Test[
Norm[(X1Z1Funcpar @@ anXEpsFlatBetter)-
		(X1Z1Func @@ anXEpsFlatBetter)]<10^-8
		,
		True
	,
	TestID->"TestParallelSeriel-klkklj20160904-I5E5Y5"
]