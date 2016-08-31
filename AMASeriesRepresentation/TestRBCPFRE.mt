(* Wolfram Language Test file *)

{x1z1Func, X1Z1Func} = 
  doIterREInterp[{genFRFunc},linModBetter, {genX0Z0Funcs[linModBetter],2},rbcEqnsFunctionalNextBetter,aGSpecBetter,theDistBetter];
Test[
	Norm[(X1Z1Func @@anXEpsFlatBetter)-{{0.3785961351933357}, {0.1950002329961874}, {3.1045685320887886}, 
 {1.0949705528856826}, {-0.026330856144866164}, {-0.017844902107835115}, 
 {0.2065899406443951}, {-0.0000749253529454369}}]<10^(-6)
	,
	True
	,
	TestID->"TestRBCPFRE-20151114-U5V8A1"
]

bunch=
  nestIterREInterp[{genFRFunc},linModBetter, {genX0Z0Funcs[linModBetter],2},rbcEqnsFunctionalNextBetter,aGSpecBetter,theDistBetter,1]
Test[
	Norm[(bunch[[-1,2]] @@anXEpsFlatBetter)-{{0.3785961351933357}, {0.1950002329961874}, {3.1045685320887886}, 
 {1.0949705528856826}, {-0.026330856144866164}, {-0.017844902107835115}, 
 {0.2065899406443951}, {-0.0000749253529454369}}]<10^(-6)
	,
	True
	,
	TestID->"TestRBCPFRE-20151114-O2Q3I3"
]
