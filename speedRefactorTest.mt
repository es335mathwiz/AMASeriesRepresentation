(* Wolfram Language Test file *)

Test[
drFuncs=
Table[genSeriesRepFunc[linModBetter,theDistBetter,simpRBCExactDRBetter,ii],
		   {ii,20}];
	res=drFuncs[[-1]] @@anXEpsBetter;
		Norm[res-{{0.2}, {0.18}, {1.}, {1.1}, {0.392455112657589}, {0.20398122876708016}, 
 {2.817578369395103}, {1.1057730363825737}, {0.4084912681692493}, 
 {0.212316130263564}, {2.693528859380366}, {1.1002830196188984}, {0.01}}]<10^(-10)
	,
True
	,
	TestID->"speedRefactorTest-20160803-O2U4E8"
]