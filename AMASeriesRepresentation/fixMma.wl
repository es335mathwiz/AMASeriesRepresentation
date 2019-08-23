Get["finishPaper.mth"]

(*  fails 
approxAndBound[*)
approx=1*{1,1,1}(*,*)
theK=2
(*]*)



{tryEps,numKern,theName,mthName,bothX0Z0,sgSpecErg,zPts}=
prepSmol[approx,theK];
{tm,ig}=Timing[
theRes=parallelNestGenericIterREInterp[genFRExtFunc,linModBetterRBCTrips,
{bothX0Z0,theK},rbcEqnsBetterRBCTrips,sgSpecErg,smolyakInterpolation,{},2,Apply[Sequence,FilterRules[{},Options[parallelNestGenericIterREInterp]]]]];

