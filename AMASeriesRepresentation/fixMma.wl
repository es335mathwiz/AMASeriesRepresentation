Get["finishPaper.mth"]

(*  fails 
approxAndBound[*)
approx=1*{1,1,1}(*,*)
theK=2
(*]*)


(*succeeds*)
{tryEps,numKern,theName,mthName,bothX0Z0,sgSpecErg,zPts}=
prepSmol[approx,theK];




(*no errors or warnings*)
linMod=linModBetterRBCTrips;
numX=Length[getB[linMod]];
numZ=Length[getPsiZ[linMod][[1]]];
numEps=sgSpecErg[[7]];
bothXZFuncs={bothX0Z0,theK};
triples=eqnsCompiledBetterRBCTrips;
reapRes=Reap[boo=genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,
triples,{}]]

Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];


(*fails*)
bothX0Z0 = genBothX0Z0Funcs[
   linModBetterRBCTrips];

approx=1*{1,1,1};
smolStuff = {ptErg, tfErg, plyErg, iplyErg, dplyErg} = 
   smolyakInterpolationPrep[approx, {betterRBCTripsMean, 
     betterRBCTripsSD, 
     betterRBCTripsMinZ, 
     betterRBCTripsMaxZ, 
     betterRBCTripsvv}, 
    theDistBetterRBCTrips]
smolRngErg = 
  Transpose[{betterRBCTripsMinZ, 
    betterRBCTripsMaxZ}];
toIg = aGSpecBetterRBCTrips[[1]];
sgSpecErg = {toIg, smolRngErg, ptErg, tfErg, plyErg, iplyErg, 
   1, approx, {betterRBCTripsMean, 
    betterRBCTripsSD, 
    betterRBCTripsMinZ, 
    betterRBCTripsMaxZ, 
    betterRBCTripsvv}}

theK=3

{tm,ig}=Timing[
theRes=parallelNestGenericIterREInterp[genFRExtFunc,linModBetterRBCTrips,
{bothX0Z0,theK},eqnsCompiledBetterRBCTrips,sgSpecErg,smolyakInterpolation,{},2 ,Apply[Sequence,FilterRules[{},Options[parallelNestGenericIterREInterp]]]]];

(*
The following fixes problem
approx=1*{1,1,1};
smolStuff = {ptErg, tfErg, plyErg, iplyErg, dplyErg} = 
   smolyakInterpolationPrep[approx, {betterRBCTripsMean, 
     betterRBCTripsSD, 
     betterRBCTripsMinZ, 
     betterRBCTripsMaxZ, 
     betterRBCTripsvv}, 
    theDistBetterRBCTrips]
smolRngErg = 
  Transpose[{betterRBCTripsMinZ, 
    betterRBCTripsMaxZ}];
toIg = aGSpecBetterRBCTrips[[1]];
sgSpecErg = {toIg, smolRngErg, ptErg, tfErg, plyErg, iplyErg, 
   1, approx, {betterRBCTripsMean, 
    betterRBCTripsSD, 
    betterRBCTripsMinZ, 
    betterRBCTripsMaxZ, 
    betterRBCTripsvv}}

bothX0Z0 = genBothX0Z0Funcs[
   linModBetterRBCTrips];

*)




wrong number args to compiled function
theDo=parallelDoGenericIterREInterp[genFRExtFunc,linModBetterRBCTrips,
{bothX0Z0,theK},eqnsCompiledBetterRBCTrips,sgSpecErg,smolyakInterpolation,{} ,Apply[Sequence,FilterRules[{},Options[parallelNestGenericIterREInterp]]]]

wrong number args to compiled function
smolGSpec=sgSpecErg;
theFuncs=
parallelMakeGenericInterpFuncs[reapRes[[1]],linMod[[-1]],smolGSpec,
smolyakInterpolation,{}]


wrong number args to compiled function
interpData=AMASeriesRepresentation`Private`parallelSmolyakGenInterpData[triples,smolGSpec]


(*the following works*)
smolPts=smolGSpec[[3]];
smolToIgnore=smolGSpec[[1]];
filledPts=Map[Function[xx,fillIn[{{},smolToIgnore,xx}]],N[smolPts]]



triples=eqnsCompiledBetterRBCTrips;
theVals=
ParallelTable[evaluateTriple[aTriple,Flatten[aPt]],
{aPt,Join[#[[Range[4]]],#[[Range[4]]],#[[Range[4]]],{0}]&/@filledPts},{aTriple,triples[[1]]}]