Get["JavaGraphics`"]
Get["tests/pathSetup.mth"]

Get["AMASeriesRepresentation`"];



Get["betterRBCTrips`"];
(*
LaunchKernels[]

ParallelEvaluate[$HistoryLength=1];
ParallelEvaluate[Get["tests/pathSetup.mth"]]
ParallelNeeds["AMASeriesRepresentation`"]
(*ParallelNeeds["emscsToMma`"]*)
ParallelNeeds["betterRBCTrips`"]

*)

dirNameString[]:=
Module[{},
aDir="resDir"<>"-host-"<>$MachineName<>"numKern"<>ToString[numKern]<>"-"<>ToString[Round[AbsoluteTime[]]]<>"/"(*;
CreateDirectory[aDir]*)]

fNameString[approx_?VectorQ,theK_Integer,numKern_Integer]:=
Module[{},
StringReplace[dirNameString[]<>"forBetterTripsRBC-"<>ToString[approx]<>"theK"<>ToString[theK],{" "->"","{"->"-","}"->"-"}]];




Options[approxAndBound]={"Traditional"->False,"addTailContribution"->False,"maxForCEIters"->40,
"normConvTol"->10^(-10),"maxNormsToKeep"->50};
approxAndBound[approx_?VectorQ,theK_Integer,numIters_Integer,
opts:OptionsPattern[]]:=
Module[{},
{tryEps,numKern,theName,mthName,bothX0Z0,sgSpecErg,zPts}=
prepSmol[approx,theK];
{tm,ig}=Timing[
theRes=parallelNestGenericIterREInterp[genFRExtFunc,linModBetterRBCTrips,
{bothX0Z0,theK},rbcEqnsBetterRBCTrips,sgSpecErg,smolyakInterpolation,{},
numIters,Apply[Sequence,FilterRules[{opts},Options[parallelNestGenericIterREInterp]]]]];
{sgSpecErg,theRes,tm,Length[Kernels[]]}]


approx=1*{1,1,1};

prepSmol[approx_?VectorQ,theK_Integer]:=
Module[{toIg,zPts,ptErg,tfErg,plyErg,iplyErg,dplyErg,bothX0Z0,smolStuff,smolRngErg,sgSpecErg},
smolStuff = {ptErg, tfErg, plyErg, iplyErg, dplyErg} = 
   smolyakInterpolationPrep[approx, {betterRBCTripsMean, 
     betterRBCTripsSD, 
     betterRBCTripsMinZ, 
     betterRBCTripsMaxZ, 
     betterRBCTripsvv}, 
    theDistBetterRBCTrips];
smolRngErg = 
  Transpose[{betterRBCTripsMinZ, 
    betterRBCTripsMaxZ}];
toIg = aGSpecBetterRBCTrips[[1]];
sgSpecErg = {toIg, smolRngErg, ptErg, tfErg, plyErg, iplyErg, 
   1, approx, {betterRBCTripsMean, 
    betterRBCTripsSD, 
    betterRBCTripsMinZ, 
    betterRBCTripsMaxZ, 
    betterRBCTripsvv}};
    numKern=Length[Kernels[]];
theName=fNameString[approx,theK,numKern];
mthName=theName<>".mth";
X0Z0 = genBothX0Z0Funcs[linModBetterRBCTrips];
 {tryEps,numKern,theName,mthName,X0Z0,sgSpecErg,zPts}]


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

X0Z0 = genBothX0Z0Funcs[linModBetterRBCTrips];







theK=2;

{dr,cedr}=parallelDoGenericIterREInterp[
   genFRExtFunc, 
   linModBetterRBCTrips,{X0Z0, theK}, 
   eqnsCompiledBetterRBCTrips, sgSpecErg, 
   smolyakInterpolation, {},(*
"xVarRanges"->theRanges,*)"Traditional"->False]
