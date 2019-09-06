
Get["tests/pathSetup.mth"]
Needs["AMASeriesRepresentation`"];
Needs["betterRBCTrips`"];
EXPERfSumC[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
           psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
           backLookingInfo:{{_Integer,_,_}...}},initVec_?MatrixQ,cdrFunc_Function,
numIters_Integer]:=
Module[{
thePhi=getPhi[linMod],
theF=getF[linModBetterRBCTrips],
thePsiZ=getPsiZ[linModBetterRBCTrips],
theZsNow},
theZsNow=
AMASeriesRepresentation`Private`genZsForFindRoot[linMod,
        initVec,cdrFunc,numIters];
fSumC[thePhi,theF,thePsiZ,theZsNow]]

{drFunc,cdrFunc}=genBothX0Z0Funcs[linModBetterRBCTrips];


badcdrFunc=Function[{x1,x2,x3,x4},cdrFunc[x1^2,1/x2,x1*x4,x4^3]]


EXPERfSumC[linModBetterRBCTrips,anXBetterRBCTrips,cdrFunc,2]
EXPERfSumC[linModBetterRBCTrips,anXBetterRBCTrips,badcdrFunc,2]

