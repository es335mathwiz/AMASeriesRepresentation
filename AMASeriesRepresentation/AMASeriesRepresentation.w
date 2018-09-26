\documentclass[12pt]{article}
\usepackage[english]{babel}
\usepackage{hyperref}
\usepackage{datetime}
\title{Mathematica Code for AMASeriesRepresentation Package}
\author{Gary S Anderson}

\begin{document}
\maketitle


\section{Introduction and Summary}
\label{sec:introduction-summary}

\section{Triples}
\label{sec:triples}

\subsection{genBothX0Z0Funcs}
\label{sec:genx0z0funcs}


@d genBothX0Z0FuncsUsage
@{genBothX0Z0Funcs::usage=
"place holder for genBothX0Z0Funcs"
@}

@d genBothX0Z0Funcs
@{
(*begin code for genBothX0Z0Funcs*)
genBothX0Z0Funcs[@<linMod@>]:=
With[{numXVars=getNumX[linMod],numEpsVars=getNumEps[linMod],
numZVars=getNumZ[linMod]},
With[{xtm1Vars=genSlots[numXVars],
epsVars=Drop[genSlots[numXVars+numEpsVars],numXVars]},
With[{fromLinMod=Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,
ConstantArray[0,{numZVars,1}]]+
Join[psiEps.epsVars,ConstantArray[0,{numZVars,1}]],
fromLinModCE=Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,
ConstantArray[0,{numZVars,1}]]},
{
Apply[Function,{fromLinMod}],
Apply[Function,{fromLinModCE}]
}
]]]

(*end code for genBothX0Z0Funcs*)
@}



\subsection{genSlots}
\label{sec:genxtm1vars}


@d genSlots
@{
(*begin code for genSlots*)
genSlots[numVars_Integer]:=
Module[{},
replaceMySlotStandIn[Table[{mySlotStandIn[ii]},{ii,numVars}]]]/;And[numVars>=0]

replaceMySlotStandIn[xx_]:=xx/.mySlotStandIn->Slot
(*end code for genSlots*)
@}


\subsection{genFRExtFunc}
\label{sec:genfrfunc}


@d genFRExtFuncUsage
@{genFRExtFunc::usage=
"genFRExtFunc"
@}

@d genFRExtFunc
@{

(*begin code for genFRExtFunc*)
Options[genFRExtFunc]={"xVarRanges"->{},"Traditional"->False,"addTailContribution"->False} 

@}

@d genFRExtFunc
@{
genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},@<linMod@>,
@<bothXZFuncs@>,
@<rawTriples@>,
opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{funcTrips=
Map[{#[[1]],genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,#[[2]],
Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],#[[3]]}&,triples[[1]]]},
{funcTrips,selectorFunc[triples]}
]]

selectorFunc[@<rawTriples@>]:=triples[[-1]]

@}

@d genFRExtFunc
@{
genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},
@<linMod@>,@<bothXZFuncs@>,
@<eqnsFunc@>,opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{@<findRootArgNames@>},
With[{@<prepFindRootXInitBoth@>},
With[{zArgsInit=Transpose[{zArgs,Drop[theXInit,numX]}]},
With[{@<cmptXArgsInit@>,
@<makeArgPatternsBoth@>},
(**)
Switch[OptionValue["Traditional"],
True,WriteString["stdout","t"];@<setDelayedTradFXtZtBoth@>;@<setDelayedTradFXtm1Eps@>,
False,WriteString["stdout","n"]@<setDelayedSeriesFXtZtBoth@>;@<setDelayedSeriesFXtm1Eps@>]
(**)
(**)
DistributeDefinitions[funcOfXtZt,funcOfXtm1Eps]
Off[FindRoot::srect];
Off[FindRoot::nlnum];Sow[{funcOfXtm1Eps,funcOfXtZt},"theFuncs"];
funcOfXtm1Eps
]]]]]
@}

@d genFRExtFunc
@{


makePatternArgs[theNames_List]:=
Map[PatternTest[Pattern[#, Blank[]], NumberQ]&,theNames]

@}




@d findRootArgNames
@{funcArgs=Flatten[genSlots[numX+numEps]],
zArgs=Table[Unique["theFRZArgs"],{numZ}],
xArgs=Table[Unique["theFRXArgs"],{numX}],
xLagArgs=Table[Unique["theFRXLagArgs"],{numX}],
eArgs=Table[Unique["theFREArgs"],{numEps}]@}

@d prepFindRootXInitBoth
@{theXInit=Flatten[Apply[bothXZFuncs[[1,1]],Join[xLagArgs,eArgs]]],
funcOfXtm1Eps=Unique["fNameXtm1Eps"],
funcOfXtZt=Unique["fNameXtZt"]
@}
@d cmptXArgsInit
@{xArgsInit=If[varRanges==={},
MapThread[Function[{xx,yy},{xx,yy}],
{xArgs,theXInit[[Range[numX]]]}],
If[VectorQ[varRanges],
MapThread[{#1,#2}&,{xArgs(*,theXInit[[Range[numX]]]*),varRanges}],
MapThread[{#1,#2,#3[[1]],#3[[2]]}&,{xArgs,theXInit[[Range[numX]]],varRanges}]]]@}

@d makeArgPatternsBoth
@{xtm1epsArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs]],
xtztArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makePatternArgs[xArgs],makePatternArgs[zArgs]],
xtNoZtArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makePatternArgs[xArgs]]@}

@d setDelayedTradFXtZtBoth
@{SetDelayed[
funcOfXtZt[
(**)
Apply[Sequence,xtNoZtArgPatterns]],
Module[{},
With[{
xkAppl=Flatten[
Join[xLagArgs,xArgs,
(Apply[bothXZFuncs[[1,2]],xArgs][[Range[numX]]]),eArgs]]},
With[{eqnAppl=Apply[eqnsFunc,Flatten[xkAppl]]},
Flatten[Join[eqnAppl]]]]]]@}

@d setDelayedSeriesFXtZtBoth
@{SetDelayed[
funcOfXtZt[
(**)
Apply[Sequence,xtztArgPatterns]],
Module[{theZsNow=genZsForFindRoot[linMod,
Transpose[{xArgs}],bothXZFuncs[[1,2]],bothXZFuncs[[2]]]},
With[{xkFunc=Catch[
(Check[genLilXkZkFunc[linMod,theZsNow,
Apply[Sequence,FilterRules[{opts},
Options[genLilXkZkFunc]]]
],
Print["trying higher throw"];Throw[$Failed,"higher"]]),_,
Function[{val,tag},Print["catchfxtzt:",{xArgs,val,tag}//InputForm];
Throw[$Failed,"fromGenLil"]]]},
With[{xkAppl=Apply[xkFunc,Join[xLagArgs,eArgs,zArgs]]},
With[{eqnAppl=Apply[eqnsFunc,Flatten[xkAppl]],
xDisc=xArgs-xkAppl[[numX+Range[numX]]]},
Flatten[Join[xDisc,eqnAppl]]]]]]]@}

@d setDelayedTradFXtm1Eps
@{SetDelayed[
funcOfXtm1Eps
[Apply[Sequence,xtm1epsArgPatterns]],
(**)
With[{frRes=FindRoot[
funcOfXtZt[Apply[Sequence,Join[xLagArgs,eArgs,xArgs]]],
Join[xArgsInit]]},If[Not[FreeQ[frRes,FindRoot]],
Throw[$Failed,"genFRExtFunc:FindRoot"]];
Transpose[{Flatten[Join[xArgs,zArgs*0]]/.frRes}]]]@}


@d setDelayedSeriesFXtm1Eps
@{SetDelayed[
funcOfXtm1Eps
[Apply[Sequence,xtm1epsArgPatterns]],
(**)
With[{frRes=FindRoot[
funcOfXtZt[Apply[Sequence,Join[xLagArgs,eArgs,xArgs,zArgs]]],
Join[xArgsInit,zArgsInit]]},If[Not[FreeQ[frRes,FindRoot]],
Throw[$Failed,"genFRExtFunc:FindRoot"]];
Transpose[{Flatten[Join[xArgs,zArgs]]/.frRes}]]]@}



@d evaluateTripleUsage
@{
evaluateTriple::usage=
"place holder for genFRExtFunc"
@}

@d evaluateTriple
@{

evaluateTriple[
@<aProcessedTriple@>,
thePt:{_?NumberQ..}]:=
Catch[
If[
Apply[preFunc,thePt],
With[{theRes=
Apply[theFunc,thePt]},
If[Apply[postFunc,{thePt,theRes}],theRes,$Failed]],
$Failed],_,Function[{val,tag},
Print["catchinevaluateTriple:",{xArgs,val,tag}//InputForm];$Failed]]


evaluateTriple[
@<aProcessedTriple@>,
thePt:{_?NumberQ..}]:=
Catch[
If[
Apply[preFunc,thePt],
With[{theRes=
Apply[theFunc,thePt]},
If[Apply[postFunc,{thePt,theRes}],theRes,$Failed]],
$Failed],_,Function[{val,tag},
Print["catchinevaluateTriple:",{xArgs,val,tag}//InputForm];$Failed]]



evaluateTripleEqnSys[
triple:{preFunc_Function,theFunc:(_Function|_CompiledFunction|_Symbol),
postFunc_Function},
thePt:{_?NumberQ..}]:=
Catch[
If[
Apply[preFunc,thePt],
With[{theRes=
Apply[theFunc,thePt]},
With[{forTest=Transpose[{Drop[thePt,Length[theRes]]}]},
If[Apply[postFunc,{thePt,forTest}],theRes,$Failed]]],
$Failed],_,Function[{val,tag},
Print["catchinevaluateTriple:",{xArgs,val,tag}//InputForm];$Failed]]

@}


@d aProcessedTriple@{
triple:{preFunc_Function,theFunc:(_Function|_CompiledFunction|_Symbol),
postFunc_Function}@}


\subsection{genLilXkZkFunc}
\label{sec:genlilxkzkfunc}

@d genLilXkZkFuncUsage
@{
genLilXkZkFunc::usage=
"@<genLilXkZkFunc noZs call@>"<>
"\n@<genLilXkZkFunc fcon call@>"<>
"\ngenerate a function that computes x z based on an assumed F sum\n"<>
"\n"
@}



@d genLilXkZkFunc fcon call
@{genLilXkZkFunc[@<linMod@>,@<fCon@>,opts:OptionsPattern[]]@}

@d fCon
@{fCon_?MatrixQ@}
\subsubsection{Apply Series Formula}
\label{sec:apply-series-formula}

@d theZs
@{theZs:{_?MatrixQ..}@}
@d genLilXkZkFunc theZs call
@{
Options[genLilXkZkFunc]={"addTailContribution"->False};
genLilXkZkFunc[@<linMod@>,@<theZs@>,opts:OptionsPattern[]]@}



@d genLilXkZkFunc
@{
@<genLilXkZkFunc fcon call@>:=
Module[{},
@<apply formula F...@>
]
@}

@d apply formula F contribution given
@{With[{numXVars=getNumX[linMod],numEpsVars=getNumEps[linMod],
numZVars=getNumZ[linMod]},
With[{theSlots=genSlots[numXVars+numEpsVars+numZVars]},
With[{xtm1Vars=theSlots[[Range[numXVars]]],
epsVars=theSlots[[numXVars+Range[numEpsVars]]],
zVars=theSlots[[numXVars+numEpsVars+Range[numZVars]]]},
With[{xtVals=genXtOfXtm1[linMod,xtm1Vars,epsVars,zVars,fCon]},
With[{xtp1Vals=genXtp1OfXt[linMod,xtVals,fCon]},
With[{fullVec=Join[xtm1Vars,xtVals,xtp1Vals,epsVars]},
With[{chk=Function[fullVec]},
chk
]]]]]]]@}


\subsection{genXtOfXtm1}
\label{sec:genxtofxtm1}



@d genXtOfXtm1
@{
(*begin code for genXtOfXtm1*)
genXtOfXtm1[@<linMod@>,xtm1Vars_?MatrixQ,epsVars_?MatrixQ,zVars_?MatrixQ,
	fCon_?MatrixQ]:=
With[{xtVals=BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . epsVars+
phi . psiZ . zVars +FF.fCon},xtVals]



(*end code for genXtOfXtm1*)
@}


\subsection{genXtp1OfXt}
\label{sec:genxtp1ofxt}



@d genXtp1OfXt
@{
(*begin code for genXtp1OfXt*)

genXtp1OfXt[@<linMod@>,xtVals_?MatrixQ,
	fCon_?MatrixQ]:=
With[{xtp1Vals=
BB.xtVals+Inverse[IdentityMatrix[Length[xtVals]]-FF] . phi . psiC+fCon},xtp1Vals]


(*end code for genXtp1OfXt*)
@}



\subsubsection{Zero F contribution }
\label{sec:zero-f-contribution}
@d genLilXkZkFunc noZs call
@{ genLilXkZkFunc[@<linMod@>,{},opts:OptionsPattern[]]@}

@d genLilXkZkFunc
@{
@<genLilXkZkFunc noZs call@>:=
@<fConZero@>
@}

@d fConZero
@{With[{numZ=getNumZ[linMod]},
With[{fCon=ConstantArray[0,{1,numZ,1}]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},theRes]]]
@}
@d genLilXkZkFunc
@{
@< genLilXkZkFunc theZs call@>:=
Module[{},
@<Z Matrices Given@>
]
tailContribution[FF_?MatrixQ,phi_?MatrixQ,theTailZ_?MatrixQ]:=
Module[{},Inverse[IdentityMatrix[Length[FF]]-FF] . phi . theTailZ]


@}

@d Z Matrices Given
@{With[{fCon=Check[fSumC[phi,FF,psiZ,theZs],Print["trying to throw low"];
Throw[$Failed,"low"]]},
With[{(*theRes=genLilXkZkFunc[linMod,fCon,
Apply[Sequence,FilterRules[{opts},
Options[genLilXkZkFunc]]]
],*)numZs=Length[theZs]},
If[And[OptionValue["addTailContribution"],numZs>=1],
With[{tailCon=MatrixPower[FF,Length[theZs]+1].tailContribution[FF,phi,theZs[[-1]]]},
genLilXkZkFunc[linMod,fCon+tailCon]],
genLilXkZkFunc[linMod,fCon]]]
]
@}

@d genZsForFindRoot
@{
Print["should update genZsForFindRoot to use conditional expectation instead of using hmat********************************"];
genZsForFindRoot[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,
phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
backLookingInfo:{{_Integer,backLooking_,backLookingExp_}...}},
	initVec_?MatrixQ,theCondExp:(_Function|_CompiledFunction),iters_Integer]:=
Module[{},
With[{numX=Length[initVec],
 	thePath=
Check[iterateDRCE[theCondExp,initVec,iters+1],
Print["problems with current DRCE, at",initVec,"linMod!!!!!"];
iterateDRCE[genX0Z0Funcs[linMod],initVec,iters+1]]},
With[{restVals=
  Map[(theHMat .thePath[[Range[3*numX]+numX*(#-1)]] -psiC)&,
Range[(Length[thePath]/numX)-3]]},
      restVals
]]]




@}

@d iterateDRCEUsage
@{
iterateDRCE::usage="iterateDRCE[drExpFunc:(_Function|_CompiledFunction|_Symbol),initVec_?MatrixQ,numPers_Integer]"
@}


@d iterateDRCE
@{

iterateDRCE[drExpFunc:(_Function|_CompiledFunction|_Symbol),
initVec_?MatrixQ,numPers_Integer]:=
	With[{numX=Length[initVec]},
With[{iterated=
NestList[Function[xx,((Transpose[{Flatten[Apply[drExpFunc,Flatten[xx]]]}]))],
Identity[initVec],numPers]},
Apply[Join,
(Map[Function[xx,Identity[xx[[Range[numX]]]]],iterated])]]]/;
And[numPers>0]

iterateDRCE[drExpFunc:(_Function|_CompiledFunction|_Symbol),
initVec_?MatrixQ,0]:={}


@}

\subsubsection{fSumC}
\label{sec:fsumc}
@d fSumCUsage
@{
fSumC::usage=
"compiled function computing the sum of the Zs weighted by F"
@}

@d fSumC
@{

fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},
With[{numXVars=Length[psiZ]},
With[{fPows=Drop[NestList[Function[xx,FF. xx],
IdentityMatrix[numXVars],Length[zPath]],-1]},
Apply[Plus,
MapThread[Function[{xx,yy},Dot[xx,phi.psiZ.yy]],{fPows , zPath}]]]]]

@}



\section{parallelDoGenericIterREInterp}


@d parallelDoGenericIterREInterpUsage
@{
parallelDoGenericIterREInterp::usage=
"place holder for info";
@}

@d parallelDoGenericIterREInterp
@{
(*begin code for doSmolyakIterREInterp*)

Options[parallelDoGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False,"addTailContribution"->False}

@}


@d parallelDoGenericIterREInterp
@{
parallelDoGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<bothXZFuncs@>,
@<rawTriples@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,
triples,Apply[Sequence,FilterRules[{opts},
Options[genFRExtFunc]]]],"theFuncs"];
Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=
parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,
genericInterp,svmArgs]},
theFuncs]]


@}




\subsection{smolyakInterpolationPrep}
\label{sec:smoly}

Applies techniques for \cite{Judd2014}.
Takes model specification and prepares inputs for smolyakInterpolation function.
\begin{description}
\item[xPts] The smolyak points
\item[smolMat]The matrix for computing the weights
\item[smolPolys] The polynomial basis
\item[intSolPolys] The expected value of each polynomial basis
\end{description}

@d smolyakInterpolationPrepUsage
@{smolyakInterpolationPrep::usage="place holder"
smolPolyDrvs::usage="derivatives of smolyak polynomials"
backXtoZ::usage="backXtoZ[theXs_?MatrixQ,theMeans_?VectorQ,theSDs_?VectorQ,"<>
"theV_?MatrixQ]"
backZtoX::usage="backZtoX[theXs_?MatrixQ,theMeans_?VectorQ,theSDs_?VectorQ,"<>
"theV_?MatrixQ]"
@}

@d smolyakInterpolationPrep
@{
Options[smolyakInterpolationPrep]=
{"Derivatives"->False,"ptGenerator"->chebyshevPtGenerator}
smolyakInterpolationPrep[approxLevels_?listOfIntegersQ,
smolRngs_?MatrixQ,
@<distribSpec@>,opts:OptionsPattern[]]:=
Module[{smolRes=
  sparseGridEvalPolysAtPts[approxLevels,OptionValue["ptGenerator"],
chebyshevPolyGenerator],
numVars=Length[approxLevels],numEps=Length[distribSpec[[1]]]},
With[{thePts=smolRes[[1]],smolPolys=smolRes[[2]],smolMat=smolRes[[3]]},
With[{xPts=Map[Function[xx,xformToXVec[xx,smolRngs]],thePts]},
With[{numPolys=Length[smolPolys]},
With[{intPolys=ExpandAll[smolPolyExp[smolPolys,smolRngs,distribSpec]]},
With[{dintPolys=
If[OptionValue["Derivatives"]===True,smolPolyDrvs[intPolys,smolRngs,numEps],{}]},
{xPts,smolMat,ExpandAll[smolPolys],intPolys,dintPolys}]]]]]]/;
And[Length[smolRngs]==Length[approxLevels]]

backZtoX[theZs_?MatrixQ,theMeans_?VectorQ,theSDs_?VectorQ,theV_?MatrixQ]:=
With[{theInv=Inverse[theV]},
Map[(#+theMeans)&,Map[(#*theSDs)&,Map[# . theInv&,theZs]]]]

backXtoZ[theXs_?MatrixQ,theMeans_?VectorQ,theSDs_?VectorQ,theV_?MatrixQ]:=
Map[# . theV&,
Map[(#/theSDs)&,
Map[(#-theMeans)&,theXs]]]

smolyakInterpolationPrep[approxLevels_?listOfIntegersQ,
{means_?VectorQ,stds_?VectorQ,minZs_?VectorQ,maxZs_?VectorQ,vv_?MatrixQ},
@<distribSpec@>,opts:OptionsPattern[]]:=
Module[{smolRes=
sparseGridEvalPolysAtPts[approxLevels,OptionValue["ptGenerator"],
chebyshevPolyGenerator],
smolRngs=Transpose[{minZs,maxZs}],
numVars=Length[approxLevels],numEps=Length[distribSpec[[1]]]},
With[{thePts=smolRes[[1]],smolPolys=smolRes[[2]],smolMat=smolRes[[3]]},
With[{zPts=Map[Function[xx,xformToXVec[xx,smolRngs]],thePts]},
With[{xPts=backZtoX[zPts,means,stds,vv]},
With[{numPolys=Length[smolPolys]},
With[{intPolys=ExpandAll[smolPolyExp[smolPolys,smolRngs,distribSpec]]},
With[{dintPolys=
If[OptionValue["Derivatives"]===True,
smolPolyDrvs[intPolys,smolRngs,numEps],{}]},
With[{oldSmolPolys=smolPolys/.xx->oldX,
oldIntPolys=intPolys/.xx->oldX,
oldDPolys=dPolys/.xx->oldX,
allXs=Table[xx[ii],{ii,Length[means]}],
allOldXs=Table[oldX[ii],{ii,Length[means]}]},
With[{theSubs=Thread[allOldXs->Flatten[backXtoZ[{allXs},means,stds,vv]]]},
{xPts,smolMat,
ExpandAll[smolPolys],
intPolys,
dintPolys}]]]]]]]]]/;And[Length[means]==Length[approxLevels]]


smolyakInterpolationPrep[approxLevels_?listOfIntegersQ,smolRngs_?MatrixQ]:=
Module[{smolRes=sparseGridEvalPolysAtPts[approxLevels],
numVars=Length[approxLevels]},
With[{thePts=smolRes[[1]],smolPolys=smolRes[[2]],smolMat=smolRes[[3]]},
With[{xPts=Map[Function[xx,xformToXVec[xx,smolRngs]],thePts]},
With[{numPolys=Length[smolPolys]},
a{xPts,smolMat,ExpandAll[smolPolys]}]]]]/;And[Length[smolRngs]==Length[approxLevels]]


smolPolyDrvs[theSmolPoly_List,smolRngs_?MatrixQ,numEps_Integer]:=
With[{vars=Table[xx[ii],{ii,Length[smolRngs]-numEps}]},
Map[Function[ee,Map[D[ee,#]&,vars]],theSmolPoly]]






smolPolyExp[aSmolPoly_,smolRngs_?MatrixQ,@<distribSpec@>]:=
With[{numEps=Length[distribSpec[[1]]],
polyVars=Sort[Cases[aSmolPoly,xx[_Integer]]]},
With[{theChebValSubs=Thread[polyVars->
MapThread[xformXValToCheb,{polyVars,smolRngs}]]
},
With[{numX=Length[polyVars]-numEps},
With[{intVarRes=genIntVars[numX,distribSpec]},
With[{polyEps=Drop[polyVars,numX],intEps=Drop[intVarRes[[2]],numX]},
With[{epsSubs=MapThread[#1->#2&,{polyEps,intEps}]},
With[{funcGuts=((aSmolPoly/.theChebValSubs)/.epsSubs)},
myExpectation[funcGuts,intVarRes[[3]]]]]]]]]]







xformXValToCheb[xVal_,
range:{lowVal_?NumberQ,highVal_?NumberQ}]:=
xFormToChebInterval[xVal,lowVal,highVal]




xformChebValToX[chebVal_,
range:{lowVal_?NumberQ,highVal_?NumberQ}]:=
xFormFromChebInterval[chebVal,lowVal,highVal]




xformToXVec[chebPt_?VectorQ,ranges_?MatrixQ]:=
MapThread[xformChebValToX,{chebPt,ranges}]


@}



\subsection{myExpectation}
\label{sec:myexpectation}


@d myExpectationUsage
@{myExpectation::usage=
"place holder for myExpectation"
@}

@d myExpectation
@{
(*begin code for myExpectation*)


myExpectation[farg_List,nArgs_List]:=
stringArgsToInt[
Expectation[intArgsToString[farg],intArgsToString[nArgs]]]


intArgsToString[exp_]:=exp/.xx[val_Integer]:>xx[ToString[val]]

stringArgsToInt[exp_]:=exp/.xx[val_String]:>xx[ToExpression[val]]

(*end code for myExpectation*)
@}





\subsection{genIntVars}
\label{sec:genintvars}



@d genIntVarsUsage
@{genIntVars::usage=
"place holder for genIntVars"
@}

@d genIntVars
@{
(*begin code for genIntVars*)
 genIntVars[numX_Integer,@<distribSpec@>]:=
With[{xVars=Table[Unique["xV"],{numX}],
	dists=getDistribs[distribSpec],
	distVars=Table[Unique["epIntV"],{getNumEpsVars[distribSpec]}]},
With[{xEpsVars=Join[xVars,distVars],
	intArg=
MapThread[Function[{xx,yy},xx \[Distributed] yy],{distVars,dists}]},
	{xVars,xEpsVars,intArg}]]


(*end code for genIntVars*)
@}


\subsection{parallelMakeGenericInterpFuncs}
@d parallelMakeGenericInterpFuncsUsage
@{
parallelMakeGenericInterpFuncs::usage=
"place holder for makeGenericInterpFuncs";
@}


@d parallelMakeGenericInterpFuncs
@{



parallelMakeGenericInterpFuncs[@<rawTriples@>,
backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
Module[{},
With[{interpData=parallelSmolyakGenInterpData[triples,smolGSpec]},
interpDataToFunc[interpData,backLookingInfo,smolGSpec,genericInterp,svmArgs]]]

interpDataToFunc[interpData_?MatrixQ,
backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...}]:=
Module[{},tn=AbsoluteTime[];
With[{numArgs=Length[smolPts[[1]]]},
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["f03Args"],{numArgs}],theXs=Table[xx[ii],{ii,numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}],
funcSubs=Thread[theXs->funcArgs]},
With[{interpFuncList=
ParallelMap[Function[funcIdx,
With[{theInterps=genericInterp[interpData[[All,funcIdx]],smolGSpec,svmArgs]},
With[{smolApp=theInterps},
smolApp]]],Range[numFuncs]]},
(*Print[NKs[],"intdattofunc1:",AbsoluteTime[]-tn];*)
Sow[AbsoluteTime[]-tn,"intdattofunc1"];tn=AbsoluteTime[];
With[
{applied=Transpose[{ParallelMap[notApply[#,funcArgs]/.funcSubs&,
Map[First,interpFuncList]]}],
appliedExp=Transpose[{ParallelMap[notApply[#,funcArgs]/.funcSubs&,
Map[Last,interpFuncList]]}]},
With[{thePair=
{
ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}]/.notApply->Apply,
ReplacePart[
	Function[xxxxxxx, appliedExp],
		{1->Drop[longFuncArgs,-numEps]}]/.notApply->Apply
}},
With[{outgoing=
{replaceEqnOrExp[thePair[[1]],longFuncArgs,2,backLookingInfo],
replaceEqnOrExp[thePair[[2]],Drop[longFuncArgs,-numEps],3,backLookingInfo]}},
(*Print[NKs[],"intdattofunc2:",AbsoluteTime[]-tn];*)
Sow[AbsoluteTime[]-tn,"intdattofunc2"];
outgoing
]]]]]]]]




@}

\subsection{parallelSmolyakGenInterpData}
\label{sec:parall}

@d parallelSmolyakGenInterpData
@{
 

parallelSmolyakGenInterpData[
@<rawTriples@>,@<smolGSpec@>]:=
Module[{},
With[{filledPts=Map[Function[xx,fillIn[{{},smolToIgnore,xx}]],N[smolPts]]},
With[{theVals=
ParallelTable[evaluateTriple[aTriple,Flatten[aPt]],
{aPt,filledPts},{aTriple,triples[[1]]}]},
With[{toWorkOn={filledPts,theVals}//Transpose},
tn=AbsoluteTime[];
With[{interpData=
ParallelMap[With[{baddy=#},Catch[
Apply[selectorFunc[triples],#],
_,Function[{val,tag},Print["catchsmolGenInterp: aborting",
{val,tag,baddy,triples,filledPts}//InputForm];
Abort[]]]]&,toWorkOn]},
(*Print[NKs[],"psgid:",AbsoluteTime[]-tn];Sow[AbsoluteTime[]-tn,"psgid"];*)
interpData]]]]]

@}




\subsection{fillIn}
\label{sec:fillin}



@d fillInUsage
@{fillIn::usage=
"place holder for fillIn"
@}

@d fillIn
@{
(*begin code for fillIn*)

fillIn[args___]:=Print["wrong args for fillIn",{args}];
fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
If[MemberQ[toIgnore,
Length[theRes]+1],fillIn[{Append[theRes,1],Drop[toIgnore,1],shortVec}],
fillIn[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;
OrderedQ[toIgnore]


(*end code for fillIn*)
@}


\subsection{fillInSymb}
\label{sec:fillinsymb}



@d fillInSymbUsage
@{fillInSymb::usage=
"place holder for fillInSymb"
@}

@d fillInSymb
@{
(*begin code for fillInSymb*)

fillInSymb[{theRes:{___},toIgnore:{_Integer...},shortVec:{___}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
If[MemberQ[toIgnore,Length[theRes]+1],
fillInSymb[{Append[theRes,Unique["ig"]],Drop[toIgnore,1],shortVec}],
fillInSymb[{Append[theRes,shortVec[[1]]],
toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]

fillInSymb[{theRes:{___},toIgnore:{_Integer...},shortVec:{___}}]:=
fillInSymb[{theRes,Sort[toIgnore],shortVec}]

(*end code for fillInSymb*)
@}

@d replaceEqnOrExp
@{
replaceEqnOrExp[vecFunc_Function,theVars_List,indx_Integer,
backLookingInfo:{{_Integer,_,_}...}]:=
With[{theRes=Map[Function[uu,{uu[[1]],Apply[uu[[indx]],
Flatten[theVars]]}],backLookingInfo]},
Fold[ReplacePart[#1,{2,#2[[1]]}->#2[[2]]]&,vecFunc,theRes]]

@}


@d makeSubs
@{
makeSubs[thisFunc_Function,someArgs_List]:=
MapThread[#1->#2&,{thisFunc[[1]],someArgs}]
@}




\subsection{smolyakInterpolation}
\label{sec:smolyakinterpolation}

The function returns both the level and the derivative approximating functions.

@d smolyakInterpolation
@{

maxExponent[thePoly_,theVars_List]:=
With[{pows=Map[First,CoefficientRules[thePoly,theVars]]},
Max[Map[Apply[Plus,#]&,pows]]]

smolyakInterpolation[fVals:{_?NumberQ..},@<smolGSpec@>,{}]:=
smolyakInterpolation[fVals,smolGSpec]

smolyakInterpolation[fVals:{_?NumberQ..},@<smolGSpec@>]:=
With[{wts=LinearSolve[smolMat,fVals],numVars=Length[smolRngs]},
With[{origXs=Table[xx[ii],{ii,numVars}],
theXs=Table[Unique["xx"],{ii,numVars}],
moreXs=Table[Unique["xx"],{ii,numVars}]},
With[{shortOrigXs=Drop[origXs,-numEps],
shortXs=Drop[theXs,-numEps],
moreShortXs=Drop[moreXs,-numEps],
shortSmolRngs=Drop[smolRngs,-numEps]},
With[{preInt=({theXs,ExpandAll[
(wts.(smolPolys/.Thread[origXs->theXs]))/.
Thread[theXs->MapThread[xformXValToCheb,{theXs,smolRngs}]]]}),
postInt=({shortXs,ExpandAll[
(wts.(smolIntPolys/.Thread[shortOrigXs->shortXs]))]})
},
With[{taylorOrd=maxExponent[preInt[[2]],preInt[[1]]]},
With[{preIntTaylor={
preInt[[1]],multivariateTaylor[preInt[[2]],preInt[[1]],taylorOrd]},
postIntTaylor={
postInt[[1]],multivariateTaylor[postInt[[2]],postInt[[1]],taylorOrd]}},
If[ergodic==={},
{Apply[Function,preIntTaylor],
Apply[Function,postIntTaylor]},
With[{xformed=Flatten[backXtoZ[{moreXs},means,stds,vv]]},
{Apply[Function,{moreXs,Apply[
Apply[Function,preIntTaylor],xformed]}],
Apply[Function,{Drop[moreXs,-numEps],Apply[
Apply[Function,postIntTaylor],Drop[xformed,-numEps]]}]}]]]]]]]]

@}

@d genFRExtFunc
@{


multivariateTaylor[xx_?NumberQ,_,_]:=xx

multivariateTaylor[thePoly_,theVars:{_Symbol..},theOrd_Integer]:=
With[{newVar=Unique["ee"]},
With[{newArgs=newVar*theVars,
thePolyFunc=Apply[Function,{theVars,thePoly}]},
Normal[Series[Apply[thePolyFunc,newArgs],{newVar,0,theOrd}]]/.newVar->1]]


@}



\section{parallelNestGenericIterREInterp}


@d parallelNestGenericIterREInterpUsage
@{
parallelNestGenericIterREInterp::usage=
"place holder for info";
@}



@d parallelNestGenericIterREInterp
@{

Options[parallelNestGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False,"addTailContribution"->False,"maxForCEIters"->Infinity,
"normConvTol"->10.^(-10),"maxNormsToKeep"->50}


parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<bothXZFuncs@>,
@<rawTriples@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|
svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoGenericIterREInterp[genFRExtFunc,linMod,
{xx,numSteps},triples,smolGSpec,genericInterp,svmArgs,
Apply[Sequence,FilterRules[{opts},
Options[parallelDoGenericIterREInterp]]]]],justBothXZFuncs,numIters]]


parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<bothXZFuncs@>,
@<rawTriples@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|
svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},evalPts_?MatrixQ,opts:OptionsPattern[]]:=
Module[{itCount=1,maxNorm,tol},
With[{nestResults=
NestWhileList[Function[xx,parallelDoGenericIterREInterp[genFRExtFunc,linMod,
{xx,numSteps},triples,smolGSpec,genericInterp,svmArgs,
Apply[Sequence,FilterRules[{opts},
Options[parallelDoGenericIterREInterp]]]]],justBothXZFuncs,
With[{theNorms=
Map[Function[notxx,
(Norm[Apply[#1[[1]],notxx]-Apply[#2[[1]],notxx]])],
evalPts]},Print[{"nestWhile:",itCount++,maxNorm=Max[theNorms],tol=Norm[theNorms]/Length[theNorms],OptionValue["normConvTol"]}];(Norm[theNorms]/Length[theNorms])>OptionValue["normConvTol"]]&,2,
OptionValue["maxForCEIters"]]},
{itCount,maxNorm,tol,nestResults}
]]



reNormTime[]:=AbsoluteTime[]/(10^9)

@}


\subsection{makeREIterFunc}



@d makeREIterFuncUsage
@{makeREIterFunc::usage=
"place holder for makeREIterFunc"
@}

@d makeREIterFunc
@{
(*begin code for makeREIterFunc*)

makeREIterFunc[drFunc:(_Function|_CompiledFunction|_Symbol),@<distribSpec@>]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[drFunc[[1]]]-numEps,numZ=0},
	genXZFuncRE[{numX,numEps,numZ},drFunc,distribSpec]]]


(*end code for makeREIterFunc*)
@}



\subsection{genXZFuncRE}
\label{sec:genxzfuncreinterpfunc}


@d genXZFuncREUsage
@{genXZFuncRE::usage=
"place holder for genXZFuncRE"
@}

@d genXZFuncRE
@{
(*begin code for genXZFuncRE*)



genXZFuncRE[{numX_Integer,ignored_Integer,numZ_Integer},
aLilXkZkFunc_Function,@<distribSpec@>]:=
With[{intVarRes=genIntVars[numX,distribSpec],
funcName=Unique["fName"]},
funcName[fNameArgs:{_?NumberQ..},idx_Integer]:=Module[{},
(Apply[aLilXkZkFunc,fNameArgs])[[idx,1]]];
With[{funcGuts=
Function[xxxx,Module[{},
Transpose[{Map[Function[xx,myNExpectation[
	(funcName[intVarRes[[2]],xx]),intVarRes[[3]]]],Range[numX+numZ]]}]]]},
	ReplacePart[funcGuts,1->intVarRes[[1]]]]]

@}

\subsection{myNExpectation}
\label{sec:mynexpectation}


@d myNExpectationUsage
@{myNExpectation::usage=
"place holder for myNExpectation"
@}

@d myNExpectation
@{
(*begin code for myNExpectation*)



myNExpectation[funcName_Symbol[funcArgs_List,idx_Integer],anEpsVar_\[Distributed] PerfectForesight]:=
Apply[funcName,Append[ReplacePart[{funcArgs},{{(1),(-1)}->0}],idx]]
myNExpectation[funcName_Symbol[funcArgs_List,idx_Integer],{anEpsVar_\[Distributed] PerfectForesight}]:=
Apply[funcName,Append[ReplacePart[{funcArgs},{{1,(-1)}->0}],idx]]

myNExpectation[funcName_Symbol[farg_List,idx_Integer],nArgs_List]:=NExpectation[funcName[farg,idx],nArgs]





(*end code for myNExpectation*)
@}


\section{RegimeSwitching}

\subsection{genRegimesBothX0Z0Funcs}
\label{sec:genx0z0funcs}


@d parallelNestGenericIterREInterp
@{
parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<regimesBothXZFuncs@>,
@<rawRegimesTriples@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|
svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
Module[{theIters=getNumIters[regimesBothXZFuncs]},
NestList[Function[xxx,
parallelDoGenericIterREInterp[genFRExtFunc,linMod,
resultsForIter[xxx,theIters],rawRegimesTriples,smolGSpec,genericInterp,svmArgs,
Apply[Sequence,FilterRules[{opts},
Options[parallelDoGenericIterREInterp]]]]],Map[First,regimesBothXZFuncs],numIters]]

getNumIters[@<regimesBothXZFuncs@>]:=
Map[Last,regimesBothXZFuncs]

resultsForIter[@<functionPairs@>,numIters:{_Integer..}]:=
With[{theRes=Transpose[{functionPairs,numIters}]},
theRes]
@}

@d functionPairs
@{functionPairs:{{_Function,_Function}..}
@}


@d parallelDoGenericIterREInterp
@{
parallelDoGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
@<regimesBothXZFuncs@>,
@<rawRegimesTriples@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,regimesBothXZFuncs,
rawRegimesTriples,Apply[Sequence,FilterRules[{opts},
Options[genFRExtFunc]]]],"theFuncs"];
Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=
parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,
genericInterp,svmArgs]},
theFuncs]]


@}




@d parallelMakeGenericInterpFuncs
@{
parallelMakeGenericInterpFuncs[
@<processedRegimesTriples@>,
backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
Module[{},
ParallelMap[
With[{interpData=parallelSmolyakGenInterpData[#,smolGSpec]},
interpDataToFunc[interpData,backLookingInfo,smolGSpec,
genericInterp,svmArgs]]&,
processedRegimesTriples]]

@}



@d parallelSmolyakGenInterpData
@{
 

parallelSmolyakGenInterpData[
@<processedRegimesTriples@>,@<smolGSpec@>]:=
Module[{numRegimes=Range[Length[processedRegimesTriples]],
numCases=Map[Range[Length[#[[1]]]]&,processedRegimesTriples],numPts=Length[smolPts]},
With[{filledPts=Map[Function[xxxx,fillIn[{{},smolToIgnore,xxxx}]],N[smolPts]]},
With[{preCombos=MapIndexed[Table[{#2[[1]],ii}, {ii,#}]&,numCases]},
With[{combos=
Map[Function[yyy,Map[forPoints[#,numPts]&,yyy]],preCombos]},
With[{theVals=Map[evaluateTripleToCases[processedRegimesTriples,filledPts,#1]&,
combos,{3}]},
MapThread[applySelectorFuncs[#1[[-1]],filledPts,#2]&,
{processedRegimesTriples,theVals}]
]]]]]

evaluateTripleToCases[
@<processedRegimesTriples@>,pts_?MatrixQ,
{rIndx_Integer,cIndx_Integer,pIndx_Integer}]:=
evaluateTriple[processedRegimesTriples[[rIndx,1,cIndx]],Flatten[pts[[pIndx]]]]


forPoints[soFar_?VectorQ,numPts_Integer]:=
Map[Append[soFar,#]&,Range[numPts]]

applySelectorFuncs[aSelectorFunc:(_Function|_CompiledFunction|_Symbol),
filledPts_?MatrixQ,theVals_List]:=
With[{toWorkOn={filledPts,Transpose[theVals]}//Transpose},
With[{interpData=
ParallelMap[With[{baddy=#},Catch[
Apply[aSelectorFunc,#],
_,Function[{val,tag},Print["catchsmolGenInterp: aborting",
{val,tag,baddy,triples,filledPts}//InputForm];
Abort[]]]]&,toWorkOn]},
interpData]]


@}
\subsubsection{Using both decision rule and decision rule expectation for regimes}
\label{sec:using-both-decision}


@d genFRExtFunc
@{

genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},
@<linMod@>,
@<regimesBothXZFuncs@>,
@<rawRegimesTriples@>,opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{regimeTrips=
Table[genFRExtFunc[{numX,numEps,numZ},linMod,regimesBothXZFuncs,
rawRegimesTriples[[-1]],rawRegimesTriples[[1,ii]],ii,Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],{ii,Length[rawRegimesTriples]}]},
regimeTrips
]]
@}




@d genFRExtFunc
@{
genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},@<linMod@>,
@<regimesBothXZFuncs@>,probFunc:(_Function|_CompiledFunction|_Symbol),
@<rawTriples@>,regimeIndx_Integer,
opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{funcTrips=
Map[{#[[1]],genFRExtFunc[{numX,numEps,numZ},linMod,regimesBothXZFuncs,
probFunc,#[[2]],regimeIndx,
Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],#[[3]]}&,triples[[1]]]},
{funcTrips,selectorFunc[triples]}
]]
@}

@d rawTriples
@{triples:xxx_?conditionsGroupQ@}


@d genFRExtFunc
@{

genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},
@<linMod@>,
@<regimesBothXZFuncs@>,probFunc:(_Symbol|_Function|_CompiledFunction),
@<eqnsFunc@>,regimeIndx_Integer,opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{@<findRootArgNames@>},
With[{@<prepFindRootXInitRegimesBoth@>},
With[{zArgsInit=Transpose[{zArgs,Drop[theXInit,numX]}]},
With[{@<cmptXArgsInit@>,
@<makeArgPatternsBoth@>},
(**)
Switch[OptionValue["Traditional"],
True,@<setDelayedTradFXtZtRegimesBoth@>;@<setDelayedTradFXtm1Eps@>,
False,@<setDelayedSeriesFXtZtRegimesBoth@>;@<setDelayedSeriesFXtm1Eps@>]
(**)
(**)
DistributeDefinitions[funcOfXtZt,funcOfXtm1Eps]
Off[FindRoot::srect];
Off[FindRoot::nlnum];Sow[{funcOfXtm1Eps,funcOfXtZt},"theFuncs"];
funcOfXtm1Eps
]]]]]
@}

@d prepFindRootXInitRegimesBoth
@{theXInit=Flatten[Apply[regimesBothXZFuncs[[regimeIndx,1,1]],
Join[xLagArgs,eArgs]]],
funcOfXtm1Eps=Unique["fNameXtm1Eps"],
funcOfXtZt=Unique["fNameXtZt"]
@}



@d setDelayedTradFXtZtRegimesBoth
@{SetDelayed[
funcOfXtZt[
(**)
Apply[Sequence,xtNoZtArgPatterns]],
Module[{},
With[{
xkAppl=Flatten[
Join[xLagArgs,xArgs,
Map[(Apply[#[[1,2]],xArgs][[Range[numX]]])&,regimesBothXZFuncs],eArgs]]},
With[{eqnAppl=Apply[eqnsFunc,Flatten[xkAppl]]},
Flatten[Join[eqnAppl]]]]]]@}

@d setDelayedSeriesFXtZtRegimesBoth
@{SetDelayed[
funcOfXtZt[
(**)
Apply[Sequence,xtztArgPatterns]],
Module[{theZsNow=genZsForFindRoot[linMod,
Transpose[{xArgs}],Apply[probFunc,xArgs][[regimeIndx]],regimesBothXZFuncs[[All,1,2]],probFunc,
regimesBothXZFuncs[[regimeIndx,2]]]
},
With[{xkFunc=Catch[
(Check[genLilXkZkFunc[linMod,theZsNow,
Apply[Sequence,FilterRules[{opts},
Options[genLilXkZkFunc]]]
],
Print["trying higher throw"];Throw[$Failed,"higher"]]),_,
Function[{val,tag},Print["catchfxtzt:",{xArgs,val,tag}//InputForm];
Throw[$Failed,"fromGenLil"]]]},
With[{xkAppl=Apply[xkFunc,Join[xLagArgs,eArgs,zArgs]]},
With[{eqnAppl=Apply[eqnsFunc,Flatten[xkAppl]],
xDisc=xArgs-xkAppl[[numX+Range[numX]]]},
Flatten[Join[xDisc,eqnAppl]]]]]]]@}


@d regimesBothXZFuncs
@{regimesBothXZFuncs:{
regimesJustBothXZFuncs:{
{
(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},
_Integer}..}@}



@d genRegimesBothX0Z0FuncsUsage
@{genRegimesBothX0Z0Funcs::usage=
"place holder for genRegimesBothX0Z0Funcs"
@}

@d genRegimesBothX0Z0Funcs
@{
(*begin code for genRegimesBothX0Z0Funcs*)
genRegimesBothX0Z0Funcs[@<linMods@>]:=Map[genBothX0Z0Funcs,linMods]

@}




@d genZsForFindRoot
@{

genZsForFindRoot[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,
phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
backLookingInfo:{{_Integer,backLooking_,backLookingExp_}...}},
firstSteps:{_?MatrixQ..},firstProbs_?MatrixQ,
drExpFuncs:{(_Function|_CompiledFunction|_Symbol)..},probFunc:(_Symbol|_Function|_CompiledFunction),iters_Integer]:=
Module[{numX=Length[getB[linMod]]},
With[{thePaths=
Check[
regimesExpectation[firstSteps,firstProbs,drExpFuncs,probFunc,numX,iters+1],
Print["problems with current DRCE,using at",initVec,"linMod!!!!!"]]},
With[{restVals=
Map[compZsOnPath[theHMat,psiC,numX,#]&,thePaths]},
      restVals
]]]



genZsForFindRoot[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,
phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
backLookingInfo:{{_Integer,backLooking_,backLookingExp_}...}},
firstSteps:{_?MatrixQ..},firstProbs_?MatrixQ,
drExpFuncs:{(_Function|_CompiledFunction|_Symbol)..},probFunc:(_Symbol|_Function|_CompiledFunction),iters:{_Integer..}]:=
MapThread[Flatten[genZsForFindRoot[linMod,{#1},{#2},drExpFuncs,probFunc,#3],1]&,
{firstSteps,firstProbs,iters+1}]



genZsForFindRoot[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,
phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
backLookingInfo:{{_Integer,backLooking_,backLookingExp_}...}},
firstSteps_?MatrixQ,firstProbs_?VectorQ,
drExpFuncs:{(_Function|_CompiledFunction|_Symbol)..},probFunc:(_Symbol|_Function|_CompiledFunction),iters_Integer]:=
Flatten[genZsForFindRoot[linMod,{firstSteps},{firstProbs},
drExpFuncs,probFunc,iters+1],1]


compZsOnPath[theHMat_?MatrixQ,psiC_?MatrixQ,numX_Integer,thePath:{_?MatrixQ..}]:=
With[{fullPath=Apply[Join,thePath]},
  Map[(theHMat .fullPath[[Range[3*numX]+numX*(#-1)]] -psiC)&,
Range[(Length[fullPath]/numX)-3]]]


@}


@d iterateRegimesDRValsUsage
@{
iterateRegimesDRVals::usage="iterateRegimesDRVals[drExpFunc:(_Function|_CompiledFunction|_Symbol),initVec_?MatrixQ,numPers_Integer]"
@}

@d iterateRegimesDRVals
@{


iterateRegimesDRVals[drFuncs:{(_Function|_CompiledFunction|_Symbol)..},
initVec_?MatrixQ,regimeNum_Integer]:=
Module[{},
Apply[drFuncs[[regimeNum]],Flatten[initVec]]]/;
And[Length[drFuncs]>=regimeNum>0]




iterateRegimesDRVals[drExpFuncs:{(_Function|_CompiledFunction|_Symbol)..},
initVecs:{_?MatrixQ..},numPers_?NumberQ]:=
With[{iterated=NestList[doStep[drExpFuncs,#]&,initVecs,numPers]},
iterated]/;
And[numPers>0]





doStep[drExpFuncs:{(_Function|_CompiledFunction|_Symbol)..},
initVecs:{_?MatrixQ..}]:=
With[{numX=Length[initVecs[[1]]]},Flatten[
Outer[(Apply[#2 , Flatten[#1]][[Range[numX]]])&,initVecs,drExpFuncs,1,1],1]]





@}

@d iterateRegimesDRProbsUsage
@{

iterateRegimesDRProbs::usage="iterateRegimesDRProbs"
@}


@d iterateRegimesDRProbs
@{
iterateRegimesDRProbs[initVec_?MatrixQ,
probFunc:(_Symbol|_Function|_CompiledFunction),
regimeNum_Integer]:=
Module[{},Apply[probFunc,Flatten[initVec]][[{regimeNum}]]]
And[Length[drFuncs]>=regimeNum>0]




iterateRegimesDRProbs[initVecs:{{_?MatrixQ..}..},initProbs_?VectorQ,
probFunc:(_Symbol|_Function|_CompiledFunction),numX_Integer]:=
With[{iterated=FoldList[doStepProbs[#2,#1,probFunc,numX]&,initProbs,initVecs]},
iterated]

iterateRegimesDRProbs[initVecs:{{_?MatrixQ..}...},initProbs_?VectorQ,
probFunc:(_Symbol|_Function|_CompiledFunction),numX_Integer]:=
With[{iterated=FoldList[doStepProbs[#2,#1,probFunc,numX]&,initProbs,initVecs]},
iterated]

doStepProbs[initVecs:{_?MatrixQ..},initProbs_?VectorQ,
probFunc:(_Symbol|_Function|_CompiledFunction),numX_Integer]:=
With[{theTransProbs=Flatten[
Map[Apply[probFunc,Flatten[#][[Range[numX]]]]&,
initVecs],1]},Flatten[MapThread[#1*#2&,{initProbs,theTransProbs}]]]





@}


\subsubsection{regimes expectation}
\label{sec:using-both-decision}



@d processedRegimesTriples
@{processedRegimesTriples:xx_?processedRegimesGroupQ@}

@d rawRegimesTriples
@{rawRegimesTriples:xx_?regimesGroupQ@}




@d patternMatchCode
@{
aTripleQ[xx_]:=MatchQ[xx,{_Function,(_Function|_CompiledFunction|_Symbol),_Function}]

conditionsGroupQ[xx_]:=MatchQ[xx,{{_?aTripleQ..},(_Function|_CompiledFunction|_Symbol)}]


regimesGroupQ[xx_]:=MatchQ[xx,{{_?conditionsGroupQ..},(_Function|_CompiledFunction|_Symbol)}]

processedRegimesGroupQ[xx_]:=MatchQ[xx,{_?conditionsGroupQ..}]


@}

@d regimesExpectationUsage
@{
regimesExpectation::usage="regimesExpectation"

@}

@d regimesExpectation
@{

regimesExpectation[
drFuncs:{(_Function|_CompiledFunction|_Symbol)..},
drExpFuncs:{(_Function|_CompiledFunction|_Symbol)..},
initVec_?MatrixQ,probFunc:(_Symbol|_Function|_CompiledFunction),
numXVars_Integer,
numSteps_Integer]:=
Module[{numRegimes=Length[drFuncs]},
With[{firstSteps=
Map[iterateRegimesDRVals[drFuncs,initVec,#]&,Range[numRegimes]]},
With[{furtherSteps=Map[iterateRegimesDRVals[drExpFuncs,{#},numSteps]&,
firstSteps],
firstProbs=
Map[Flatten[iterateRegimesDRProbs[initVec[[Range[numXVars]]],probFunc,#]]&,
Range[numRegimes]]},
With[{restProbs=
MapThread[iterateRegimesDRProbs[Drop[#1,-2],#2,probFunc,numXVars]&,
{furtherSteps,firstProbs}]},
With[{theProducts=MapThread[Drop[#1,1]*#2&,{furtherSteps,restProbs}]},
With[{theSums=Map[doRegime,theProducts]},
theSums]]]]]]

regimesExpectation[firstSteps:{_?MatrixQ..},firstProbs_?MatrixQ,
drExpFuncs:{(_Function|_CompiledFunction|_Symbol)..},probFunc:(_Symbol|_Function|_CompiledFunction),
numXVars_Integer,
numSteps_Integer]:=
Module[{numRegimes=Length[drFuncs]},
With[{furtherSteps=Map[iterateRegimesDRVals[drExpFuncs,{#},numSteps]&,
firstSteps]},
With[{restProbs=
MapThread[iterateRegimesDRProbs[Drop[#1,-2],#2,probFunc,numXVars]&,
{furtherSteps,firstProbs}]},
With[{theProducts=MapThread[Drop[#1,1]*#2&,{furtherSteps,restProbs}]},
With[{theSums=Map[doRegime,theProducts]},
theSums]]]]]

plusAllEvents[theEvents:{_?MatrixQ..}]:=Apply[Plus,theEvents]
doRegime[regimeEvents:{{_?MatrixQ..}..}]:=Map[plusAllEvents,regimeEvents]
@}





\appendix

\subsection{Argument Specifications}

\label{sec:argum-spec}

@d eqnsFunc
@{eqnsFunc:(_Function|_CompiledFunction|_Symbol)@}

@d distribSpec
@{distribSpec:{expctSpec:{{_Symbol,_}..}}@}


@d smolGSpec
@{smolGSpec:{smolToIgnore:{_Integer...},
smolRngs:{{_?NumberQ,_?NumberQ}..},
smolPts_?MatrixQ,
smolMat_?MatrixQ,
smolPolys_?VectorQ,
smolIntPolys_?VectorQ,
numEps_Integer,
approxLevels_?listOfIntegersQ,ergodic:{}|
{means_?VectorQ,stds_?VectorQ,minZs_?VectorQ,maxZs_?VectorQ,vv_?MatrixQ}}@}



@d linMod
@{linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
backLookingInfo:{{_Integer,_,_}...}}@|
linMod
BB
phi
FF
psiZ
psiEps
theHMat
psiC
@}


@d linMods
@{linMods:{{_?MatrixQ,_?MatrixQ,_?MatrixQ,_?MatrixQ, 
_?MatrixQ,_?MatrixQ,_?MatrixQ,
{{_Integer,_,_}...}}..}
@}

\subsection{Assess error approximation}

@d genTestPtsUsage
@{
genTestPts::usage="place holder"
@}

@d genTestPts
@{
genTestPts[theVarBounds:{minVars_?VectorQ,maxVars_?VectorQ},numPts_Integer,
numGen:(Nied|Sobol)]:=
With[{varRanges=Transpose[theVarBounds]},
With[{someUnis=numGen[numPts,Length[varRanges]]},
With[{theVars=backUnisToVars[someUnis,varRanges]},
theVars]]]

backUnisToVars[someVars_?MatrixQ,varRanges_?MatrixQ]:=
With[{},
Transpose[MapThread[doAVar,{Transpose[someVars],varRanges}]]]

doAVar[aVar_?VectorQ,aRange:{low_?NumberQ,high_?NumberQ}]:=
Map[low+#*(high-low)&,aVar]


genTestPts[theZBounds:{minZs_?VectorQ,maxZs_?VectorQ},numPts_Integer,
numGen:(Nied|Sobol),theMeans_?VectorQ,theSDs_?VectorQ,thessv_?MatrixQ,
ignored_?VectorQ]:=
With[{zRanges=Transpose[theZBounds]},
With[{someUnis=numGen[numPts,Length[zRanges]]},
With[{someZs=backUnisToVars[someUnis,zRanges]},
With[{theXs=backZtoX[someZs,theMeans,theSDs,thessv]},
Map[fillIn[{{},ignored,#}]&,theXs]]]]]
@}

@d assessErrPredictionUsage
@{
assessErrPrediction::usage="place holder"
getSlopesRSqs::usage="place holder"
varyParamsForSlSq::usage="place holder"
assessSimplestErrPrediction::usage="place holder"
assessNextSimplestErrPrediction::usage="place holder"
varyParamsGenMods::usage="varyParamsGenMods[paramRanges_?MatrixQ,modGenerator:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),numParamPts_Integer]"
doRefModel::usage="doRefModel[approx_?VectorQ]"
doGuessToRefModel::usage="doGuessToRefModel[aGuessFunc:{(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)"

@}


@d assessErrPrediction
@{
Options[assessErrPrediction]={"epsCalc"->"zero","useTail"->False}
assessErrPrediction[{@<rawTriples@>,testPts_?MatrixQ,numEps_Integer,@<linMod@>,
bestxzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)
},
{candxzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
candXZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},
candErrFunc:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
opts:OptionsPattern[]]:=
With[{
pvals=Map[candErrFunc[triples,candxzFuncs,candXZFuncs,Transpose[{#}],numEps,
linMod,opts]&,testPts]},
With[{avals=Map[(((Apply[candxzFuncs ,Flatten[#]])[[Range[4]]])-(Apply[bestxzFuncs ,Flatten[#]]))&,testPts]},
With[{
forRegs=Map[Transpose[{Flatten[pvals[[All,#]]],Flatten[avals[[All,#]]]}]&,
Range[Length[pvals[[1]]]]]},
With[{lmfs=Map[LinearModelFit[#,xx,xx]&,forRegs]},
lmfs]]]]



assessSimplestErrPrediction[{@<rawTriples@>,testPts_?MatrixQ,numEps_Integer,@<linMod@>,
bestxzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)
},
{candxzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
candXZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},
opts:OptionsPattern[]]:=
With[{
pvals=Map[simplestErrXtm1Eps[triples,candxzFuncs,candXZFuncs,Transpose[{#}],numEps,linMod,
opts]&,testPts]},
With[{avals=Map[(((Apply[candxzFuncs ,Flatten[#]])[[Range[4]]])-(Apply[bestxzFuncs ,Flatten[#]]))&,testPts]},(*Print[{"why:",pvals,avals}];*)
With[{
forRegs=Map[Transpose[{Flatten[pvals[[All,#]]],Flatten[avals[[All,#]]]}]&,
Range[Length[pvals[[1]]]]]},
With[{lmfs=Map[LinearModelFit[#,xx,xx]&,forRegs]},
lmfs]]]]


assessNextSimplestErrPrediction[{@<rawTriples@>,testPts_?MatrixQ,numEps_Integer,@<linMod@>,
bestxzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)
},
{candxzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
candXZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},
theK_Integer,
opts:OptionsPattern[]]:=
With[{
pvals=Map[nextSimplestErrXtm1Eps[triples,candxzFuncs,candXZFuncs,Transpose[{#}],numEps,
linMod,theK,opts]&,testPts]},
With[{avals=Map[(((Apply[candxzFuncs ,Flatten[#]])[[Range[4]]])-(Apply[bestxzFuncs ,Flatten[#]]))&,testPts]},
With[{
forRegs=Map[Transpose[{Flatten[pvals[[All,#]]],Flatten[avals[[All,#]]]}]&,
Range[Length[pvals[[1]]]]]},
With[{mnsStdevs=
Map[{Mean[(#[[1]]-#[[2]])/#[[2]]],StandardDeviation[(#[[1]]-#[[2]])/#[[2]]]}&,forRegs]},
With[{lmfs=Map[LinearModelFit[#,xx,xx]&,forRegs]},
{mnsStdevs,lmfs}]]]]]

Options[getSlopesRSqs]={"epsCalc"->"zero","useTail"->False}
getSlopesRSqs[
bothXZFuncs:{
xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},
@<linMod@>,
numEps_Integer,toIg_?VectorQ,approx_?VectorQ,theK_Integer,
opts:OptionsPattern[]]:=
Module[{refModData=doRefModel[approx],
lms,diffs,
resids,rsqs,bfs,mn,sd,minz,maxz,svd,
rbcEqnsFirstRBCTrips,firstRBCTripsExactDR,
firstRBCTripsExactDRCE,forErgodicInfo,theDistFirstRBCTrips,theFullXs},
{rbcEqnsFirstRBCTrips,theFullXs,firstRBCTripsExactDR,sgSpecErg}=
refModData;
{diffs,lms}=
assessNextSimplestErrPrediction[{rbcEqnsFirstRBCTrips,theFullXs,1,
linMod,firstRBCTripsExactDR},bothXZFuncs,theK,opts];
bfs=Through[lms["BestFitParameters"]];
pvs=Through[lms["ParameterPValues"]];
evs=Through[lms["EstimatedVariance"]];
rsqs=Through[lms["RSquared"]];
resids=Through[lms["FitResiduals"]];
{bfs,pvs,evs,rsqs,diffs,resids}]


Print["varyParamsForSlSq does not do approx"]
Options[varyParamsForSlSq]={"epsCalc"->"zero","useTail"->False}
varyParamsForSlSq[paramRanges_?MatrixQ,
modGenerator:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
numParamPts_Integer,approx_?VectorQ,theK_Integer,
opts:OptionsPattern[]]:=
Module[{rbcEqnsFirstRBCTrips,theFullXs,firstRBCTripsExactDR,sgSpecErg},
With[{theModsFull=varyParamsGenMods[paramRanges,modGenerator,numParamPts]},
With[{theMods=Map[genBothX0Z0Funcs,Map[First,theModsFull]]},
{rbcEqnsFirstRBCTrips,theFullXs,firstRBCTripsExactDR,sgSpecErg}=
doRefModel[approx];
numEps=Length[theModsFull[[1,-2]]];
toIg=theModsFull[[1,-1]];
(*someRes=ParallelTable[
parallelNestGenericIterREInterp[genFRExtFunc,linModNow,
{bFuncs,theK},rbcEqnsFirstRBCTrips,sgSpecErg,smolyakInterpolation,{},
theFullXs],{bFuncs,theMods},{linModNow,Map[First,theModsFull]}];*)
With[{thePreds=ParallelTable[getSlopesRSqs[bFuncs,linModNow,numEps,toIg,approx,
theK,opts],
{bFuncs,theMods},{linModNow,Map[First,theModsFull]}](*,
morePreds=ParallelTable[getSlopesRSqs[bFuncs,linModNow,numEps,toIg,approx,
theK,opts],
{bFuncs,Flatten[Flatten[someRes,1][[All,4]],1]},{linModNow,Map[First,theModsFull]}]*)
},
thePreds]]]]


varyParamsGenMods[paramRanges_?MatrixQ,
modGenerator:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
numParamPts_Integer]:=
With[{testPts=genTestPts[paramRanges,numParamPts,Nied]},
Map[Apply[modGenerator,#]&,testPts]]



doRefModel[approx_?VectorQ]:=
Module[{linModFirstRBCTrips,rbcEqnsFirstRBCTrips,firstRBCTripsExactDR,firstRBCTripsExactDRCE,forErgodicInfo,theDistFirstRBCTrips,toIg,
tryEps,numKern,theName,mthName,bothX0Z0,sgSpecErg,zPts,theFullXs},
{linModFirstRBCTrips,rbcEqnsFirstRBCTrips,firstRBCTripsExactDR,firstRBCTripsExactDRCE,forErgodicInfo,theDistFirstRBCTrips,toIg}=
(*alphaVal,ddVal,deltaVal,etaVal,rhoVal,sigmaVal,upsilonVal*)
firstRBCTrips`firstRBCGenModel[0.36,0.95,1,0.95,0.01];
{tryEps,numKern,theName,mthName,bothX0Z0,sgSpecErg,zPts,theFullXs}=
doSmolPrep[approx,999,999,forErgodicInfo,toIg,linModFirstRBCTrips,theDistFirstRBCTrips];
{rbcEqnsFirstRBCTrips,theFullXs,firstRBCTripsExactDR,sgSpecErg}]

doGuessToRefModel[aGuessFunc:{(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},
theK_Integer,@<linMod@>,
{@<rawTriples@>,
theFullXs_?MatrixQ,
firstRBCTripsExactDR:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),@<smolGSpec@>}]:=
Module[{},
{tm,ig}=Timing[
theRes=parallelNestGenericIterREInterp[genFRExtFunc,linMod,
{aGuessFunc,theK},triples,smolGSpec,smolyakInterpolation,{},theFullXs]];
theRes]






@}


\subsection{genSolutionPath}
\label{sec:gensolnpath}

@d genSolutionPathUsage
@{
genSolutionPath::usage="genSolutionPath[@<bothXZFuncs@>,initVec_?MatrixQ,numEps_Integer,iters_Integer]"
@}


@d genSolutionPath
@{
genSolutionPath[
xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
initVec_?MatrixQ,numEps_Integer,iters_Integer]:=
Module[{},
With[{numX=Length[initVec]-numEps},
With[{firstVal=Apply[xzFuncs,Flatten[initVec]][[Range[numX]]]},
With[{restVals=
iterateDRCE[XZFuncs,firstVal,iters]},
If[iters===0,Join[Drop[initVec,-numEps],firstVal],
Join[Drop[initVec,-numEps],restVals]]]]]]

@}


\subsection{compute error functions}
\label{sec:gensolnpath}

@d genSolutionErrXtm1EpsUsage
@{
genSolutionErrXtm1Eps::usage="genSolutionErrXtm1Eps[@<bothXZFuncs@>,initVec_?MatrixQ,numEps_Integer]"
@}


@d genSolutionErrXtm1Eps
@{
genSolutionErrXtm1Eps[@<rawTriples@>,
xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
initVec_?MatrixQ,numEps_Integer]:=
Module[{thePath=
genSolutionPath[xzFuncs,XZFuncs,initVec,numEps,1]},
With[{numX=Length[initVec]-numEps},
With[{theArg=Join[
Take[thePath,{1,3*numX}],initVec[[-Reverse[Range[numEps]]]]]},
With[{theVal=
ParallelTable[evaluateTripleEqnSys[aTriple,Flatten[theArg]],
{aTriple,triples[[1]]}]},
triples[[2]][theArg,theVal]]]]]

@}



@d simplestErrXtm1EpsUsage
@{
simplestErrXtm1Eps::usage="simplestErrXtm1Eps[@<bothXZFuncs@>,initVec_?MatrixQ,numEps_Integer]"
@}



@d simplestErrXtm1Eps
@{

Options[simplestErrXtm1Eps]={"useTail"->False}
simplestErrXtm1Eps[@<rawTriples@>,
xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
initVec_?MatrixQ,numEps_Integer,
@<linMod@>,
opts:OptionsPattern[]]:=
Module[{theErr=genSolutionErrXtm1Eps[triples,xzFuncs,XZFuncs,initVec,numEps]},
With[{tErr=Transpose[{theErr}]},
If[OptionValue["useTail"],
Inverse[IdentityMatrix[Length[initVec]-numEps]-FF] . phi .tErr,
phi . tErr]]]

@}




@d genSolutionErrXtEpsZeroUsage
@{
genSolutionErrXtEpsZero::usage="genSolutionErrXtEpsZero[@<bothXZFuncs@>,initVec_?MatrixQ,numEps_Integer]"
@}


@d genSolutionErrXtEpsZero
@{
Options[genSolutionErrXtEpsZero]={"epsCalc"->"zero","useTail"->False}
genSolutionErrXtEpsZero[@<rawTriples@>,
xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
initVecNoEps_?MatrixQ,numEps_Integer,
opts:OptionsPattern[]]:=
Module[{},
genSolutionErrXtm1Eps[triples,xzFuncs,XZFuncs,
Join[initVecNoEps,ConstantArray[0,{numEps,1}]],numEps]]
@}

@d nextSimplestErrXtm1EpsUsage
@{
nextSimplestErrXtm1Eps::usage="place holder"
@}

@d nextSimplestErrXtm1Eps
@{

Options[nextSimplestErrXtm1Eps]={"useTail"->False}
nextSimplestErrXtm1Eps[@<rawTriples@>,
xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
initVec_?MatrixQ,numEps_Integer,@<linMod@>,kk_Integer,
opts:OptionsPattern[]]:=
Module[{theErr=genSolutionErrXtm1Eps[triples,xzFuncs,XZFuncs,initVec,numEps],
thePath=genSolutionPath[xzFuncs,XZFuncs,initVec,numEps,kk],
numX=Length[initVec]-numEps},
With[{fPows=Drop[NestList[FF .#&,IdentityMatrix[Length[FF]],kk],1],
theArgs=
Map[makeArgsNoEps[thePath,numX,#]&,Range[kk]]},
With[{tErr=phi.Transpose[{theErr}],
restErrs=Map[Transpose[{genSolutionErrXtEpsZero[triples,
xzFuncs,XZFuncs,#,numEps]}]&,theArgs]},
With[{fProds=MapThread[#1 . phi . #2&,{fPows,restErrs}]},
(*Print[{"curious:",thePath,theArgs,restErrs,fProds,Apply[Plus,fProds]}];*)
With[{totErr=tErr+Apply[Plus,fProds]},
If[OptionValue["useTail"],
If[kk===0,
Inverse[IdentityMatrix[Length[initVec]-numEps]-FF] . totErr,
totErr+Inverse[IdentityMatrix[Length[initVec]-numEps]-FF] . fProds[[-1]]],
totErr
]]]]]]


makeArgsNoEps[thePath_?MatrixQ,numX_Integer,timeOffset_Integer]:=
Take[thePath,{1,numX}+timeOffset*numX]
@}




\subsection{Getters and Setters}
\label{sec:getters-setters}



@d getDistribsUsage
@{getDistribs::usage=
"place holder for getDistribs"
@}

@d getDistribs
@{
(*begin code for getDistribs*)

getDistribs[@<distribSpec@>]:= Map[Last,expctSpec]
(*{numReg,tranType,tranFunc}*)

(*end code for getDistribs*)
@}




@d getNumEpsVarsUsage
@{getNumEpsVars::usage=
"place holder for getNumEpsVars"
@}

@d getNumEpsVars
@{
(*begin code for getNumEpsVars*)
getNumEpsVars[@<distribSpec@>]:=Length[expctSpec]


(*end code for getNumEpsVars*)
@}


@d getNumEpsUsage
@{
getNumEps::usage=
"getNumEps[@<linMod@>]"<>
"number of eps variables"
@}


@d getNumEps
@{
getNumEps[@<linMod@>]:=
Length[getPsiEps[linMod][[1]]]
@}


@d getNumXUsage
@{
getNumX::usage=
"getNumX[@<linMod@>]"<>
"number of x variables"
@}

@d getNumX
@{
getNumX[@<linMod@>]:=
Length[getB[linMod]]
@}


@d getNumZUsage
@{
getNumZ::usage=
"getNumZ[@<linMod@>]"<>
"number of z variables"
@}

@d getNumZ
@{
getNumZ[@<linMod@>]:=
Length[getPsiZ[linMod][[1]]]

@}


@d getPsiZUsage
@{
getPsiZ::usage=
"getPsiZ[@<linMod@>]"<>
"number of z variables"
@}

@d getPsiZ
@{
getPsiZ[@<linMod@>]:=
psiZ
@}



@d getPsiEpsUsage
@{
getPsiEps::usage=
"getPsiEps[@<linMod@>]"<>
"number of z variables"
@}

@d getPsiEps
@{
getPsiEps[@<linMod@>]:=
psiEps
@}


@d getFUsage
@{
getF::usage=
"getF[@<linMod@>]"<>
"number of z variables"
@}

@d getF
@{
getF[@<linMod@>]:=
FF
@}




@d getBUsage
@{
getB::usage=
"getB[@<linMod@>]"<>
"number of z variables"
@}

@d getB
@{
getB[@<linMod@>]:=
BB
@}

@d getPhiUsage
@{
getPhi::usage=
"getPhi[@<linMod@>]"<>
"phi matrix"
@}

@d getPhi
@{
getPhi[@<linMod@>]:=
phi
@}


\subsection{ergodic function analysis}

@d ergodic function usage
@{
ergodicInfo::usage="ergodicInfo[simFunc,toIgnore]"

@}

@d ergodic function code
@{




ergodicInfo[
simFunc:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
toIgnore_?listOfIntegersQ,numEps_Integer]:=
Module[{theRes=simFunc[2000]},
       With[{numVals=Length[theRes[[1]]]},
	    With[{valCols=Range[numVals-numEps],
		  errCols=Range[numVals-numEps+1,numVals]},
	    With[{theErrs=theRes[[All,errCols]],
     toKeep=theRes[[All,Complement[valCols,toIgnore]]]},
{funcMean,funcSD,funcMinZ,funcMaxZ,funcvv}=doPCA[toKeep];
{errsMean,errsSD,errsMinZ,errsMaxZ,errsvv}=doPCA[theErrs];
{Join[funcMean,errsMean],
 Join[funcSD,errsSD],
 Join[funcMinZ,errsMinZ],
 Join[funcMaxZ,errsMaxZ],
 ArrayFlatten[{{funcvv,ConstantArray[0,{Length[funcvv],numEps}]},
	       {ConstantArray[0,{numEps,Length[funcvv]}],errsvv}}]}]]]]



doPCA[theVals_?MatrixQ]:=
Module[{funcMean,funcSD,funcMinZ,funcMaxZ,vv},
funcMean=Mean[theVals];
funcSD=StandardDeviation[theVals];
normedRes=Map[(#/funcSD)&,(Map[(#-funcMean)&,theVals])];
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
funcMinZ=Map[Min,Transpose[zz]];
funcMaxZ=Map[Max,Transpose[zz]];
{funcMean,funcSD,funcMinZ,funcMaxZ,vv}
]



@}





\section{Function Definitions}
\label{sec:function-definitions}



@o AMASeriesRepresentation.m
@{
BeginPackage["AMASeriesRepresentation`",
 {"JLink`","ProtectedSymbols`","mathSmolyak`","MSNTO`"}]
@<usage definitions@>
Begin["`Private`"]
Print["code should restrict findroot guesses to ergodic set"];
@<package code@>
End[]
EndPackage[]


@}




@d usage definitions
@{
(*Begin Usage Definitions*)
@<genBothX0Z0FuncsUsage@>
@<getNumEpsUsage@>
@<getNumXUsage@>
@<getNumZUsage@>
@<getPsiZUsage@>
@<getPsiEpsUsage@>
@<getBUsage@>
@<getPhiUsage@>
@<getFUsage@>
@<genFRExtFuncUsage@>
@<evaluateTripleUsage@>
@<genLilXkZkFuncUsage@>
@<iterateDRCEUsage@>
@<fSumCUsage@>
@<parallelDoGenericIterREInterpUsage@>
@<smolyakInterpolationPrepUsage@>
@<myExpectationUsage@>
@<genIntVarsUsage@>
@<getNumEpsVarsUsage@>
@<getDistribsUsage@>
@<smolyakInterpolationUsage@>
@<parallelMakeGenericInterpFuncsUsage@>
@<fillInUsage@>
@<fillInSymbUsage@>
@<parallelNestGenericIterREInterpUsage@>
@<iterateRegimesDRValsUsage@>
@<iterateRegimesDRProbsUsage@>
@<regimesExpectationUsage@>
@<processedRegimesTriples@>
@<rawRegimesTriples@>
@<patternMatchCode@>
@<genRegimesBothX0Z0FuncsUsage@>
@<genSolutionPathUsage@>
@<genSolutionErrXtm1EpsUsage@>
@<simplestErrXtm1EpsUsage@>
@<nextSimplestErrXtm1EpsUsage@>
@<genSolutionErrXtEpsZeroUsage@>
@<makeREIterFuncUsage@>
@<genXZFuncREUsage@>
@<myNExpectationUsage@>
@<assessErrPredictionUsage@>
@<genTestPtsUsage@>
@<ergodic function usage@>
@<doSmolPrepUsage@>
@}



@d package code
@{
@<doSmolPrep@>
@<ergodic function code@>
@<genTestPts@>
@<assessErrPrediction@>
@<myNExpectation@>
@<genXZFuncRE@>
@<makeREIterFunc@>
@<genSolutionErrXtEpsZero@>
@<simplestErrXtm1Eps@>
@<nextSimplestErrXtm1Eps@>
@<genSolutionErrXtm1Eps@>
@<genSolutionPath@>
@<genRegimesBothX0Z0Funcs@>
@<iterateRegimesDRVals@>
@<iterateRegimesDRProbs@>
@<regimesExpectation@>
@<parallelNestGenericIterREInterp@>
@<smolyakInterpolation@>
@<fillInSymb@>
@<replaceEqnOrExp@>
@<makeSubs@>
@<fillIn@>
@<parallelMakeGenericInterpFuncs@>
@<parallelSmolyakGenInterpData@>
@<getDistribs@>
@<getNumEpsVars@>
@<smolyakInterpolationPrep@>
@<myExpectation@>
@<genIntVars@>
@<parallelDoGenericIterREInterp@>
@<genXtOfXtm1@>
@<genXtp1OfXt@>
@<fSumC@>
@<iterateDRCE@>
@<genZsForFindRoot@>
@<genLilXkZkFunc@>
@<evaluateTriple@>
@<genFRExtFunc@>
@<genBothX0Z0Funcs@>
@<genSlots@>
@<getNumEps@>
@<getNumX@>
@<getNumZ@>
@<getPsiZ@>
@<getPsiEps@>
@<getB@>
@<getPhi@>
@<getF@>
@}
\subsection{Argument Specifications}
\label{sec:argum-spec}


@d smolyakInterpolationUsage
@{smolyakInterpolation::usage=
"designation for type of interpolation"
@}


@d bothXZFuncs
@{bothXZFuncs:{
justBothXZFuncs:{
xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},
numSteps_Integer}@}


\subsection{doSmolPrep}

@d doSmolPrepUsage
@{
doSmolPrep::usage="doSmolPrep[approx_?VectorQ,iters_Integer,theK_Integer,forErgodicInfo:(_Symbol|_Function|_CompiledFunction),toIg_?VectorQ]"
@}

@d doSmolPrep
@{
doSmolPrep[approx_?VectorQ,iters_Integer,theK_Integer,
forErgodicInfo:(_Symbol|_Function|_CompiledFunction),toIg_?VectorQ,
@<linMod@>,
@<distribSpec@>]:=
Module[{zPts,ptErg,tfErg,plyErg,iplyErg,dplyErg,bothX0Z0,smolStuff,smolRngErg,sgSpecErg},
(**)tryEps=0;
LaunchKernels[];numKern=Length[Kernels[]];
theName=fNameString[approx,iters,theK,numKern];
mthName=theName<>".mth";
bothX0Z0=genBothX0Z0Funcs[linMod];
{mn,sd,minz,maxz,svd}=ergodicInfo[forErgodicInfo,toIg,1];
smolStuff=
{ptErg,tfErg,plyErg,iplyErg,dplyErg}=smolyakInterpolationPrep[approx,{mn,sd,minz,maxz,svd},distribSpec];
smolRngErg=Transpose[{minz,maxz}];
sgSpecErg={toIg,smolRngErg,ptErg,tfErg,plyErg,iplyErg,1,approx,
{mn,sd,minz,maxz,svd}};
zPts=backXtoZ[ptErg,mn,sd,svd];
theFullXs=genTestPts[{minz,maxz},30,Nied,mn,sd,svd,toIg];
{tryEps,numKern,theName,mthName,bothX0Z0,sgSpecErg,zPts,theFullXs}]



fNameString[approx_?VectorQ,iters_Integer,theK_Integer,numKern_Integer]:=
Module[{},
StringReplace[dirNameString[]<>"forBetterRBC-CS-"<>ToString[approx]<>"Iters"<>ToString[iters]<>"theK"<>ToString[theK],{" "->"","{"->"-","}"->"-"}]];
dirNameString[]:=
Module[{},
aDir="resDirGen"<>"-host-"<>$MachineName<>"numKern"<>ToString[numKern]<>"-"<>ToString[Round[AbsoluteTime[]]]<>"/";
If[Not[FileExistsQ[aDir]],
CreateDirectory[aDir],aDir]]

@}

\section{Models}
\subsection{firstRBCTrips}
\subsubsection{rbcEqns}
@d rbcEqns
@{
CRRAUDrv[cc_,eta_]:=If[eta===1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]];
(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)
rbcEqns={
CRRAUDrv[cc[t],eta]-
(delta*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* (theta[t]*CRRAUDrv[cc[t],eta])),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t])
};
@}

\subsubsection{steady state solution}



\subsubsection{substitutions}

@d ssSolnSubsRE
@{
thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
Off[Solve::ifun];
thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta][[2]]];
kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk][[-1]]];
On[Solve::ifun];
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]]/.simpParamSubs;
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];

@}



@d simpParamSubsUsage
@{
simpParamSubs::usage="placeholder"
@}
@d simpParamSubs
@{
(*parameters page 21 using state 1*)
paramSubs={
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01
} ;
paramSubs={
alpha->alphaVal,
delta->deltaVal,
eta->etaVal,
rho->rhoVal,
sigma->sigmaVal
} ;


forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];


@}



\subsubsection{linModFirstRBC}
@d linModFirstRBCTripsUsage
@{
linModFirstRBCTrips::usage="linear model matrices for approx"
@}

@d linModFirstRBCTrips
@{

hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((Map[D[#,eps[theta][t]]&, rbcEqns])/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs];


hmatSymbRE=(hmatSymbRawRE//.simpSubs)//.simpParamSubs;



{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

(*Print["computing and simplifying the symbolic b phi f etc"]*)
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

hSumRE=hmatSymbRE[[All,Range[4]]]+hmatSymbRE[[All,4+Range[4]]]+hmatSymbRE[[All,8+Range[4]]];
ssSolnVecRE=({{cc},{kk},{nlPart},{theta}}//.ssSolnSubsRE)//.simpParamSubs;
psicSymbRE=hSumRE . ssSolnVecRE;
psiz=IdentityMatrix[4];


linModFirstRBCTrips={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{}};

@}
\subsection{some example initial conditions}

@d exampleInitsUsage
@{
anXEpsFirstRBCTrips::usage="for test input";
aGSpecFirstRBCTrips::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
@}

@d exampleInits
@{
anXFirstRBCTrips=Transpose[{{.2,.18,1.0,1.01}}];
anEpsFirstRBCTrips={{0.01}};
anXEpsFirstRBCTrips=Join[anXFirstRBCTrips,anEpsFirstRBCTrips];
aZFirstRBCTrips=Transpose[{{.1,.2,.3,.4}}];
anXEpsZsFirstRBCTrips=Join[anXEpsFirstRBCTrips,aZFirstRBCTrips];



probDimsFirstRBCTrips={4,1,4};

thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
anExpPF=1;
Off[Solve::ifun];
thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta][[2]]];
kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk][[-1]]];
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];
(*Print["RE done now PF"];*)
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
On[Solve::ifun];
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc //.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;
kHigh = 1.2*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;

aGSpecFirstRBCTrips={{1,3},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
      
@}

@d rbcEqnsFirstRBCTripsUsage
@{
rbcEqnsFirstRBCTrips::usage="model equations";
@}


@d rbcEqnsFirstRBCTrips
@{
  rbcEqnsFirstRBCTrips={
 { {True&,
Apply[  Compile , {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
({cct^(-1) - (alpha*delta*nltp1)/kkt^(1-alpha),
cct + kkt - 1.*kktm1^(alpha)*thetat, 
nlt - thetat/cct,
  thetat - ((N[E]^epsVal)*(thetatm1^(rho)))}/.paramSubs),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],
   True&}
 },
Function[{aPt,allRes},
If[allRes[[1]]===$Failed,Throw[$Failed,"noSolutionFound"],
Flatten[allRes[[1]]]]]};
@}

@d exact definitions usage
@{
firstRBCTripsExactDR::usage="simpRBCExactDR";
firstRBCTripsExactDRCE::usage="firstRBCTripsExactDRCE";
forErgodicInfo::usage="forErgodicInfo[numPers_Integer]";

@}


@d exact definitions code
@{
thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
sigVal = sigma //. (simpParamSubs//N);

forErgodicInfo=Function[{numPers},
With[{draws=RandomVariate[theDistFirstRBCTrips[[1,1,2]],numPers],
initVec={99,kVal,99,thVal}},
With[{vars=
FoldList[Flatten[Apply[firstRBCTripsExactDR, Append[Flatten[#1],#2]]]&,initVec,draws]},
ArrayFlatten[{{Drop[vars,1],Transpose[{draws}]}}]]]];


theDistFirstRBCTrips={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;


firstRBCTripsExactDR = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]];



thePFDistFirstRBCTrips={{{ee,PerfectForesight}}};

thExp=Expectation[(tht^rho)*E^eps,eps \[Distributed] NormalDistribution[0,sigma]];
kkExp=Expectation[(((tht^rho)*E^eps)*alpha*delta*kkt^alpha),eps \[Distributed] NormalDistribution[0,sigma]];

ccExp=Expectation[(((((tht^rho)*E^eps)*kkt^alpha)*(1-alpha*delta))),eps \[Distributed] NormalDistribution[0,sigma]];

nnExp=Expectation[((tht^rho)*E^eps)/((((((tht^rho)*E^eps)*kkt^alpha)*(1-alpha*delta)))),eps \[Distributed] NormalDistribution[0,sigma]];
 
firstRBCTripsExactDRCE=
Apply[  Function , {{cct, kkt, nlt, tht}, Flatten[
               {ccExp,kkExp,nnExp,thExp}//.paramSubs]}];



@}


\subsection{assemble code}


@o firstRBCTrips.m
@{
(* Wolfram Language Package *)
BeginPackage["firstRBCTrips`", { 
"AMASeriesRepresentation`","AMAModel`", "ProtectedSymbols`", "SymbolicAMA`"
}]
(* Exported symbols added here with SymbolName::usage *)  
@<firstRBCTripsUsage definitions@>
Begin["`Private`"]
firstRBCGenModel[
alphaVal_?NumberQ,
deltaVal_?NumberQ,
etaVal_?NumberQ,
rhoVal_?NumberQ,
sigmaVal_?NumberQ]:=
Module[{},
@<firstRBCTripsPackage code@>
{linModFirstRBCTrips,rbcEqnsFirstRBCTrips,firstRBCTripsExactDR,firstRBCTripsExactDRCE,forErgodicInfo,theDistFirstRBCTrips,{1,3}}
]
End[]
EndPackage[]
@}

@d firstRBCTripsUsage definitions
@{
firstRBCGenModel::usage="firstRBCGenModel"
@<linModFirstRBCTripsUsage@>
@<simpParamSubsUsage@>
@<exampleInitsUsage@>
@<rbcEqnsFirstRBCTripsUsage@>
@<exact definitions usage@>
@}


@d firstRBCTripsPackage code
@{
@<simpParamSubs@>
@<exampleInits@>
@<rbcEqns@>
@<ssSolnSubsRE@>
@<linModFirstRBCTrips@>
@<exact definitions code@>
@<rbcEqnsFirstRBCTrips@>
@}


\subsection{firstCompSlack}
\subsubsection{rbcCSEqns}
@d rbcEqnsNotBinding
@{
CRRAUDrv[cc_,eta_]:=If[eta===1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]];
(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqnsNotBinding={
lam[t] -CRRAUDrv[cc[t],eta],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha))) -lam[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
mu1[t],
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho)
};


@}


@d simpCSParamSubs
@{
(*parameters page 28 guerrieri iacoviello*)
paramSubs={
alpha->alphaVal,
dd->ddVal,
delta->deltaVal,
eta->etaVal,
rho->rhoVal,
sigma->sigmaVal,
upsilon->upsilonVal
} ;

forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs,simpSubs];

@}
\subsection{steady state solution}

\subsubsection{substitutions}

@d rbcCSSSSubs
@{
ssEqnSubs=
{xx_Symbol[t+v_.]->xx};



rbcEqnsNotBindingSubbed=((rbcEqnsNotBinding/.paramSubs)/.eps[theta][t]->0);
theVars=Cases[Variables[forFR=(rbcEqnsNotBindingSubbed/.ssEqnSubs)],_Symbol];
frArg=MapThread[Prepend[#1,#2]&,{{{.3604,.1,2},{.187,-1.35,5.35},{.187,0.01,25.9},
 {0,-9.,9.},{1,-10.03,10.},{2.7741,0.001,9.},{1.0001,.8,1.2}},theVars}];
ssFRSolnSubs=Prepend[Chop[FindRoot[forFR,frArg(*,MaxIterations->1000*)]],IIss->0];



@}
\subsubsection{linModFirstRBCCS}
@d linModFirstRBCCSTripsUsage
@{
linModFirstRBCCSTrips::usage="linear model matrices for approx"
@}

@d linModFirstRBCCSTrips
@{

psiz=IdentityMatrix[7];

hmatSymbRawRE=(((equationsToMatrix[
rbcEqnsNotBinding/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssFRSolnSubs)/.{eps[_]->0}//FullSimplify;

psiepsSymbRE=-Transpose[{((Map[D[#,eps[theta][t]]&, rbcEqnsNotBinding])/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssFRSolnSubs}/.simpParamSubs];



hmatSymbRE=hmatSymbRawRE//.simpParamSubs;
hSumRE=hmatSymbRE[[All,Range[7]]]+hmatSymbRE[[All,7+Range[7]]]+hmatSymbRE[[All,2*7+Range[7]]];


ssSolnVecRE={{cc},{II},{kk},{lam},{mu1},{nlPart},{theta}}//.ssFRSolnSubs;
psicSymbRE=hSumRE . ssSolnVecRE;




{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];



{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];


qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];


{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModFirstRBCCSTrips={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{7,rbcEqnsBackLookingCS,rbcEqnsBackLookingExpCS}}};


@}


\subsection{some example initial conditions}

@d exampleInitsCSUsage
@{
anXEpsFirstRBCCSTrips::usage="for test input";
forErgodicInfoCS::usage="place holder";
theDistCS::usage="place holder";
aGSpecFirstRBCCSTrips::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
@}

@d exampleInitsCS
@{
anXFirstRBCCSTrips=Transpose[{{99,99,.18,99,99,99,1.01}}];
anEpsFirstRBCCSTrips={{0.01}};
anXEpsFirstRBCCSTrips=Join[anXFirstRBCCSTrips,anEpsFirstRBCCSTrips];
aZFirstRBCCSTrips=Transpose[{{.1,.2,.3,.4,.5,.6,.7}}];
anXEpsZsFirstRBCCSTrips=Join[anXEpsFirstRBCCSTrips,aZFirstRBCCSTrips];

probDimsFirstRBCCSTrips={7,1,7};

thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
anExpPF=1;
Off[NSolve::ifun];
thSubsRE=Flatten[NSolve[theta==anExpRE*thNow[theta,0],theta,Reals][[2]]];
kSSSubRE=Flatten[NSolve[nxtK[kk,theta/.thSubsRE]==kk,kk,Reals][[-1]]];
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];
(*Print["RE done now PF"];*)
thSubsPF=Flatten[NSolve[theta==theta^rho,theta,Reals]][[1]];
kSSSubPF=Flatten[NSolve[nxtK[kk,theta/.thSubsPF]==kk,kk,Reals]][[-1]];
On[NSolve::ifun];
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc //.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;
kHigh = 1.2*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;

aGSpecFirstRBCCSTrips={{1,2,4,5,6},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
      



@}



@d rbcEqnsFirstRBCCSTripsUsage
@{
rbcEqnsFirstRBCCSTrips::usage="model equations";
@}


@d rbcEqnsFirstRBCCSTrips
@{

theProduct=upsilon*II//.ssFRSolnSubs/.paramSubs;

Print["II>=",theProduct];

argsSubs={
cc[t-1]->cctm1,
II[t-1]->iitm1,
kk[t-1]->kktm1,
lam[t-1]->lamtm1,
mu1[t-1]->mu1tm1,
nlPart[t-1]->nltm1,
theta[t-1]->thetatm1,
cc[t]->cct,
II[t]->iit,
kk[t]->kkt,
lam[t]->lamt,
mu1[t]->mu1t,
nlPart[t]->nlt,
theta[t]->thetat,
cc[t+1]->cctp1,
II[t+1]->iitp1,
kk[t+1]->kktp1,
lam[t+1]->lamtp1,
mu1[t+1]->mu1tp1,
nlPart[t+1]->nltp1,
theta[t+1]->thetatp1,
eps[theta][t]->epsVal
};




theArgs={cctm1,iitm1,kktm1,lamtm1,mutm1,nltm1,thetatm1,epsVal};

rbcBackLookingEqns={E^(rho*Log[theta[t-1]] + eps[theta][t])};
rbcBackLookingExpEqns={Expectation[rbcBackLookingEqns[[1]],eps[theta][t] \[Distributed] NormalDistribution[0,sigma]]};


rbcEqnsBackLookingCS=
Apply[Function , ({theArgs,rbcBackLookingEqns/.argsSubs}/.paramSubs)];


rbcEqnsBackLookingExpCS=
Apply[Function , ({Drop[theArgs,-1],rbcBackLookingExpEqns/.argsSubs}/.paramSubs)];

preRbcEqnsBinding={
lam[t] -CRRAUDrv[cc[t],eta],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha)))-lam[t+1]*delta*(1-dd)+mu1[t]-mu1[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
II[t] - theProduct,
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho)
};


eqnsForBind=(((preRbcEqnsBinding/.paramSubs)/.argsSubs)//.ssFRSolnSubs)//N;


eqnsForNotBind=(((rbcEqnsNotBinding/.paramSubs)/.argsSubs)//.ssFRSolnSubs)//N;



  rbcEqnsFirstRBCCSTrips={
 { 
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],
Function[{aPt,aRes},
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[2,1]]>=(theProduct)]]]},
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],(True)&}},
Function[{aPt,allRes},
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Flatten[allRes[[2]]],Flatten[allRes[[1]]]]]
};

theDistCS={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;


thVal=(theta//.ssFRSolnSubs//.(simpParamSubs//N))//N;
kVal = (kk //.ssFRSolnSubs//.(simpParamSubs//N))//N;
cVal = (cc //.ssFRSolnSubs//.(simpParamSubs//N))//N ;
(*following guerrieri and iacoviello Appendix A*)




forErgodicInfoCS[numPers_Integer]:=
With[{draws=RandomVariate[theDistCS[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}],
fMul=Inverse[IdentityMatrix[7]-fmatSymbRE]},
With[{mats=FoldList[(bmatSymbRE . #1+ (phimatSymbRE .psiepsSymbRE .{{#2}})+
fMul.phimatSymbRE.psicSymbRE)&,initVec,draws]},
With[{justVars=Map[Flatten,mats]},
ArrayFlatten[{{Drop[justVars,1],Transpose[{draws}]}}]]]];


@}


\subsection{assemble code}



@o firstRBCCSTrips.m
@{
(* Wolfram Language Package *)
BeginPackage["firstRBCCSTrips`", { 
"AMASeriesRepresentation`","AMAModel`", "ProtectedSymbols`", "SymbolicAMA`"
}]
(* Exported symbols added here with SymbolName::usage *)  
@<firstRBCCSTripsUsage definitions@>
Begin["`Private`"]
(*
alpha->.36,
dd->.1,
delta->.95,
eta->1,
rho->.95,
sigma->.01,
upsilon->0.975
*)


firstRBCCSGenModel[
alphaVal_?NumberQ,
ddVal_?NumberQ,
deltaVal_?NumberQ,
etaVal_?NumberQ,
rhoVal_?NumberQ,
sigmaVal_?NumberQ,
upsilonVal_?NumberQ]:=
Module[{},
@<firstRBCCSTripsPackage code@>
{linModFirstRBCCSTrips,rbcEqnsFirstRBCCSTrips,forErgodicInfoCS,theDistCS,{1,2,4,5,6}}
]
End[]
EndPackage[]
@}

@d firstRBCCSTripsUsage definitions
@{
firstRBCCSGenModel::usage="firstRBCCSGenModel"
@<exampleInitsCSUsage@>
@<linModFirstRBCCSTripsUsage@>
@<rbcEqnsFirstRBCCSTripsUsage@>
@}



@d firstRBCCSTripsPackage code
@{
@<simpCSParamSubs@>
@<exampleInitsCS@>
@<rbcEqnsNotBinding@>
@<rbcCSSSSubs@>
@<linModFirstRBCCSTrips@>
@<rbcEqnsFirstRBCCSTrips@>
@}


\subsection{Identifiers}
\label{sec:identifiers}

@u
\subsection{Macros}
\label{sec:macros}

@m



\end{document}
