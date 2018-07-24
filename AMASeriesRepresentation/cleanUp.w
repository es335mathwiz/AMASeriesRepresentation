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
{funcTrips,selectorFunc}
]]



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
True,@<setDelayedTradFXtZtBoth@>;@<setDelayedTradFXtm1Eps@>,
False,@<setDelayedSeriesFXtZtBoth@>;@<setDelayedSeriesFXtm1Eps@>]
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
Transpose[{xArgs}],bothXZFuncs[[1,2]],bothXZFuncs[[2]]]
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

\

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

genZsForFindRoot[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,
phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
backLookingInfo:{{_Integer,backLooking_,backLookingExp_}...}},
	initVec_?MatrixQ,theCondExp:(_Function|_CompiledFunction),iters_Integer]:=
Module[{},
With[{numX=Length[initVec],
 	thePath=
Check[iterateDRCE[theCondExp,initVec,iters+1],
Print["problems with current DRCE,using at",initVec,"linMod!!!!!"];
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
Print["interpData:",interpData];
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
Print["smolApp:",interpFuncList];
Print[NKs[],"intdattofunc1:",AbsoluteTime[]-tn];
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
Print[NKs[],"intdattofunc2:",AbsoluteTime[]-tn];
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
Apply[selectorFunc,#],
_,Function[{val,tag},Print["catchsmolGenInterp: aborting",
{val,tag,baddy,triples,filledPts}//InputForm];
Abort[]]]]&,toWorkOn]},
Print[NKs[],"psgid:",AbsoluteTime[]-tn];Sow[AbsoluteTime[]-tn,"psgid"];
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
"normConvTol"->10^(-10),"maxNormsToKeep"->50}


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
svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
Module[{},
NestWhileList[Function[xx,
parallelDoGenericIterREInterpAndInterpData[genFRExtFunc,linMod,
{xx[[1]],numSteps},triples,smolGSpec,genericInterp,svmArgs,
Apply[Sequence,FilterRules[{opts},
Options[parallelDoGenericIterREInterpAndInterpData]]]]],{justBothXZFuncs,0},
(With[{theResNow=Norm[#1[[-1]]-#2[[-1]]]},
Print[{"norm=",theResNow,
With[{lookey=
Map[Function[xxx,Max[Abs[xxx]]],#1[[-1]]-#2[[-1]]]},
getWorstValsAndLocs[lookey,OptionValue["maxNormsToKeep"]]]}];(theResNow>OptionValue["normConvTol"])])&,2,OptionValue["maxForCEIters"]]]



reNormTime[]:=AbsoluteTime[]/(10^9)

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



\section{Function Definitions}
\label{sec:function-definitions}



@o AMASeriesRepresentation.m
@{
BeginPackage["AMASeriesRepresentation`",
 {"JLink`","ProtectedSymbols`","mathSmolyak`"}]
@<usage definitions@>
Begin["`Private`"]
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
@}



@d package code
@{
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








@d rawTriples
@{triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function}@}




\subsection{Identifiers}
\label{sec:identifiers}

@u
\subsection{Macros}
\label{sec:macros}

@m



\end{document}
