\documentclass[12pt]{article}
\usepackage[margin=1.0in]{geometry}
\usepackage[english]{babel}
\usepackage{hyperref}
\usepackage{datetime}
\usepackage{amsmath}
\title{Mathematica Code for AMASeriesRepresentation Package}
\author{Gary S Anderson}

\begin{document}
\maketitle


\tableofcontents

\newpage
\section{Introduction and Summary}
\label{sec:introduction-summary}

\subsection{genX0Z0Funcs}
\label{sec:genx0z0funcs}


@d genX0Z0FuncsUsage
@{genX0Z0Funcs::usage=
"genX0Z0Funcs[@<linMod@>]:use linear reference model" <>
" to generate augmented decision rule functions"
@}

@d genX0Z0Funcs
@{
(*begin code for genX0Z0Funcs*)
genX0Z0Funcs[@<linMod@>]:=
With[{numXVars=getNumX[linMod],numZVars=getNumZ[linMod]},
With[{xtm1Vars=genSlots[numXVars]},
With[{fromLinMod=Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,
ConstantArray[0,{numZVars,1}]]},
Apply[Function,{replaceLinPart[fromLinMod,xtm1Vars,backLookingInfo]}]]]]
@}

@d replaceLinPart
@{
replaceLinPart[flm_List,xtm1Vars_List,ble:{{_Integer,_,_}...}]:=
With[{theRes=Map[Function[uu,{uu[[1]],Apply[uu[[3]],Flatten[xtm1Vars]]}],ble]},
Fold[ReplacePart[#1,#2[[1]]->#2[[2]]]&,flm,theRes]]
(*end code for genX0Z0Funcs*)
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
With[{theRes=genLilXkZkFunc[linMod,fCon,
Apply[Sequence,FilterRules[{opts},
Options[genLilXkZkFunc]]]
],numZs=Length[theZs]},
If[And[OptionValue["addTailContribution"],numZs>=1],Print["addingTailContribution"];
genLilXkZkFunc[linMod,fCon+MatrixPower[FF,Length[theZs]+1].tailContribution[FF,phi,theZs[[-1]]]],
genLilXkZkFunc[linMod,fCon]
]
]]
@}

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

@d genLilXkZkFunc fcon call
@{genLilXkZkFunc[@<linMod@>,@<fCon@>,opts:OptionsPattern[]]@}

@d fCon
@{fCon_?MatrixQ@}


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

\subsection{genFRExtFunc}
\label{sec:genfrfunc}


@d genFRExtFuncUsage
@{genFRExtFunc::usage=
"genFRExtFunc"
@}


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


@d genFRExtFunc
@{


makePatternArgs[theNames_List]:=
Map[PatternTest[Pattern[#, Blank[]], NumberQ]&,theNames]

makeBlankPatternArgs[theNames_List]:=
Map[Pattern[#, Blank[]]&,theNames]

multivariateTaylor[xx_?NumberQ,_,_]:=xx

multivariateTaylor[thePoly_,theVars:{_Symbol..},theOrd_Integer]:=
With[{newVar=Unique["ee"]},
With[{newArgs=newVar*theVars,
thePolyFunc=Apply[Function,{theVars,thePoly}]},
Normal[Series[Apply[thePolyFunc,newArgs],{newVar,0,theOrd}]]/.newVar->1]]


delayEvalEqnsFunc[numX_Integer,eqnsFunc_,xArgs_List,theApp_?MatrixQ]:=
Join[Apply[eqnsFunc,Flatten[theApp]],Flatten[xArgs-(Part[theApp,Range[numX]])]];

@}

@d genZsForFindRoot
@{

genZsForFindRoot[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,
phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
backLookingInfo:{{_Integer,backLooking_,backLookingExp_}...}},
	initVec_?MatrixQ,theCondExp:(_Function|_CompiledFunction),iters_Integer]:=
Module[{},
With[{numX=Length[initVec],
 	thePath=Flatten[
Check[iterateDRCE[theCondExp,initVec,iters+1],
Print["problems with current DRCE,using at",initVec,"linMod!!!!!"];
iterateDRCE[genX0Z0Funcs[linMod],initVec,iters+1]]]},
With[{restVals=
  Map[(theHMat .thePath[[Range[3*numX]+numX*(#-1)]] -psiC)&,
Range[(Length[thePath]/numX)-3]]},
      restVals
]]]




@}
\subsubsection{Using just decision rule  expectation}
\label{sec:using-both-decision}


@d genFRExtFunc
@{

(*begin code for genFRExtFunc*)
Options[genFRExtFunc]={"xVarRanges"->{},"Traditional"->False,"addTailContribution"->False} 



genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},@<linMod@>,@<XZFuncs@>,
@<eqnsFunc@>,opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{@<findRootArgNames@>},
With[{@<prepFindRootXInit@>},
With[{@<cmptXArgsInit@>,
@<makeArgPatterns@>},
(**)
Switch[OptionValue["Traditional"],
True,@<setDelayedTradFXtZt@>;@<setDelayedTradFXtm1Eps@>,
False,@<setDelayedSeriesFXtZt@>;@<setDelayedSeriesFXtm1Eps@>]
(**)
(**)
DistributeDefinitions[funcOfXtZt,funcOfXtm1Eps]
Off[FindRoot::srect];
Off[FindRoot::nlnum];Sow[{funcOfXtm1Eps,funcOfXtZt},"theFuncs"];
funcOfXtm1Eps
]]]]

@}


@d XZFuncs
@{XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
numSteps_Integer}@}



@d prepFindRootXInit
@{theXInit=Flatten[Apply[XZFuncs[[1]],Join[xLagArgs,eArgs]]],
zArgsInit=Map[Function[xx,{xx,0}],zArgs],
funcOfXtm1Eps=Unique["fNameXtm1Eps"],
funcOfXtZt=Unique["fNameXtZt"]
@}


@d makeArgPatterns
@{xtm1epsArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs]],
xtztArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makePatternArgs[xArgs],makePatternArgs[zArgs]],
xtNoZtArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makePatternArgs[xArgs]]@}

@d setDelayedTradFXtZt
@{SetDelayed[
funcOfXtZt[
(**)
Apply[Sequence,xtNoZtArgPatterns]],
Module[{},
With[{
xkAppl=Flatten[Join[xLagArgs,xArgs,(Apply[XZFuncs[[1]],xArgs][[Range[numX]]]),
eArgs]]},
With[{eqnAppl=Apply[eqnsFunc,Flatten[xkAppl]]},
Flatten[Join[eqnAppl]]]]]]@}

@d setDelayedSeriesFXtZt
@{SetDelayed[
funcOfXtZt[
(**)
Apply[Sequence,xtztArgPatterns]],
Module[{theZsNow=genZsForFindRoot[linMod,
Transpose[{xArgs}],XZFuncs[[1]],XZFuncs[[2]]]
},
With[{xkFunc=Catch[(Check[genLilXkZkFunc[linMod,theZsNow,
Apply[Sequence,FilterRules[{opts},
Options[genLilXkZkFunc]]]
],
Print["trying higher throw"];
Throw[$Failed,"higher"]]),_,Function[{val,tag},
Print["catchfxtzt:",{xArgs,val,tag}//InputForm];Throw[$Failed,"fromGenLil"]]]},
With[{xkAppl=Apply[xkFunc,Join[xLagArgs,eArgs,zArgs]]},
With[{eqnAppl=Apply[eqnsFunc,Flatten[xkAppl]],
xDisc=xArgs-xkAppl[[numX+Range[numX]]]},
Flatten[Join[xDisc,eqnAppl]]]]]]]@}

\subsubsection{Using both decision rule and decision rule expectation}
\label{sec:using-both-decision}


@d genFRExtFunc
@{

genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},
@<linMod@>,@<bothXZFuncs@>,
@<eqnsFunc@>,opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{@<findRootArgNames@>},
With[{@<prepFindRootXInitBoth@>},
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
]]]]
@}


@d bothXZFuncs
@{bothXZFuncs:{
justBothXZFuncs:{
xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),
XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},
numSteps_Integer}@}




@d prepFindRootXInitBoth
@{theXInit=Flatten[Apply[bothXZFuncs[[1,1]],Join[xLagArgs,eArgs]]],
zArgsInit=Map[Function[xx,{xx,0}],zArgs],
funcOfXtm1Eps=Unique["fNameXtm1Eps"],
funcOfXtZt=Unique["fNameXtZt"]
@}


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

\subsection{smolyakGenInterpData}
\label{sec:geninterpdata}

This code takes a function and a Smolyak ``grid'' specification and 
generates the data:  application of function at the Smolyak points.



@d smolyakGenInterpDataUsage
@{smolyakGenInterpData::usage=
"place holder for smolyakGenInterpData"

defaultSelectorFunc::usage="defaultSelectorFunc"

@}


@d smolyakGenInterpData
@{
(*begin code for smolyakGenInterpData*)


checkEval[aVecFunc_]:=
Catch[Function[xx,(Print["checkEval:",xx];Apply[aVecFunc,xx])],{$Failed,xx}]
 


 
smolyakGenInterpData[
aVecFunc:(_Function|_CompiledFunction|_Symbol),@<smolGSpec@>]:=
Module[{},Print["smolyakGenInterpData"];
With[{filledPts=Map[
Function[xx,fillIn[{{},smolToIgnore,xx}]],N[smolPts]]},
With[{theVals=Map[checkEval[aVecFunc],filledPts]},
With[{interpData=Map[Flatten,theVals]},
interpData]]]]
 
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
{xPts,smolMat,ExpandAll[smolPolys]}]]]]/;And[Length[smolRngs]==Length[approxLevels]]


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



\subsection{makeGenericInterpFuncs}
\label{sec:makesmolinterpfunc}




@d makeGenericInterpFuncsUsage
@{
makeGenericInterpFuncs::usage=
"place holder for makeGenericInterpFuncs"
@}

@d smolyakInterpolationUsage
@{smolyakInterpolation::usage=
"designation for type of interpolation"
@}

@d makeGenericInterpFuncs
@{
(*begin code for makeGenericInterpFuncs*)


makeGenericInterpFuncs[aVecFunc:(_Function|_CompiledFunction|_Symbol),
backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:
(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...}]:=
Module[{},
With[{interpData=smolyakGenInterpData[aVecFunc,smolGSpec],
numArgs=Length[smolPts[[1]]]},Print["interpData:",interpData];
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["f02Args"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,
With[{theInterps=genericInterp[interpData[[All,funcIdx]],smolGSpec,svmArgs]},
With[{smolApp=theInterps},
smolApp]]],Range[numFuncs]]},
With[
{applied=Transpose[{Map[notApply[#,funcArgs]/.makeSubs[#,funcArgs]&,
Map[First,interpFuncList]]}],
appliedExp=Transpose[{Map[notApply[#,funcArgs]/.makeSubs[#,Drop[funcArgs,-numEps]]&,
Map[Last,interpFuncList]]}]},Print[{"here",interpFuncList,applied,interpData}];
With[{thePair=
{
ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}]/.notApply->Apply,
ReplacePart[
	Function[xxxxxxx, appliedExp],
		{1->Drop[longFuncArgs,-numEps]}]/.notApply->Apply
}},
{replaceEqnOrExp[thePair[[1]],longFuncArgs,2,backLookingInfo],
replaceEqnOrExp[thePair[[2]],Drop[longFuncArgs,-numEps],3,backLookingInfo]}
	]
]]]]]]
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




\section{doGenericIterREInterp}

@d doGenericIterREInterpUsage
@{
doGenericIterREInterp::usage=
"place holder for info";
@}

@d doGenericIterREInterp
@{
(*begin code for doSmolyakIterREInterp*)

Options[doGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False,"addTailContribution"->False}
doGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(
smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},
opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,eqnsFunc,Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],
"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=
makeGenericInterpFuncs[reapRes[[1]],{},smolGSpec,
genericInterp,svmArgs]},
theFuncs]]
(*end code for doGenericIterREInterp*)
@}

\section{nestGenericIterREInterp}



@d nestGenericIterREInterpUsage
@{
nestGenericIterREInterp::usage=
"place holder for info"
@}


@d nestGenericIterREInterp
@{
(*begin code for nestGenericIterREInterp*)

Options[nestGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False,"addTailContribution"->False}
nestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|
svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
NestList[Function[xx,doGenericIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,
genericInterp,svmArgs,Apply[Sequence,
FilterRules[{opts},Options[doGenericIterREInterp]]]]],{99,XZFuncs[[1]]},numIters]


(*end code for nestGenericIterREInterp*)
@}



\subsection{genInterpData}
\label{sec:geninterpdata}

This code uses a function and a grid specification to generate the data
needed for constructing an interpolating function.



@d genInterpDataUsage
@{genInterpData::usage=
"place holder for genInterpData"
@}

@d genInterpData
@{
(*begin code for genInterpData*)

 
genInterpData[aVecFunc:(_Function|_CompiledFunction|_Symbol),@<gSpec@>]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=Map[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=Map[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]

@}

\subsection{gridPts}
\label{sec:gridpts}



@d gridPtsUsage
@{gridPts::usage=
"place holder for gridPts"
@}

@d gridPts
@{
(*begin code for gridPts*)
 
gridPts[@<gSpec@>]:=
With[{funcForPts=
Function[yy,
(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]][yy])]},
With[{oneDimPts=Map[funcForPts,rngs]},
With[{theOuter=Function[xx,Outer[List,Apply[Sequence,xx]]][oneDimPts]},
Flatten[theOuter,Depth[theOuter]-3]]]]


(*end code for gridPts*)
@}

\subsection{oneDimGridPts}
\label{sec:onedimgridpts}


@d oneDimGridPtsUsage
@{oneDimGridPts::usage=
"place holder for oneDimGridPts"
@}

@d oneDimGridPts
@{
(*begin code for oneDimGridPts*)

oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
If[iPts==0,{{(xLow+xHigh)2}},
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]]/;iPts>=0


(*end code for oneDimGridPts*)
@}


\subsection{parallelSmolyakGenInterpData}
\label{sec:parall}
@d parallelSmolyakGenInterpDataUsage
@{parallelSmolyakGenInterpData::usage=
"place holder for parallelSmolyakGenInterpData"
@}

@d parallelSmolyakGenInterpData
@{
 
parallelSmolyakGenInterpData[
aVecFunc:(_Function|_CompiledFunction|_Symbol),@<smolGSpec@>]:=
Module[{},
DistributeDefinitions[aVecFunc];
With[{filledPts=ParallelMap[
Check[Function[xx,fillIn[{{},smolToIgnore,xx}]],Print["aborting"];AbortKernels[];
Throw["parallelSmolyakGenInterpData"]],N[smolPts]]},
With[{theVals=ParallelMap[Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Map[Flatten,theVals]},
interpData]]]]


@}

\subsection{parallelMakeGenericInterpFuncs}
@d parallelMakeGenericInterpFuncsUsage
@{
parallelMakeGenericInterpFuncs::usage=
"place holder for makeGenericInterpFuncs";
@}


@d parallelMakeGenericInterpFuncs
@{
(*begin code for makeGenericInterpFuncs*)

parallelMakeGenericInterpFuncs[aVecFunc:(_Function|_CompiledFunction|_Symbol),
backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
Module[{},
With[{interpData=parallelSmolyakGenInterpData[aVecFunc,smolGSpec],
numArgs=Length[smolPts[[1]]]},
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["f03Args"],{numArgs}],theXs=Table[xx[ii],{ii,numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}],
funcSubs=Thread[theXs->funcArgs]},
With[{interpFuncList=
ParallelMap[Function[funcIdx,
With[{theInterps=genericInterp[interpData[[All,funcIdx]],smolGSpec,svmArgs]},
With[{smolApp=theInterps},
smolApp]]],Range[numFuncs]]},
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
{replaceEqnOrExp[thePair[[1]],longFuncArgs,2,backLookingInfo],
replaceEqnOrExp[thePair[[2]],Drop[longFuncArgs,-numEps],3,backLookingInfo]}
	]
]]]]]]


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
parallelDoGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,eqnsFunc,Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];
Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=
parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,
genericInterp,svmArgs]},
theFuncs]]

@}

\section{parallelNestGenericIterREInterp}


@d parallelNestGenericIterREInterpUsage
@{
parallelNestGenericIterREInterp::usage=
"place holder for info";
@}


@d parallelNestGenericIterREInterp
@{
(*begin code for nestGenericIterREInterp*)





Options[parallelNestGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False,"addTailContribution"->False}

parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|
svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoGenericIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,genericInterp,svmArgs,
Apply[Sequence,FilterRules[{opts},
Options[parallelDoGenericIterREInterp]]]]],{99,XZFuncs[[1]]},numIters]]

@}

\subsection{parallelMakeInterpFunc}
\label{sec:makeinterpfunc}




@d parallelMakeInterpFuncUsage
@{parallelMakeInterpFunc::usage=
"place holder for parallelMakeInterpFunc"
@}


@d parallelMakeInterpFunc
@{
(*begin code for parallelMakeInterpFunc*)



parallelMakeInterpFunc[aVecFunc:(_Function|_CompiledFunction|_Symbol),
backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>]:=
Module[{},
parallelMakeGenericInterpFuncs[aVecFunc,backLookingInfo,
smolGSpec,smolyakInterpolation,{}]]





(*end code for parallelMakeInterpFunc*)
@}

\subsection{parallelDoIterREInterp}
\label{sec:doiterreinterp}



@d parallelDoIterREInterpUsage
@{parallelDoIterREInterp::usage=
"place holder for parallelDoIterREInterp"
@}

@d parallelDoIterREInterp
@{
(*begin code for parallelDoIterREInterp*)

parallelDoIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,opts:OptionsPattern[]]:=
With[{numX=getNumX[linMod],lclnumEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[XZFuncs[[1]]];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];
Print["done distributing defs:"];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},
linMod,XZFuncs,eqnsFunc,Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];
Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=parallelMakeGenericInterpFuncs[reapRes[[1]],
backLookingInfo,smolGSpec,smolyakInterpolation,{}]},
Print["done parallelmakegenericinterpfunc:"];
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
theFuncs]]]



Options[parallelDoIterREInterp]={"xVarRanges"->{},"Traditional"->False}
@}
\subsection{parallelNestIterREInterp}
\label{sec:nestiterreinterp}


@d parallelNestIterREInterpUsage
@{parallelNestIterREInterp::usage=
"place holder for parallelNestIterREInterp"
@}

@d parallelNestIterREInterp
@{
(*begin code for parallelNestIterREInterp*)


parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,
@<smolGSpec@>,
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,Apply[Sequence,FilterRules[{opts},
Options[parallelDoIterREInterp]]]]],{ig,XZFuncs[[1]]},numIters]]




@}

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
Just ADRCE

@d genFRExtFunc
@{

genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},@<linMod@>,@<XZFuncs@>,
@<rawTriples@>,
opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{funcTrips=
Map[{#[[1]],genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,#[[2]],
Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],#[[3]]}&,triples[[1]]]},
{funcTrips,selectorFunc}
]]
@}

ADR and  ADRCE



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
@d aProcessedTriple@{
triple:{preFunc_Function,theFunc:(_Function|_CompiledFunction|_Symbol),
postFunc_Function}@}



@d evaluateTripleUsage
@{
evaluateTriple::usage=
"place holder for genFRExtFunc"
@}

@d evaluateTriple
@{

evaluateTriple[
@<aProcessedTriple@>,
thePt_?VectorQ]:=
Catch[
If[
Apply[preFunc,thePt],
With[{theRes=
Apply[theFunc,thePt]},
If[Apply[postFunc,{thePt,theRes}],theRes,$Failed]],
$Failed],_,Function[{val,tag},
Print["catchinevaluateTriple:",{xArgs,val,tag}//InputForm];$Failed]]

@}

@d parallelMakeGenericInterpFuncs
@{
parallelMakeGenericInterpFuncs[@<rawTriples@>,
backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|
svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
Module[{},
With[{interpData=parallelSmolyakGenInterpData[triples,smolGSpec],
numArgs=Length[smolPts[[1]]]},
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["f03Args"],{numArgs}],theXs=Table[xx[ii],{ii,numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}],
funcSubs=Thread[theXs->funcArgs]},
With[{interpFuncList=
ParallelMap[Function[funcIdx,
With[{theInterps=genericInterp[interpData[[All,funcIdx]],smolGSpec,svmArgs]},
With[{smolApp=theInterps},
smolApp]]],Range[numFuncs]]},
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
{replaceEqnOrExp[thePair[[1]],longFuncArgs,2,backLookingInfo],
replaceEqnOrExp[thePair[[2]],Drop[longFuncArgs,-numEps],3,backLookingInfo]}
	]
]]]]]]


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
Print["toWorkOn:",toWorkOn];
With[{interpData=
ParallelMap[With[{baddy=#},Catch[
Apply[selectorFunc,#],
_,Function[{val,tag},Print["catchsmolGenInterp: aborting",
{val,tag,baddy,triples,filledPts}//InputForm];
Abort[]]]]&,toWorkOn]},
interpData]]]]]






@}



\subsection{parallelDoGenericIterREInterp}

just  and ADRCE


@d parallelDoGenericIterREInterp
@{
parallelDoGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,
@<rawTriples@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|
svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,triples,Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];
Print["timing distributedefinitionsin parallelDoGenericIter:",
AbsoluteTiming[Apply[DistributeDefinitions,Flatten[reapRes[[2]]]]]];
With[{theFuncs=
parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,
genericInterp,svmArgs]},
theFuncs]]


@}
ADR and ADRCE

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


\subsection{parallelNestGenericIterREInterp}

just  and ADRCE
@d parallelNestGenericIterREInterp
@{
parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,
@<rawTriples@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|
svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),
svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoGenericIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},triples,smolGSpec,genericInterp,svmArgs,
Apply[Sequence,FilterRules[{opts},
Options[parallelDoGenericIterREInterp]]]]],{99,XZFuncs[[1]]},numIters]]


@}
both ADR and ADRCE

@d parallelNestGenericIterREInterp
@{
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

@}


\section{Regimes}
\subsection{genRegimesBothX0Z0Funcs}
\label{sec:genx0z0funcs}


@d parallelSmolyakGenInterpData
@{
 

parallelSmolyakGenInterpData[
@<processedRegimesTriples@>,@<smolGSpec@>]:=
Module[{numRegimes=Range[Length[processedRegimesTriples]],
numCases=Map[Range[Length[#[[1]]]]&,processedRegimesTriples],numPts=Length[smolPts]},Print["prefill"];
With[{filledPts=Map[Function[xxxx,fillIn[{{},smolToIgnore,xxxx}]],N[smolPts]]},
Print[{smolPts,smolToIgnore,numRegimes,numCases,numPts}];
With[{preCombos=MapIndexed[Table[{#2[[1]],ii}, {ii,#}]&,numCases]},
With[{combos=
Map[Function[yyy,Map[forPoints[#,numPts]&,yyy]],preCombos]},
Print[{"combos:",combos}//InputForm];
With[{theVals=Map[evaluateTripleToCases[processedRegimesTriples,filledPts,#1]&,
combos,{3}]},Print["dims",Map[Dimensions,{processedRegimesTriples,filledPts,theVals}]];
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
Print[{"toWorkOn:",toWorkOn}];
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
{funcTrips,selectorFunc}
]]
@}

@d rawTriples
@{triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function}@}


@d genFRExtFunc
@{

genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},
@<linMod@>,
@<regimesBothXZFuncs@>,probFunc:(_Symbol|_Function|_CompiledFunction),
@<eqnsFunc@>,regimeIndx_Integer,opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{@<findRootArgNames@>},
With[{@<prepFindRootXInitRegimesBoth@>},
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
]]]]
@}

@d prepFindRootXInitRegimesBoth
@{theXInit=Flatten[Apply[regimesBothXZFuncs[[regimeIndx,1,1]],
Join[xLagArgs,eArgs]]],
zArgsInit=Map[Function[xx,{xx,0}],zArgs],
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
{
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


@d getHUsage
@{
getH::usage=
"getH[@<linMod@>]"<>
"number of z variables"
@}

@d getH
@{
getH[@<linMod@>]:=
theHMat
@}


\newpage
\appendix

\section{Appendix}
\label{sec:appendix}
\subsection{Argument Specifications}

\label{sec:argum-spec}

@d gSpec
@{gSpec:{toIgnore:{_Integer...},iOrd_Integer,
rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}@}



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



@d distribSpec
@{distribSpec:{expctSpec:{{_Symbol,_}..}}@}



@d cmptXArgsInit
@{xArgsInit=If[varRanges==={},
MapThread[Function[{xx,yy},{xx,yy}],
{xArgs,theXInit[[Range[numX]]]}],
If[VectorQ[varRanges],
MapThread[{#1,#2}&,{xArgs(*,theXInit[[Range[numX]]]*),varRanges}],
MapThread[{#1,#2,#3[[1]],#3[[2]]}&,{xArgs,theXInit[[Range[numX]]],varRanges}]]]@}




@d findRootArgNames
@{funcArgs=Flatten[genSlots[numX+numEps]],
zArgs=Table[Unique["theFRZArgs"],{numZ}],
xArgs=Table[Unique["theFRXArgs"],{numX}],
xLagArgs=Table[Unique["theFRXLagArgs"],{numX}],
eArgs=Table[Unique["theFREArgs"],{numEps}]@}


@d eqnsFunc
@{eqnsFunc:(_Function|_CompiledFunction|_Symbol)@}


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


\subsection{Usage Definitions}
\label{sec:usage-definitions-1}



@d usage definitions
@{
@<genX0Z0FuncsUsage@>
@<getNumXUsage@>
@<getNumZUsage@>
@<getNumEpsUsage@>
@<getPsiZUsage@>
@<getPsiEpsUsage@>
@<getBUsage@>
@<getFUsage@>
@<getHUsage@>
@<genLilXkZkFuncUsage@>
@<fSumCUsage@>
@<genFRExtFuncUsage@>
@<smolyakGenInterpDataUsage@>
@<smolyakInterpolationPrepUsage@>
@<myExpectationUsage@>
@<genIntVarsUsage@>
@<getNumEpsVarsUsage@>
@<getDistribsUsage@>
@<fillInUsage@>
@<makeGenericInterpFuncsUsage@>
@<smolyakInterpolationUsage@>
@<fillInSymbUsage@>
@<doGenericIterREInterpUsage@>
@<nestGenericIterREInterpUsage@>
@<genInterpDataUsage@>
@<gridPtsUsage@>
@<oneDimGridPtsUsage@>
@<parallelSmolyakGenInterpDataUsage@>
@<parallelMakeGenericInterpFuncsUsage@>
@<parallelDoGenericIterREInterpUsage@>
@<parallelNestGenericIterREInterpUsage@>
@<parallelMakeInterpFuncUsage@>
@<parallelDoIterREInterpUsage@>
@<parallelNestIterREInterpUsage@>
@<genRegimesBothX0Z0FuncsUsage@>
@<genBothX0Z0FuncsUsage@>
@<evaluateTripleUsage@>
@<iterateRegimesDRValsUsage@>
@<iterateDRCEUsage@>
@<iterateRegimesDRProbsUsage@>
@<regimesExpectationUsage@>
@<genRegimesBothX0Z0FuncsUsage@>
@}

\subsection{Package Code}
\label{sec:package-definitions-1}

@d package code
@{
@<genX0Z0Funcs@>
@<genSlots@>
@<getNumX@>
@<getNumZ@>
@<getNumEps@>
@<getPsiZ@>
@<getPsiEps@>
@<getB@>
@<getF@>
@<getH@>
@<genLilXkZkFunc@>
@<fSumC@>
@<genXtOfXtm1@>
@<genXtp1OfXt@>
@<genFRExtFunc@>
@<genZsForFindRoot@>
@<smolyakInterpolationPrep@>
@<myExpectation@>
@<genIntVars@>
@<getNumEpsVars@>
@<getDistribs@>
@<smolyakGenInterpData@>
@<fillIn@>
@<makeGenericInterpFuncs@>
@<fillInSymb@>
@<replaceEqnOrExp@>
@<makeSubs@>
@<smolyakInterpolation@>
@<doGenericIterREInterp@>
@<nestGenericIterREInterp@>
@<genInterpData@>
@<gridPts@>
@<oneDimGridPts@>
@<parallelSmolyakGenInterpData@>
@<parallelMakeGenericInterpFuncs@>
@<parallelDoGenericIterREInterp@>
@<parallelNestGenericIterREInterp@>
@<parallelMakeInterpFunc@>
@<parallelDoIterREInterp@>
@<parallelNestIterREInterp@>
@<replaceLinPart@>
@<genRegimesBothX0Z0Funcs@>
@<genBothX0Z0Funcs@>
@<evaluateTriple@>
@<iterateDRCE@>
@<iterateRegimesDRVals@>
@<iterateRegimesDRProbs@>
@<regimesExpectation@>
@<patternMatchCode@>
@}


\subsection{m-File Definition}
\label{sec:m-file-definition}



@o AMASeriesRepresentation.m
@{
BeginPackage["AMASeriesRepresentation`",
 {"JLink`","ProtectedSymbols`","mathSmolyak`","MmaModelToC`"}]
@<usage definitions@>
Begin["`Private`"]
@<package code@>
End[]
EndPackage[]


@}

\section{Models}
\label{sec:models}

\subsection{betterRBC.m}
\label{sec:betterrbc.m}


@o betterRBC.m
@{
(* Wolfram Language Package *)
Print["start reading betterRBC.m"]
BeginPackage["betterRBC`", { "AMASeriesRepresentation`", 
"ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetter::usage="for test input";
anEpsBetter::usage="for test input";
anXEpsBetter::usage="for test input";
aZBetter::usage="for test input";
anXEpsZsBetter::usage="for test input";

anXFlatBetter::usage="for test input";
anEpsFlatBetter::usage="for test input";
anXEpsFlatBetter::usage="for test input";
aZFlatBetter::usage="for test input";
anXEpsZsFlatBetter::usage="for test input";
@}
@o betterRBC.m
@{


probDimsBetter::usage="for test input";
simpRBCExactX0Z0CEBetter::usage="simpRBCExactX0Z0CEBetter"
simpRBCExactZCEBetter::usage="simpRBCExactZCEBetter"
simpRBCExactZBetter::usage="simpRBCExactZBetter"
simpRBCExactX0Z0Better::usage = "simpRBCExactX0Z0Better"
simpRBCExactDRCEBetter::usage="simpRBCExactDRCEBetter";
simpRBCExactDRBetter::usage="simpRBCExactDR"
betterRBCExactCondExp::usage="betterRBCExactCondExp"
theDistBetter::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetter::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetter::usage="linear model matrices for approx"
aGSpecBetter::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},
{3,sigLow,3*sigHigh}}}";
eqnsCompiledBetter::usage="model equations function"
rbcEqnsBetter::usage="model equations"
eqnsEulerCompiledBetter::usage="eqnsEulerCompiledBetter"
betterExactXZ::usage="betterExactXZ"
betterExactZ::usage="betterExactZ"
simulateBetterRBCExact::usage="simulateBetterRBCExact[numPers_Integer]"
betterRBCMean::usage="betterRBCMean"
betterRBCSD::usage="betterRBCSD"
betterRBCvv::usage="betterRBCvv"
betterRBCMinZ::usage="betterRBCMinZ"
betterRBCMaxZ::usage="betterRBCMaxZ"

@}
@o betterRBC.m
@{

Print["at Private"]
Begin["`Private`"] (* Begin Private Context *) 








CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
CRRAUDrv[cc[t],1]-
(delta*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* (theta[t]*CRRAUDrv[cc[t],1])),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t])
}

discMapEqns00=(Append[rbcEqns[[{1,2,3}]],
(rbcEqns[[4]]/.{xx_-yy_->Log[xx]-Log[yy]})]/.{Log[betterRBC`Private`theta[zz__]]->lnTheta[zz],theta[xx__]->E^lnTheta[xx]})//PowerExpand

zfEqns=discMapEqns00[[{2,3,4}]]//PowerExpand
discMapEqns01=Append[zfEqns/.t->t+1,discMapEqns00[[1]]]//PowerExpand
(*soln=Solve[Thread[discMapEqns01==0],{cc[t+1],kk[t+1],nlPart[t+1],lnTheta[t+1]}]*)

@}
@o betterRBC.m
@{


(*parameters page 21 using state 1*)
(*
paramSubs=Rationalize[{
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01
  } ];
*)
paramSubs={
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01
  };

forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];
@}
@o betterRBC.m
@{

(*

rbcCompileGuts=(betterRBC`Private`rbcEqns/.{
betterRBC`Private`cc[t-1]->cctm1,
betterRBC`Private`kk[t-1]->kktm1,
betterRBC`Private`nlPart[t-1]->nltm1,
betterRBC`Private`theta[t-1]->thtm1,
betterRBC`Private`cc[t]->cct,
betterRBC`Private`kk[t]->kkt,
betterRBC`Private`nlPart[t]->nlt,
betterRBC`Private`theta[t]->tht,
betterRBC`Private`cc[t+1]->cctp1,
betterRBC`Private`kk[t+1]->kktp1,
betterRBC`Private`nlPart[t+1]->nltp1,
betterRBC`Private`theta[t+1]->thtp1,
eps[betterRBC`Private`theta][t]->epsVal
}//.betterRBC`Private`paramSubs)//N//InputForm
{cct^(-1) - (0.34199999999999997*nltp1)/kkt^0.64, 
 cct + kkt - 1.*kktm1^0.36*tht, nlt - (1.*tht)/cct, 
 tht - 1.*2.718281828459045^epsVal*thtm1^0.95}

*)

@}
@o betterRBC.m
@{

rbcEqnsBetter=eqnsCompiledBetter=Apply[Compile, {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
({cct^(-1) - (alpha*delta*nltp1)/kkt^(1-alpha),
cct + kkt - 1.*kktm1^(alpha)*thetat, 
nlt - thetat/cct,
thetat - ((N[E]^epsVal)*(thetatm1^(rho)))}/.paramSubs),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}]

rbcEqnsBetterSymb[
cctm1_,kktm1_,nltm1_,thetatm1_,
cct_,kkt_,nlt_,thetat_,
cctp1_,kktp1_,nltp1_,thetatp1_,
epsVal_]:=
({cct^(-1) - (alpha*delta*nltp1)/kkt^(1-alpha),
cct + kkt - 1.*kktm1^(alpha)*thetat, 
nlt - thetat/cct,
thetat - ((N[E]^epsVal)*(thetatm1^(rho)))}/.paramSubs)

@}
@o betterRBC.m
@{


(*
causes error a
CompiledFunction::cfn: 
   Numerical error encountered at instruction 2; proceeding with
     uncompiled evaluation.




Apply[eqnsCompiledBetter  , Flatten[{{1}, {0.0187324}, {1}, {1.1}, {0.293437}, {-0.0351748},      {7.51431}, {1.08125}, {0.232894}, {0.120986}, {3.96721}, 
     {1.07709}, {-0.0124264}}]]

*)

@}
@o betterRBC.m
@{
(*
eqnsEulerCompiledBetter=Apply[Compile , {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{((kkt^(16/25)) - (0.342*nltp1)*cct)/cct,
cct + kkt - 1.*kktm1^(9/25)*thetat, 
nlt - thetat/cct,
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)},"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}]
*)

@}
@o betterRBC.m
@{


Needs["CompiledFunctionTools`"]





(*If[Length[ssSolnSubsRE]===0,*)
(*Print["computing steady state subs"];*)
thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
anExpPF=1;
Off[Solve::ifun]
thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta][[2]]];
kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk][[-1]]];
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];
(*Print["RE done now PF"];*)
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
On[Solve::ifun]
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];




@}
@o betterRBC.m
@{



simpRBCExactDRBetter = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]]

thExp=Expectation[(tht^rho)*E^eps,eps \[Distributed] NormalDistribution[0,sigma]]
kkExp=Expectation[(((tht^rho)*E^eps)*alpha*delta*kkt^alpha),eps \[Distributed] NormalDistribution[0,sigma]]

ccExp=Expectation[(((((tht^rho)*E^eps)*kkt^alpha)*(1-alpha*delta))),eps \[Distributed] NormalDistribution[0,sigma]]

nnExp=Expectation[((tht^rho)*E^eps)/((((((tht^rho)*E^eps)*kkt^alpha)*(1-alpha*delta)))),eps \[Distributed] NormalDistribution[0,sigma]]
 
simpRBCExactDRCEBetter = 
Apply[  Function , {{cct, kkt, nlt, tht}, Flatten[
	       {ccExp,kkExp,nnExp,thExp}//.paramSubs]}]

makeExactArgs[kk_,tt_,ee_]:=
    With[{xt = Flatten[simpRBCExactDRBetter[ig, kk, ig, tt, ee]]}, 
     With[{xtp1 = Flatten[Apply[simpRBCExactDRCEBetter , xt]]}, 
      Append[Join[{999, kk, 999, tt}, xt, xtp1], ee]]]




(*Print["RE solutions"]*)
hmatSymbSlowRawRE00=(((equationsToMatrix[
rbcEqns]//FullSimplify)));
hmatSymbSlowRawRE01=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)))//FullSimplify;
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((Map[D[#,eps[theta][t]]&, rbcEqns])/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[4]]]+hmatSymbRE[[All,4+Range[4]]]+hmatSymbRE[[All,8+Range[4]]];

ssSolnVecRE={{cc},{kk},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;
@}
@o betterRBC.m
@{


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

(*Print["computing and simplifying the symbolic b phi f etc"]*)
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

psiz=IdentityMatrix[4]

linModBetter={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{}};

(*linModBetter=Map[Rationalize[#,1/100000000]&,linModBetter,{-1}]*)


@}
@o betterRBC.m
@{


simpRBCExactZBetter = 
Apply[ Function , {{cct, kkt, nlt, tht,ee},
   With[{args=makeExactArgs[kkt,tht,ee],
     pc=getPsiC[linModBetter],pe=getPsiEps[linModBetter]},
With[{zt=Flatten[
(getH[linModBetter] . Transpose[{ Drop[Flatten[args],-1]}])-pc-pe*ee]},zt]]}]

simpRBCExactZCEBetter = 
Apply[ Function, {{cct, kkt, nlt, tht},
   With[{args=makeExactArgs[kkt,tht,ee],
     pc=getPsiC[linModBetter],pe=getPsiEps[linModBetter]},
With[{zt=Flatten[
(getH[linModBetter] . Transpose[{ Drop[Flatten[args],-1]}])-pc-pe*ee]},
  Expectation[zt,ee \[Distributed]NormalDistribution[0,sigma//.paramSubs]]]]}]

simpRBCExactX0Z0CEBetter = 
Apply[ Function , {{cct, kkt, nlt, tht},
	      Transpose[{Flatten[
	 Join[simpRBCExactDRCEBetter[cct,kkt,nlt,tht],
	      simpRBCExactZCEBetter[cct,kkt,nlt,tht]]]}]}]



theDistBetter={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetter={{{ee,PerfectForesight}}};

@}
@o betterRBC.m
@{


simulateBetterRBCExact[numPers_Integer]:=
With[{draws=RandomVariate[theDistBetter[[1,1,2]],numPers],
initVec={99,betterRBC`Private`kVal,99,betterRBC`Private`thVal}},
FoldList[Flatten[Apply[simpRBCExactDRBetter,Append[Flatten[#1],#2]]]&,initVec,draws]]


betterRBCExactCondExp = (*AMASeriesRepresentation`Private`*)makeREIterFunc[simpRBCExactDRBetter,theDistBetter]


betterExactZ=
Function[{cc, kk, nl, th, eps},
With[{hm=getH[linModBetter],pc=getPsiC[linModBetter],pe=getPsiEps[linModBetter],
xt=Flatten[simpRBCExactDRBetter[cc,kk,nl,th,eps]]},
With[{xtp1=Flatten[Apply[betterRBCExactCondExp, xt]]},
hm.Transpose[{Join[{cc,kk,nl,th},xt,xtp1]}]-pc-pe*eps]]]

betterExactXZ=
Function[{cc, kk, nl, th, eps},
With[{xval=simpRBCExactDRBetter[cc,kk,nl,th,eps],
zval=betterExactZ[cc,kk,nl,th,eps]},
Join[xval,zval]]]



    
anXBetter=Transpose[{{.2,.18,1.0,1.1}}];
anEpsBetter={{0.01}};
anXEpsBetter=Join[anXBetter,anEpsBetter]
aZBetter=Transpose[{{.1,.2,.3,.4}}]
anXEpsZsBetter=Join[anXEpsBetter,aZBetter];

anXFlatBetter=anXBetter//Flatten;
anEpsFlatBetter=anEpsBetter//Flatten;
anXEpsFlatBetter=anXEpsBetter//Flatten;
aZFlatBetter=aZBetter//Flatten;
anXEpsZsFlatBetter=anXEpsZsBetter//Flatten;

probDimsBetter={4,1,4};


@}
@o betterRBC.m
@{


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

@}
@o betterRBC.m
@{

	(*
aGSpecBetter={{1,3},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
	 aGSpecBetter={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};*)

	 aGSpecBetter={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,sigHigh}}};
Print["about to simulate fixed seed"];
SeedRandom[1234]
theRes=simulateBetterRBCExact[200];
justKT=theRes[[All,{2,4}]];
betterRBCMean=Mean[justKT]
betterRBCSD=StandardDeviation[justKT]
normedRes=Map[(#/betterRBCSD)&,(Map[(#-betterRBCMean)&,justKT])]
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
betterRBCMinZ=Map[Min,Transpose[zz]];
betterRBCMaxZ=Map[Max,Transpose[zz]];
{ig,theKs,ig,theThetas}=Transpose[theRes];

betterRBCMean=Append[betterRBCMean,0]
betterRBCSD=Append[betterRBCSD,sigVal]
betterRBCMinZ=Append[betterRBCMinZ,-3]
betterRBCMaxZ=Append[betterRBCMaxZ,3]
betterRBCvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0},{0}}}}]},{{{0,0,1}}}}]
@}
@o betterRBC.m
@{

(*
Print["at first export"]
Export["ergodicV.pdf", MatrixForm[betterRBCvv//N]];
Print["at second export"]
Export["ergodicMaxZ.pdf", MatrixForm[betterRBCMaxZ//N]];
Print["at next export"]
Export["ergodicMinZ.pdf", MatrixForm[betterRBCMinZ//N]];
Print["at next export"]
Export["ergodicMean.pdf", MatrixForm[betterRBCMean//N]];
Print["at next export"]
Export["ergodicSD.pdf", MatrixForm[betterRBCSD//N]];
Print["at next export"]
Export["ergodicKTheta.pdf",ListPlot[Transpose[{theKs,theThetas}],PlotLabel->"Ergodic Values for K and \[Theta]"]];
Print["at next export"]
zPts=backXtoZ[Transpose[{theKs,theThetas,Table[0,{Length[theKs]}]}],betterRBCMean,betterRBCSD,betterRBCvv];Print["errBndLoc=",errBndLoc];
	Export["ergodicZs.pdf",ListPlot[zPts[[All,{1,2}]],PlotLabel->"Ergodic Values for K and \[Theta]"]];
Print["after last export"]
*)
End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBC.m"]


@}

\subsection{betterRBCFixCompSlack.m}
\label{sec:betterrbc.m}

@o betterRBCFixCompSlack.m
@{

(* Wolfram Language Package *)

BeginPackage["betterRBCFixCompSlack`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
rbcEqnsBetterCSTrips::usage="rbcEqnsBetterCSTrips"

anXBetterFixCompSlack::usage="for test input";
anEpsBetterFixCompSlack::usage="for test input";
anXEpsBetterFixCompSlack::usage="for test input";
aZBetterFixCompSlack::usage="for test input";
anXEpsZsBetterFixCompSlack::usage="for test input";

anXFlatBetterFixCompSlack::usage="for test input";
anEpsFlatBetterFixCompSlack::usage="for test input";
anXEpsFlatBetterFixCompSlack::usage="for test input";
aZFlatBetterFixCompSlack::usage="for test input";
anXEpsZsFlatBetterFixCompSlack::usage="for test input";

probDimsBetterFixCompSlack::usage="for test input";
theDistBetterFixCompSlack::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterFixCompSlack::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterFixCompSlack::usage="linear model matrices for approx"
aGSpecBetterFixCompSlack::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsBetterBackLookingFixCompSlack::usage="rbcEqnsBetterBackLookingFixCompSlack"
rbcEqnsBetterBackLookingExpFixCompSlack::usage="rbcEqnsBetterBackLookingFixCompSlack"
rbcEqnsBetterFixCompSlack::usage="model equations"
eqnsEulerCompiledBetterFixCompSlack::usage="eqnsEulerCompiledBetterFixCompSlack"
@}
@o betterRBCFixCompSlack.m
@{



simulateBetterRBCCS::usage="simulateBetterRBCExact[numPers_Integer]"
betterRBCCSMean::usage="betterRBCCSMean"
betterRBCCSSD::usage="betterRBCCSSD"
betterRBCCSvv::usage="betterRBCCSvv"
betterRBCCSMinZ::usage="betterRBCCSMinZ"
betterRBCCSMaxZ::usage="betterRBCCSMaxZ"

chkBounded::usage="chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]"


iterateRBCCSDRCE::usage="iterateRBCCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]"


@}
@o betterRBCFixCompSlack.m
@{


Begin["`Private`"] (* Begin Private Context *) 











(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)



(*parameters page 28 guerrieri iacoviello*)

paramSubs={
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01,
dd->.1,
upsilon->0.975
} ;



forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs,simpSubs];


@}
@o betterRBCFixCompSlack.m
@{

rbcEqnsNotBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha))) -lam[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1])+mu1[t]-mu1[t+1]*delta*(1-dd),
mu1[t]
}


rbcBackLookingEqns={E^(rho*Log[theta[t-1]] + eps[theta][t])}
rbcBackLookingExpEqns={Expectation[rbcBackLookingEqns[[1]],eps[theta][t] \[Distributed] NormalDistribution[0,sigma]]}







ssEqnSubs=
{xx_Symbol[t+v_.]->xx}
rbcEqnsNotBindingSubbed=((rbcEqnsNotBinding/.paramSubs)/.eps[theta][t]->0)


theVars=Cases[Variables[forFR=(rbcEqnsNotBindingSubbed/.ssEqnSubs)],_Symbol]



frArg=MapThread[Prepend[#1,#2]&,{{{.3599,2},{0,.35},{.187,.9},{-9.,9.},{-.01,0.1},{-9.,9.},{.9,1.1}},theVars}]




ssFRSolnSubs=Prepend[Chop[FindRoot[forFR,frArg,MaxIterations->1000]],IIss->0];


theProduct=upsilon*II//.ssFRSolnSubs/.betterRBCFixCompSlack`Private`paramSubs;


@}
@o betterRBCFixCompSlack.m
@{

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
}

@}
@o betterRBCFixCompSlack.m
@{


theArgs={cctm1,iitm1,kktm1,lamtm1,mutm1,nltm1,thetatm1,epsVal};

rbcEqnsBetterBackLookingFixCompSlack=
Apply[Function , ({theArgs,rbcBackLookingEqns/.argsSubs}/.paramSubs)]


rbcEqnsBetterBackLookingExpFixCompSlack=
Apply[Function , ({Drop[theArgs,-1],rbcBackLookingExpEqns/.argsSubs}/.paramSubs)]

preRbcEqnsBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha)))-lam[t+1]*delta*(1-dd)+mu1[t]-mu1[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
mu1[t]
}


@}
@o betterRBCFixCompSlack.m
@{

eqnsForBind=(((betterRBCFixCompSlack`Private`preRbcEqnsBinding/.betterRBCFixCompSlack`Private`paramSubs)/.{
eps[betterRBCFixCompSlack`Private`theta][t]->epsVal,
betterRBCFixCompSlack`Private`cc[t-1]->Global`cctm1,
betterRBCFixCompSlack`Private`II[t-1]->iitm1,
betterRBCFixCompSlack`Private`kk[t-1]->kktm1,
betterRBCFixCompSlack`Private`lam[t-1]->lamtm1,
betterRBCFixCompSlack`Private`mu1[t-1]->mu1tm1,
betterRBCFixCompSlack`Private`nlPart[t-1]->nltm1,
betterRBCFixCompSlack`Private`theta[t-1]->thetatm1,
betterRBCFixCompSlack`Private`cc[t]->cct,
betterRBCFixCompSlack`Private`II[t]->iit,
betterRBCFixCompSlack`Private`kk[t]->kkt,
betterRBCFixCompSlack`Private`lam[t]->lamt,
betterRBCFixCompSlack`Private`mu1[t]->mu1t,
betterRBCFixCompSlack`Private`nlPart[t]->nlt,
betterRBCFixCompSlack`Private`theta[t]->thetat,
betterRBCFixCompSlack`Private`cc[t+1]->cctp1,
betterRBCFixCompSlack`Private`II[t+1]->iitp1,
betterRBCFixCompSlack`Private`kk[t+1]->kktp1,
betterRBCFixCompSlack`Private`lam[t+1]->lamtp1,
betterRBCFixCompSlack`Private`mu1[t+1]->mu1tp1,
betterRBCFixCompSlack`Private`nlPart[t+1]->nltp1,
betterRBCFixCompSlack`Private`theta[t+1]->thetat
})//.
betterRBCFixCompSlack`Private`ssFRSolnSubs)//N

@}
@o betterRBCFixCompSlack.m
@{

eqnsForNotBind=(((betterRBCFixCompSlack`Private`rbcEqnsNotBinding/.betterRBCFixCompSlack`Private`paramSubs)
/.{
eps[betterRBCFixCompSlack`Private`theta][t]->epsVal,
betterRBCFixCompSlack`Private`cc[t-1]->cctm1,
betterRBCFixCompSlack`Private`II[t-1]->iitm1,
betterRBCFixCompSlack`Private`kk[t-1]->kktm1,
betterRBCFixCompSlack`Private`lam[t-1]->lamtm1,
betterRBCFixCompSlack`Private`mu1[t-1]->mu1tm1,
betterRBCFixCompSlack`Private`nlPart[t-1]->nltm1,
betterRBCFixCompSlack`Private`theta[t-1]->thetatm1,
betterRBCFixCompSlack`Private`cc[t]->cct,
betterRBCFixCompSlack`Private`II[t]->iit,
betterRBCFixCompSlack`Private`kk[t]->kkt,
betterRBCFixCompSlack`Private`lam[t]->lamt,
betterRBCFixCompSlack`Private`mu1[t]->mu1t,
betterRBCFixCompSlack`Private`nlPart[t]->nlt,
betterRBCFixCompSlack`Private`theta[t]->thetat,
betterRBCFixCompSlack`Private`cc[t+1]->cctp1,
betterRBCFixCompSlack`Private`II[t+1]->iitp1,
betterRBCFixCompSlack`Private`kk[t+1]->kktp1,
betterRBCFixCompSlack`Private`lam[t+1]->lamtp1,
betterRBCFixCompSlack`Private`mu1[t+1]->mu1tp1,
betterRBCFixCompSlack`Private`nlPart[t+1]->nltp1,
betterRBCFixCompSlack`Private`theta[t+1]->thetat
})//.
betterRBCFixCompSlack`Private`ssFRSolnSubs)//N

@}
@o betterRBCFixCompSlack.m
@{

  rbcEqnsBetterFixCompSlack={
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
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[2,1]]>(theProduct)]]]},
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
}

@}
@o betterRBCFixCompSlack.m
@{

theDistBetterFixCompSlack={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterFixCompSlack={{{ee,PerfectForesight}}};




psiz=IdentityMatrix[7]

hmatSymbRawRE=(((equationsToMatrix[
rbcEqnsNotBinding/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssFRSolnSubs)/.{eps[_]->0}//FullSimplify;

psiepsSymbRE=-Transpose[{((Map[D[#,eps[theta][t]]&, rbcEqnsNotBinding])/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssFRSolnSubs}/.simpParamSubs]


hmatSymbRE=hmatSymbRawRE//.simpParamSubs
hSumRE=hmatSymbRE[[All,Range[7]]]+hmatSymbRE[[All,7+Range[7]]]+hmatSymbRE[[All,2*7+Range[7]]];


ssSolnVecRE={{cc},{II},{kk},{lam},{mu1},{nlPart},{theta}}//.ssFRSolnSubs;
psicSymbRE=hSumRE . ssSolnVecRE;


@}
@o betterRBCFixCompSlack.m
@{

{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];



{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];


qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];


{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterFixCompSlack={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{4,rbcEqnsBetterBackLookingFixCompSlack,rbcEqnsBetterBackLookingExpFixCompSlack}}}




anXBetterFixCompSlack=Transpose[{{99,99,.18,99,99,99,1.01}}];
anEpsBetterFixCompSlack={{0.01}};
anXEpsBetterFixCompSlack=Join[anXBetterFixCompSlack,anEpsBetterFixCompSlack]
aZBetterFixCompSlack=Transpose[{{.1,.2,.3,.4,.5,.6,.7}}]
anXEpsZsBetterFixCompSlack=Join[anXEpsBetterFixCompSlack,aZBetterFixCompSlack];

anXFlatBetterFixCompSlack=anXBetterFixCompSlack//Flatten;
anEpsFlatBetterFixCompSlack=anEpsBetterFixCompSlack//Flatten;
anXEpsFlatBetterFixCompSlack=anXEpsBetterFixCompSlack//Flatten;
aZFlatBetterFixCompSlack=aZBetterFixCompSlack//Flatten;
anXEpsZsFlatBetterFixCompSlack=anXEpsZsBetterFixCompSlack//Flatten;

probDimsBetterFixCompSlack={7,1,7};


@}
@o betterRBCFixCompSlack.m
@{


thVal=(theta//.ssFRSolnSubs//.(simpParamSubs//N))//N;
kVal = (kk //.ssFRSolnSubs//.(simpParamSubs//N))//N;
cVal = (cc //.ssFRSolnSubs//.(simpParamSubs//N))//N ;
(*following guerrieri and iacoviello Appendix A*)
kLow = kVal*.95//N;
kHigh = kVal*1.4//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;


aGSpecBetterFixCompSlack={{1,2,4,5,6},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};




simulateBetterRBCCS[numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterFixCompSlack[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}],
fMul=Inverse[IdentityMatrix[7]-fmatSymbRE]},
With[{mats=FoldList[(bmatSymbRE . #1+ (phimatSymbRE .psiepsSymbRE .{{#2}})+
fMul.phimatSymbRE.psicSymbRE)&,initVec,draws]},
Map[Flatten,mats]]]

simulateBetterRBCCS[anAugDR_Function,numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterFixCompSlack[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}]},
With[{mats=FoldList[((Apply[anAugDR , Flatten[{#1[[Range[7]]],#2}]]))&,initVec,draws]},Map[Flatten,mats]]]


@}




@o betterRBCFixCompSlack.m
@{


iterateRBCCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]:=
With[{mats=
NestList[((Apply[anAugDR , Flatten[{#1[[Range[7]]],#2}]]))&,initVec,draws]},
Map[Flatten,mats]]

chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]:=
Module[{},
If[
Catch[
Nest[With[{val=Apply[anAugDRCE,Flatten[#]][[Range[7]]]},
If[Norm[val]>lim,Throw[False,"chkBounded"],val]]&,aPt,numPers],
"chkBounded"]===False,False,True]]

Print["about to simulate fixed seed"];
SeedRandom[1234]
theRes=simulateBetterRBCCS[200];
Print["done simulate"];
justKT=theRes[[All,{3,7}]];
betterRBCCSMean=Mean[justKT];
betterRBCCSSD=StandardDeviation[justKT];

@}
@o betterRBCFixCompSlack.m
@{


normedRes=Map[(#/betterRBCCSSD)&,(Map[(#-betterRBCCSMean)&,justKT])];
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
betterRBCCSMinZ=Map[Min,Transpose[zz]];
betterRBCCSMaxZ=Map[Max,Transpose[zz]];
{ig,ig,theKs,ig,ig,ig,theThetas}=Transpose[theRes];

betterRBCCSMean=Append[betterRBCCSMean,0];
betterRBCCSSD=Append[betterRBCCSSD,sigVal];
betterRBCCSMinZ=Append[betterRBCCSMinZ,-3];
betterRBCCSMaxZ=Append[betterRBCCSMaxZ,3];
betterRBCCSvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0},{0}}}}]},{{{0,0,1}}}}];


End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBCFixCompSlack.m"]



@}

\subsection{betterRBCRegimes.m}
\label{sec:betterrbc.m}

@o betterRBCRegimes.m
@{

(* Wolfram Language Package *)

BeginPackage["betterRBCRegimes`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
rbcEqnsBetterCSTrips::usage="rbcEqnsBetterCSTrips"

anXBetterRegimes::usage="for test input";
anEpsBetterRegimes::usage="for test input";
anXEpsBetterRegimes::usage="for test input";
aZBetterRegimes::usage="for test input";
anXEpsZsBetterRegimes::usage="for test input";

anXFlatBetterRegimes::usage="for test input";
anEpsFlatBetterRegimes::usage="for test input";
anXEpsFlatBetterRegimes::usage="for test input";
aZFlatBetterRegimes::usage="for test input";
anXEpsZsFlatBetterRegimes::usage="for test input";

probDimsBetterRegimes::usage="for test input";
theDistBetterRegimes::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterRegimes::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterRegimes::usage="linear model matrices for approx"
aGSpecBetterRegimes::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsBetterBackLookingRegimes::usage="rbcEqnsBetterBackLookingRegimes"
rbcEqnsBetterBackLookingExpRegimes::usage="rbcEqnsBetterBackLookingRegimes"
rbcEqnsBetterRegimes::usage="model equations"
rbcEqnsBetterRegimes::usage="modEqnsRegimes"
eqnsEulerCompiledBetterRegimes::usage="eqnsEulerCompiledBetterRegimes"
@}
@o betterRBCRegimes.m
@{



simulateBetterRBCCS::usage="simulateBetterRBCExact[numPers_Integer]"
betterRBCCSMean::usage="betterRBCCSMean"
betterRBCCSSD::usage="betterRBCCSSD"
betterRBCCSvv::usage="betterRBCCSvv"
betterRBCCSMinZ::usage="betterRBCCSMinZ"
betterRBCCSMaxZ::usage="betterRBCCSMaxZ"

chkBounded::usage="chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]"


iterateRBCCSDRCE::usage="iterateRBCCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]"


@}
@o betterRBCRegimes.m
@{


Begin["`Private`"] (* Begin Private Context *) 











(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)



(*parameters page 28 guerrieri iacoviello*)

paramSubs01={
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01,
dd->.1,
upsilon->0.975
} ;

paramSubs02={
alpha->.26,
beta->1,
eta->1,
delta->.85,
rho->.45,
sigma->.01,
dd->.1,
upsilon->0.275
} ;



forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs01;
simpParamSubs=Join[paramSubs01,forParamSubs,simpSubs];


@}
@o betterRBCRegimes.m
@{

rbcEqnsNotBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha))) -lam[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1])+mu1[t]-mu1[t+1]*delta*(1-dd),
mu1[t]
}


rbcBackLookingEqns={E^(rho*Log[theta[t-1]] + eps[theta][t])}
rbcBackLookingExpEqns={Expectation[rbcBackLookingEqns[[1]],eps[theta][t] \[Distributed] NormalDistribution[0,sigma]]}







ssEqnSubs=
{xx_Symbol[t+v_.]->xx}
rbcEqnsNotBindingSubbed=((rbcEqnsNotBinding/.paramSubs01)/.eps[theta][t]->0)


theVars=Cases[Variables[forFR=(rbcEqnsNotBindingSubbed/.ssEqnSubs)],_Symbol]



frArg=MapThread[Prepend[#1,#2]&,{{{.3599,2},{0,.35},{.187,.9},{-9.,9.},{-.01,0.1},{-9.,9.},{.9,1.1}},theVars}]




ssFRSolnSubs=Prepend[Chop[FindRoot[forFR,frArg,MaxIterations->1000]],IIss->0];


theProduct=upsilon*II//.ssFRSolnSubs/.betterRBCRegimes`Private`paramSubs01;


@}
@o betterRBCRegimes.m
@{

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
}

@}
@o betterRBCRegimes.m
@{


theArgs={cctm1,iitm1,kktm1,lamtm1,mutm1,nltm1,thetatm1,epsVal};

rbcEqnsBetterBackLookingRegimes=
Apply[Function , ({theArgs,rbcBackLookingEqns/.argsSubs}/.paramSubs01)]


rbcEqnsBetterBackLookingExpRegimes=
Apply[Function , ({Drop[theArgs,-1],rbcBackLookingExpEqns/.argsSubs}/.paramSubs01)]

preRbcEqnsBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha)))-lam[t+1]*delta*(1-dd)+mu1[t]-mu1[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
mu1[t]
}


@}
@o betterRBCRegimes.m
@{

eqnsForBind01=(((betterRBCRegimes`Private`preRbcEqnsBinding/.betterRBCRegimes`Private`paramSubs01)/.{
eps[betterRBCRegimes`Private`theta][t]->epsVal,
betterRBCRegimes`Private`cc[t-1]->Global`cctm1,
betterRBCRegimes`Private`II[t-1]->iitm1,
betterRBCRegimes`Private`kk[t-1]->kktm1,
betterRBCRegimes`Private`lam[t-1]->lamtm1,
betterRBCRegimes`Private`mu1[t-1]->mu1tm1,
betterRBCRegimes`Private`nlPart[t-1]->nltm1,
betterRBCRegimes`Private`theta[t-1]->thetatm1,
betterRBCRegimes`Private`cc[t]->cct,
betterRBCRegimes`Private`II[t]->iit,
betterRBCRegimes`Private`kk[t]->kkt,
betterRBCRegimes`Private`lam[t]->lamt,
betterRBCRegimes`Private`mu1[t]->mu1t,
betterRBCRegimes`Private`nlPart[t]->nlt,
betterRBCRegimes`Private`theta[t]->thetat,
betterRBCRegimes`Private`cc[t+1]->cctp1,
betterRBCRegimes`Private`II[t+1]->iitp1,
betterRBCRegimes`Private`kk[t+1]->kktp1,
betterRBCRegimes`Private`lam[t+1]->lamtp1,
betterRBCRegimes`Private`mu1[t+1]->mu1tp1,
betterRBCRegimes`Private`nlPart[t+1]->nltp1,
betterRBCRegimes`Private`theta[t+1]->thetat
})//.
betterRBCRegimes`Private`ssFRSolnSubs)//N

@}
@o betterRBCRegimes.m
@{

eqnsForNotBind01=(((betterRBCRegimes`Private`rbcEqnsNotBinding/.betterRBCRegimes`Private`paramSubs01)
/.{
eps[betterRBCRegimes`Private`theta][t]->epsVal,
betterRBCRegimes`Private`cc[t-1]->cctm1,
betterRBCRegimes`Private`II[t-1]->iitm1,
betterRBCRegimes`Private`kk[t-1]->kktm1,
betterRBCRegimes`Private`lam[t-1]->lamtm1,
betterRBCRegimes`Private`mu1[t-1]->mu1tm1,
betterRBCRegimes`Private`nlPart[t-1]->nltm1,
betterRBCRegimes`Private`theta[t-1]->thetatm1,
betterRBCRegimes`Private`cc[t]->cct,
betterRBCRegimes`Private`II[t]->iit,
betterRBCRegimes`Private`kk[t]->kkt,
betterRBCRegimes`Private`lam[t]->lamt,
betterRBCRegimes`Private`mu1[t]->mu1t,
betterRBCRegimes`Private`nlPart[t]->nlt,
betterRBCRegimes`Private`theta[t]->thetat,
betterRBCRegimes`Private`cc[t+1]->cctp1,
betterRBCRegimes`Private`II[t+1]->iitp1,
betterRBCRegimes`Private`kk[t+1]->kktp1,
betterRBCRegimes`Private`lam[t+1]->lamtp1,
betterRBCRegimes`Private`mu1[t+1]->mu1tp1,
betterRBCRegimes`Private`nlPart[t+1]->nltp1,
betterRBCRegimes`Private`theta[t+1]->thetat
})//.
betterRBCRegimes`Private`ssFRSolnSubs)//N

@}
@o betterRBCRegimes.m
@{

eqnsForBind02=(((betterRBCRegimes`Private`preRbcEqnsBinding/.betterRBCRegimes`Private`paramSubs02)/.{
eps[betterRBCRegimes`Private`theta][t]->epsVal,
betterRBCRegimes`Private`cc[t-1]->Global`cctm1,
betterRBCRegimes`Private`II[t-1]->iitm1,
betterRBCRegimes`Private`kk[t-1]->kktm1,
betterRBCRegimes`Private`lam[t-1]->lamtm1,
betterRBCRegimes`Private`mu1[t-1]->mu1tm1,
betterRBCRegimes`Private`nlPart[t-1]->nltm1,
betterRBCRegimes`Private`theta[t-1]->thetatm1,
betterRBCRegimes`Private`cc[t]->cct,
betterRBCRegimes`Private`II[t]->iit,
betterRBCRegimes`Private`kk[t]->kkt,
betterRBCRegimes`Private`lam[t]->lamt,
betterRBCRegimes`Private`mu1[t]->mu1t,
betterRBCRegimes`Private`nlPart[t]->nlt,
betterRBCRegimes`Private`theta[t]->thetat,
betterRBCRegimes`Private`cc[t+1]->cctp1,
betterRBCRegimes`Private`II[t+1]->iitp1,
betterRBCRegimes`Private`kk[t+1]->kktp1,
betterRBCRegimes`Private`lam[t+1]->lamtp1,
betterRBCRegimes`Private`mu1[t+1]->mu1tp1,
betterRBCRegimes`Private`nlPart[t+1]->nltp1,
betterRBCRegimes`Private`theta[t+1]->thetat
})//.
betterRBCRegimes`Private`ssFRSolnSubs)//N

@}
@o betterRBCRegimes.m
@{

eqnsForNotBind02=(((betterRBCRegimes`Private`rbcEqnsNotBinding/.betterRBCRegimes`Private`paramSubs02)
/.{
eps[betterRBCRegimes`Private`theta][t]->epsVal,
betterRBCRegimes`Private`cc[t-1]->cctm1,
betterRBCRegimes`Private`II[t-1]->iitm1,
betterRBCRegimes`Private`kk[t-1]->kktm1,
betterRBCRegimes`Private`lam[t-1]->lamtm1,
betterRBCRegimes`Private`mu1[t-1]->mu1tm1,
betterRBCRegimes`Private`nlPart[t-1]->nltm1,
betterRBCRegimes`Private`theta[t-1]->thetatm1,
betterRBCRegimes`Private`cc[t]->cct,
betterRBCRegimes`Private`II[t]->iit,
betterRBCRegimes`Private`kk[t]->kkt,
betterRBCRegimes`Private`lam[t]->lamt,
betterRBCRegimes`Private`mu1[t]->mu1t,
betterRBCRegimes`Private`nlPart[t]->nlt,
betterRBCRegimes`Private`theta[t]->thetat,
betterRBCRegimes`Private`cc[t+1]->cctp1,
betterRBCRegimes`Private`II[t+1]->iitp1,
betterRBCRegimes`Private`kk[t+1]->kktp1,
betterRBCRegimes`Private`lam[t+1]->lamtp1,
betterRBCRegimes`Private`mu1[t+1]->mu1tp1,
betterRBCRegimes`Private`nlPart[t+1]->nltp1,
betterRBCRegimes`Private`theta[t+1]->thetat
})//.
betterRBCRegimes`Private`ssFRSolnSubs)//N

@}
@o betterRBCRegimes.m
@{

  rbcEqnsBetterRegime01={
 { 
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind01),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],
Function[{aPt,aRes},
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[2,1]]>(theProduct)]]]},
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForBind01),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],(True)&}},
Function[{aPt,allRes},
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Flatten[allRes[[2]]],Flatten[allRes[[1]]]]]
}

  rbcEqnsBetterRegime02={
 { 
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind02),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],
Function[{aPt,aRes},
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[2,1]]>(theProduct)]]]},
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForBind02),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],(True)&}},
Function[{aPt,allRes},
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Flatten[allRes[[2]]],Flatten[allRes[[1]]]]]
}
(*https://en.wikipedia.org/wiki/Sigmoid_function*)
sigmoidPair[xx_,bb_]:=  With[{pp=E^(bb*xx)/(E^(bb*xx)+1)},{pp,1-pp}]
(*
probFunc[cct_?NumberQ,iit_?NumberQ,kkt_?NumberQ,lamt_?NumberQ,
mu1t_?NumberQ,nlt_?NumberQ,thetat_?NumberQ]:=
With[{pk=sigmoidPair[kkt,1.],pc=sigmoidPair[cct,2.]},
{pk,pc}]
*)
probFunc[cct_?NumberQ,iit_?NumberQ,kkt_?NumberQ,lamt_?NumberQ,
mu1t_?NumberQ,nlt_?NumberQ,thetat_?NumberQ]:=
{{0.9,0.1},{0.2,0.8}}



rbcEqnsBetterRegimes={{rbcEqnsBetterRegime01,
 rbcEqnsBetterRegime02},probFunc}




@}
@o betterRBCRegimes.m
@{

theDistBetterRegimes={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs01;
thePFDistBetterRegimes={{{ee,PerfectForesight}}};




psiz=IdentityMatrix[7]

hmatSymbRawRE=(((equationsToMatrix[
rbcEqnsNotBinding/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssFRSolnSubs)/.{eps[_]->0}//FullSimplify;

psiepsSymbRE=-Transpose[{((Map[D[#,eps[theta][t]]&, rbcEqnsNotBinding])/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssFRSolnSubs}/.simpParamSubs]


hmatSymbRE=hmatSymbRawRE//.simpParamSubs
hSumRE=hmatSymbRE[[All,Range[7]]]+hmatSymbRE[[All,7+Range[7]]]+hmatSymbRE[[All,2*7+Range[7]]];


ssSolnVecRE={{cc},{II},{kk},{lam},{mu1},{nlPart},{theta}}//.ssFRSolnSubs;
psicSymbRE=hSumRE . ssSolnVecRE;


@}
@o betterRBCRegimes.m
@{

{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];



{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];


qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];


{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterRegimes={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{4,rbcEqnsBetterBackLookingRegimes,rbcEqnsBetterBackLookingExpRegimes}}}




anXBetterRegimes=Transpose[{{99,99,.18,99,99,99,1.01}}];
anEpsBetterRegimes={{0.01}};
anXEpsBetterRegimes=Join[anXBetterRegimes,anEpsBetterRegimes]
aZBetterRegimes=Transpose[{{.1,.2,.3,.4,.5,.6,.7}}]
anXEpsZsBetterRegimes=Join[anXEpsBetterRegimes,aZBetterRegimes];

anXFlatBetterRegimes=anXBetterRegimes//Flatten;
anEpsFlatBetterRegimes=anEpsBetterRegimes//Flatten;
anXEpsFlatBetterRegimes=anXEpsBetterRegimes//Flatten;
aZFlatBetterRegimes=aZBetterRegimes//Flatten;
anXEpsZsFlatBetterRegimes=anXEpsZsBetterRegimes//Flatten;

probDimsBetterRegimes={7,1,7};


@}
@o betterRBCRegimes.m
@{


thVal=(theta//.ssFRSolnSubs//.(simpParamSubs//N))//N;
kVal = (kk //.ssFRSolnSubs//.(simpParamSubs//N))//N;
cVal = (cc //.ssFRSolnSubs//.(simpParamSubs//N))//N ;
(*following guerrieri and iacoviello Appendix A*)
kLow = kVal*.95//N;
kHigh = kVal*1.4//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;


aGSpecBetterRegimes={{1,2,4,5,6},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};




simulateBetterRBCCS[numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterRegimes[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}],
fMul=Inverse[IdentityMatrix[7]-fmatSymbRE]},
With[{mats=FoldList[(bmatSymbRE . #1+ (phimatSymbRE .psiepsSymbRE .{{#2}})+
fMul.phimatSymbRE.psicSymbRE)&,initVec,draws]},
Map[Flatten,mats]]]

simulateBetterRBCCS[anAugDR_Function,numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterRegimes[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}]},
With[{mats=FoldList[((Apply[anAugDR , Flatten[{#1[[Range[7]]],#2}]]))&,initVec,draws]},Map[Flatten,mats]]]


@}




@o betterRBCRegimes.m
@{


iterateRBCCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]:=
With[{mats=
NestList[((Apply[anAugDR , Flatten[{#1[[Range[7]]],#2}]]))&,initVec,draws]},
Map[Flatten,mats]]

chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]:=
Module[{},
If[
Catch[
Nest[With[{val=Apply[anAugDRCE,Flatten[#]][[Range[7]]]},
If[Norm[val]>lim,Throw[False,"chkBounded"],val]]&,aPt,numPers],
"chkBounded"]===False,False,True]]

Print["about to simulate fixed seed"];
SeedRandom[1234]
theRes=simulateBetterRBCCS[200];
Print["done simulate"];
justKT=theRes[[All,{3,7}]];
betterRBCCSMean=Mean[justKT];
betterRBCCSSD=StandardDeviation[justKT];

@}
@o betterRBCRegimes.m
@{


normedRes=Map[(#/betterRBCCSSD)&,(Map[(#-betterRBCCSMean)&,justKT])];
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
betterRBCCSMinZ=Map[Min,Transpose[zz]];
betterRBCCSMaxZ=Map[Max,Transpose[zz]];
{ig,ig,theKs,ig,ig,ig,theThetas}=Transpose[theRes];

betterRBCCSMean=Append[betterRBCCSMean,0];
betterRBCCSSD=Append[betterRBCCSSD,sigVal];
betterRBCCSMinZ=Append[betterRBCCSMinZ,-3];
betterRBCCSMaxZ=Append[betterRBCCSMaxZ,3];
betterRBCCSvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0},{0}}}}]},{{{0,0,1}}}}];


End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBCRegimes.m"]



@}

\subsection{Identifiers}
\label{sec:identifiers}

@u
\subsection{Macros}
\label{sec:macros}

@m

\bibliographystyle{plainnat}
\bibliography{files}

\end{document}
