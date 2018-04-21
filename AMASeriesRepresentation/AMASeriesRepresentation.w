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
@{genX0Z0Funcs::usage="genX0Z0Funcs[@<linMod@>]:use linear reference model to generate augmented decision rule functions"
@}

@d genX0Z0Funcs
@{
(*begin code for genX0Z0Funcs*)
genX0Z0Funcs[@<linMod@>]:=
With[{numXVars=getNumX[linMod],numZVars=getNumZ[linMod]},
With[{xtm1Vars=genSlots[numXVars]},
With[{fromLinMod=Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]},
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
@{ genLilXkZkFunc[@<linMod@>,{}]@}

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
With[{fPows=Drop[NestList[Function[xx,FF. xx],IdentityMatrix[numXVars],Length[zPath]],-1]},
Apply[Plus,
MapThread[Function[{xx,yy},Dot[xx,phi.psiZ.yy]],{fPows , zPath}]]]]]

@}


\subsubsection{Apply Series Formula}
\label{sec:apply-series-formula}

@d theZs
@{theZs:{_?MatrixQ..}@}
@d genLilXkZkFunc theZs call
@{genLilXkZkFunc[@<linMod@>,@<theZs@>]@}

@d genLilXkZkFunc
@{
@< genLilXkZkFunc theZs call@>:=
Module[{},
@<Z Matrices Given@>
]
@}

@d Z Matrices Given
@{With[{fCon=Check[fSumC[phi,FF,psiZ,theZs],Print["trying to throw low"];
Throw[$Failed,"low"]]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]



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
@{genLilXkZkFunc[@<linMod@>,@<fCon@>]@}

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
With[{xtp1Vals=BB.xtVals+Inverse[IdentityMatrix[Length[xtVals]]-FF] . phi . psiC+fCon},xtp1Vals]


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

@d genFRExtFunc
@{

genZsForFindRoot[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,
backLookingInfo:{{_Integer,backLooking_,backLookingExp_}...}},
	initVec_?MatrixQ,theCondExp:(_Function|_CompiledFunction),iters_Integer]:=
Module[{},
With[{numX=Length[initVec],
 	thePath=Flatten[
Check[iterateDRCE[theCondExp,initVec,iters+1],
Print["problems with current DRCE,using at",initVec,"linMod!!!!!"];
iterateDRCE[genX0Z0Funcs[linMod],initVec,iters+1]]]},
 		With[{restVals=
      Map[(theHMat .thePath[[Range[3*numX]+numX*(#-1)]] -psiC)&,Range[(Length[thePath]/numX)-3]]},
      restVals
]]]





@}

\subsubsection{Using just decision rule  expectation}
\label{sec:using-both-decision}


@d genFRExtFunc
@{

(*begin code for genFRExtFunc*)
Options[genFRExtFunc]={"xVarRanges"->{},"Traditional"->False} 

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
@{XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),numSteps_Integer}@}



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
xkAppl=Flatten[Join[xLagArgs,xArgs,(Apply[XZFuncs[[1]],xArgs][[Range[numX]]]),eArgs]]},
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
With[{xkFunc=Catch[(Check[genLilXkZkFunc[linMod,theZsNow],Print["trying higher throw"];Throw[$Failed,"higher"]]),_,Function[{val,tag},Print["catchfxtzt:",{xArgs,val,tag}//InputForm];Throw[$Failed,"fromGenLil"]]]},
With[{xkAppl=Apply[xkFunc,Join[xLagArgs,eArgs,zArgs]]},
With[{eqnAppl=Apply[eqnsFunc,Flatten[xkAppl]],
xDisc=xArgs-xkAppl[[numX+Range[numX]]]},
Flatten[Join[xDisc,eqnAppl]]]]]]]@}

\subsubsection{Using both decision rule and decision rule expectation}
\label{sec:using-both-decision}


@d genFRExtFunc
@{

genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},@<linMod@>,@<bothXZFuncs@>,
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
@{bothXZFuncs:{justBothXZFuncs:{xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},numSteps_Integer}@}



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
xkAppl=Flatten[Join[xLagArgs,xArgs,(Apply[bothXZFuncs[[1,2]],xArgs][[Range[numX]]]),eArgs]]},
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
With[{xkFunc=Catch[(Check[genLilXkZkFunc[linMod,theZsNow],Print["trying higher throw"];Throw[$Failed,"higher"]]),_,Function[{val,tag},Print["catchfxtzt:",{xArgs,val,tag}//InputForm];Throw[$Failed,"fromGenLil"]]]},
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
fillIn[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]


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
backXtoZ::usage="backXtoZ[theXs_?MatrixQ,theMeans_?VectorQ,theSDs_?VectorQ,theV_?MatrixQ]"
backZtoX::usage="backZtoX[theXs_?MatrixQ,theMeans_?VectorQ,theSDs_?VectorQ,theV_?MatrixQ]"
@}

@d smolyakInterpolationPrep
@{
Options[smolyakInterpolationPrep]={"Derivatives"->False,"ptGenerator"->chebyshevPtGenerator}
smolyakInterpolationPrep[approxLevels_?listOfIntegersQ,smolRngs_?MatrixQ,
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
{xPts,smolMat,ExpandAll[smolPolys],intPolys,dintPolys}]]]]]]/;And[Length[smolRngs]==Length[approxLevels]]

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


makeGenericInterpFuncs[aVecFunc:(_Function|_CompiledFunction|_Symbol),backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
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
	If[MemberQ[toIgnore,Length[theRes]+1],fillInSymb[{Append[theRes,Unique["ig"]],Drop[toIgnore,1],shortVec}],
		fillInSymb[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]

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
With[{preIntTaylor={preInt[[1]],multivariateTaylor[preInt[[2]],preInt[[1]],taylorOrd]},
postIntTaylor={postInt[[1]],multivariateTaylor[postInt[[2]],postInt[[1]],taylorOrd]}},
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

Options[doGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False}
doGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,eqnsFunc,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
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

Options[nestGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False}
nestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
NestList[Function[xx,doGenericIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,genericInterp,svmArgs]],{99,XZFuncs[[1]]},numIters]


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
Check[Function[xx,fillIn[{{},smolToIgnore,xx}]],Print["aborting"];AbortKernels[];Throw["parallelSmolyakGenInterpData"]],N[smolPts]]},
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

parallelMakeGenericInterpFuncs[aVecFunc:(_Function|_CompiledFunction|_Symbol),backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
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



Options[parallelDoGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False}
parallelDoGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,eqnsFunc,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
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





Options[parallelNestGenericIterREInterp]={"xVarRanges"->{},"Traditional"->False}

parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoGenericIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,genericInterp,svmArgs,Apply[Sequence,FilterRules[{opts},Options[parallelDoGenericIterREInterp]]]]],{99,XZFuncs[[1]]},numIters]]

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



parallelMakeInterpFunc[aVecFunc:(_Function|_CompiledFunction|_Symbol),backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>]:=
Module[{},
parallelMakeGenericInterpFuncs[aVecFunc,backLookingInfo,smolGSpec,smolyakInterpolation,{}]]





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
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,eqnsFunc,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,smolyakInterpolation,{}]},
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
{xx[[2]],numSteps},eqnsFunc,smolGSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],{ig,XZFuncs[[1]]},numIters]]




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
With[{numXVars=getNumX[linMod],numEpsVars=getNumEps[linMod],numZVars=getNumZ[linMod]},
With[{xtm1Vars=genSlots[numXVars],
epsVars=Drop[genSlots[numXVars+numEpsVars],numXVars]},
With[{fromLinMod=Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]+Join[psiEps.epsVars,ConstantArray[0,{numZVars,1}]],
fromLinModCE=Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]},
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
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function},
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
genFRExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},@<linMod@>,@<bothXZFuncs@>,
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function},
opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{funcTrips=
Map[{#[[1]],genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,#[[2]],
Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],#[[3]]}&,triples[[1]]]},
{funcTrips,selectorFunc}
]]
@}
@d evaluateTripleUsage
@{
evaluateTriple::usage=
"place holder for genFRExtFunc"
@}
@d evaluateTriple
@{

evaluateTriple[
triple:{preFunc_Function,theFunc:(_Function|_CompiledFunction|_Symbol),
postFunc_Function},
thePt_?VectorQ]:=
Catch[
If[
Apply[preFunc,thePt],
With[{theRes=
Apply[theFunc,thePt]},
If[Apply[postFunc,{thePt,theRes}],theRes,$Failed]],$Failed],_,Function[{val,tag},Print["catchinevaluateTriple:",{xArgs,val,tag}//InputForm];$Failed]]

@}



\subsection{parallelSmolyakGenInterpData}
\label{sec:parall}

@d parallelSmolyakGenInterpData
@{
 

parallelSmolyakGenInterpData[
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},@<smolGSpec@>]:=
Module[{},
With[{filledPts=Map[Function[xx,fillIn[{{},smolToIgnore,xx}]],N[smolPts]]},
With[{theVals=
ParallelTable[evaluateTriple[aTriple,Flatten[aPt]],
{aPt,filledPts},{aTriple,triples[[1]]}]},
With[{interpData=
ParallelMap[With[{baddy=#},Catch[
Apply[selectorFunc,#],
_,Function[{val,tag},Print["catchsmolGenInterp: aborting",
{val,tag,baddy,triples,filledPts}//InputForm];
Abort[]]]]&,{filledPts,theVals}//Transpose]},
interpData]]]]






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
@{gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}@}



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
@<genBothX0Z0FuncsUsage@>
@<evaluateTripleUsage@>
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
@<getH@>
@<genLilXkZkFunc@>
@<fSumC@>
@<genXtOfXtm1@>
@<genXtp1OfXt@>
@<genFRExtFunc@>
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
@<genBothX0Z0Funcs@>
@<evaluateTriple@>
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


\subsection{Identifiers}
\label{sec:identifiers}

@u
\subsection{Macros}
\label{sec:macros}

@m

\bibliographystyle{plainnat}
\bibliography{files}

\end{document}
