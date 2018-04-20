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



\subsection{Getters and Setters}
\label{sec:getters-setters}


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
