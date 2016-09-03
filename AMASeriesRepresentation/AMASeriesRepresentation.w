\documentclass[12pt]{article}

\usepackage{hyperref}
\usepackage{datetime}
\title{Mathematica Code for AMASeriesRepresentation Package}
\author{Gary S Anderson}

\begin{document}
\maketitle


\section{Introduction and Summary}
\label{sec:introduction-summary}

\appendix
\section{Function Definitions}
\label{sec:function-definitions}



@o AMASeriesRepresentation.m
@{
BeginPackage["AMASeriesRepresentation`",
 {"JLink`","ProtectedSymbols`"}]
@<usage definitions@>
Begin["`Private`"]
@<package code@>
End[]
EndPackage[]


@}




@d usage definitions
@{
(*Begin Usage Definitions*)
PerfectForesight::usage="degenerate distribution implementing perfect foresight"
@<genLilXkZkFuncUsage@>
@<gettersSettersUsage@>
@<worstPathForErrDRREIntegrateUsage@>
@<evalBadPathErrDRREIntegrateUsage@>
@<evalPathErrDRREIntegrateUsage@>
@<doFuncArgUsage@>
@<pathErrsDRPFUsage@>
@<pathErrsDRREIntegrateUsage@>
@<iterateDRPFUsage@>
@<genNSFuncUsage@>
@<makeREIterFuncUsage@>
@<myNExpectationUsage@>
@<getDistribsUsage@>
@<genXZFuncREUsage@>
@<genIntVarsUsage@>
@<genXZREInterpFuncUsage@>
@<genX0Z0FuncsUsage@>
@<checkModUsage@>
@<genFRFuncUsage@>
@<genFPFuncUsage@>
@<myFixedPointUsage@>
@<getHUsage@>
@<getBUsage@>
@<getFUsage@>
@<getGridPtTripsUsage@>
@<getNumVarsUsage@>
@<parallelMakeInterpFuncUsage@>
@<makeInterpFuncUsage@>
@<nestIterREInterpUsage@>
@<parallelGenInterpDataUsage@>
@<genInterpDataUsage@>
@<oneDimGridPtsUsage@>
@<gridPtsUsage@>
@<fillInUsage@>
@<fillInSymbUsage@>
@<doIterREInterpUsage@> 
@<getPhiUsage@>
@<getPsiZUsage@>
@<getPsiCUsage@>
@<getPsiEpsUsage@>
@<getNumZUsage@>
@<getNumXUsage@>
@<getNumEpsUsage@>
@<multiStepUsage@>
@<multiStepZUsage@>
@<multiStepXUsage@>
@<checkLinModUsage@>
@<fSumCUsage@>
@<fSumUsage@>
@<getNumEpsVarsUsage@>
@<iterateDRREIntegrateUsage@>
@<genPathUsage@>
@<getNumIgnoredUsage@>
@<getNumInterpVarsUsage@>
@}

@d package code
@{

@<gettersSetters@>
@<getNumIgnored@>
@<getNumInterpVars@>
@<worstPathForErrDRREIntegrate@>
@<evalBadPathErrDRREIntegrate@>
@<evalPathErrDRREIntegrate@>
@<doFuncArg@>
@<genPath@>
@<pathErrsDRPF@>
@<pathErrsDRREIntegrate@>
@<iterateDRPF@>
@<genNSFunc@>
@<iterateDRREIntegrate@>
@<makeREIterFunc@>
@<getNumEpsVars@>
@<myNExpectation@>
@<getDistribs@>
@<getNumVars@>
@<getGridPtTrips@>
@<getH@>
@<getB@>
@<getF@>
@<getPhi@>
@<getPsiZ@>
@<getPsiC@>
@<getPsiEps@>
@<getNumZ@>
@<getNumX@>
@<getNumEps@>
@<genLilXkZkFunc@>
@<fSumC@>
@<fSum@>
@<genSlots@>
@<genXtOfXtm1@>
@<genXtp1OfXt@>
@<genX0Z0Funcs@>
@<multiStep@>
@<multiStepZ@>
@<multiStepX@>
@<checkLinMod@>
@<checkMod@>
@<genFRFunc@>
@<genFPFunc@>
@<myFixedPoint@>
@<parallelMakeInterpFunc@>
@<makeInterpFunc@>
@<parallelGenInterpData@>
@<genInterpData@>
@<gridPts@>
@<oneDimGridPts@>
@<fillIn@>
@<fillInSymb@>
@<doIterREInterp@>
@<nestIterREInterp@>
@<genXZREInterpFunc@>
@<genXZFuncRE@>
@<genIntVars@>
@}
\subsection{Argument Specifications}
\label{sec:argum-spec}


@d linMod
@{linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} @|
linMod
BB
phi
FF
psiZ
psiEps
theHMat
psiC
psiZPreComp 
@}

@d XZFuncs
@{XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction),numSteps_Integer}@}

@d theZs
@{theZs:{_?MatrixQ..}@}


@d drvPairs
@{drvPairs:({{{aa_Integer,bb_Integer}...},
eqnFunc:(_Function|_CompiledFunction)}|{{},{}}):{{},{}}@}

@d xtGuess
@{xtGuess_?MatrixQ@}

@d fCon
@{fCon_?MatrixQ@}

@d gSpec
@{gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}@}

@d distribSpec
@{distribSpec:{expctSpec:{{_Symbol,_}..}}@}

@d eqnsFunc
@{eqnsFunc:(_Function|_CompiledFunction)@}

@d theSolver
@{theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]}))@}


\subsection{genLilXkZkFunc}
\label{sec:genlilxkzkfunc}


@d genLilXkZkFuncUsage
@{
genLilXkZkFunc::usage=
"@<genLilXkZkFunc full call@>"<>
"\ngenerate a function that computes x and z given a guess for xt\n"<>
"@<genLilXkZkFunc fcon call@>"<>
"\ngenerate a function that computes x z based on an assumed F sum\n"<>
"@<genLilXkZkFunc theZs call@>"<>
"\ngenerate a function that computes x and z given sequence of Zs\n"<>
"@<genLilXkZkFunc noZs call@>"<>
"\ngenerate a function that computes x for Zs = 0\n"
@}





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


@d genLilXkZkFunc theZs call
@{genLilXkZkFunc[@<linMod@>,@<theZs@>]@}

@d genLilXkZkFunc
@{
@< genLilXkZkFunc theZs call@>:=
@<Z Matrices Given@>
@}

@d Z Matrices Given
@{With[{fCon=fSumC[phi,FF,psiZ,theZs]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]
@}


@d genLilXkZkFunc full call
@{genLilXkZkFunc[@<linMod@>,@<XZFuncs@>,@<xtGuess@>]@}

@d genLilXkZkFunc
@{
@<genLilXkZkFunc full call@>:=
@<XZ Functions Given@>
@}

@d XZ Functions Given
@{With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]
@}



@d genLilXkZkFuncUsage
@{ genLilXkZkFunc::usage="place holder"@}

@d genLilXkZkFunc fcon call
@{genLilXkZkFunc[@<linMod@>,@<fCon@>]@}


@d genLilXkZkFunc
@{
@<genLilXkZkFunc fcon call@>:=
@<apply formula F...@>
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
Function[fullVec]]
]]]]]
@}

\subsection{fSumC}
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

\subsection{fSum}
\label{sec:fsum}

@d fSumUsage
@{
fSum::usage=
"place holder fSum"
@}

@d fSum
@{
fSum[@<linMod@>,
	{},
	xtGuess_?MatrixQ]:=
ConstantArray[0,{Length[psiZ],1}]

fSum[@<linMod@>,
	@<XZFuncs@>,xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xzRes=Apply[multiStepZ[XZFuncs,numXVars,numZVars,numSteps], Flatten[xtGuess]]},
fSumC[phi,FF,psiZ,xzRes]]]
@}


\subsection{genSlots}
\label{sec:genxtm1vars}


@d genSlots
@{
(*begin code for genSlots*)
genSlots[numVars_Integer]:=
Module[{},
genSlots[numVars]=
replaceMySlotStandIn[Table[{mySlotStandIn[ii]},{ii,numVars}]]]/;And[numVars>=0]

replaceMySlotStandIn[xx_]:=xx/.mySlotStandIn->Slot



genSlot[slotNum_Integer]:=
Module[{},
genSlot[slotNum]=
replaceMySlotStandIn[mySlotStandIn[slotNum]]]/;And[slotNum>0]

(*end code for genSlots*)
@}




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

\subsection{genX0Z0Funcs}
\label{sec:genx0z0funcs}


@d genX0Z0FuncsUsage
@{genX0Z0Funcs::usage=
"place holder for genX0Z0Funcs"
@}

@d genX0Z0Funcs
@{
(*begin code for genX0Z0Funcs*)
genX0Z0Funcs[@<linMod@>]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genSlots[numXVars]},
Apply[Function, {Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]}]]]
(*end code for genX0Z0Funcs*)
@}


\subsection{multiStep Functions}
\label{sec:multistep-functions}

@d multiStepUsage
@{multiStep::usage=
"place holder for multiStep"
@}

@d multiStep
@{
(*begin code for multiStep*)

multiStep[@<XZFuncs@>,numX_Integer,valRange:{_Integer..},numTerms_Integer]:=
With[{funcArgs=Flatten[genSlots[numX]]},
With[{appGuts=(Apply[XZFuncs[[1]],Flatten[funcArgs]][[Range[numX]]])},
With[{xtFunc01=Function[appGuts]},
With[{iterGuts=
NestList[Function[xx,Apply[xtFunc01,Flatten[xx]]],funcArgs,numTerms-1]},
With[{theXZGuts=Map[(Function[xx,
Apply[XZFuncs[[1]],Flatten[xx]][[valRange]]]),iterGuts]},
With[{theFunc=Function[theXZGuts]},
theFunc]]]]]]/;numSteps>0


(*end code for multiStep*)
@}

@d multiStepZUsage
@{multiStepZ::usage=
"place holder for multiStepZ"
@}

@d multiStepZ
@{
(*begin code for multiStepZ*)
multiStepZ[@<XZFuncs@>,numX_Integer,numZ_Integer,numTerms_Integer]:=
multiStep[XZFuncs,numX,numX+Range[numZ],numTerms]

(*end code for multiStepZ*)
@}

@d multiStepXUsage
@{multiStepX::usage=
"place holder for multiStepX"
@}

@d multiStepX
@{
(*begin code for multiStepX*)
multiStepX[@<XZFuncs@>,numX_Integer,numTerms_Integer]:=
multiStep[XZFuncs,numX,Range[numX],numTerms]

(*end code for multiStepX*)
@}


\subsection{checkLinMod}
\label{sec:checklinmod}


@d checkLinModUsage
@{checkLinMod::usage=
"place holder for checkLinMod"
@}

@d checkLinMod
@{
(*begin code for checkLinMod*)

checkLinMod[@<linMod@>,
anX_?MatrixQ,anEps_?MatrixQ]:=
With[{X0Z0=genX0Z0Funcs[linMod],numZ=Length[psiZ[[1]]]},
With[{lilxz=genLilXkZkFunc[linMod, {X0Z0,2}, Join[anX,anEps]]},
	{Eigenvalues[BB]//Abs,Eigenvalues[FF]//Abs,Apply[X0Z0,Flatten[anX]],Apply[lilxz,Flatten[Join[anX,anEps,Table[{0},{numZ}]]]]}]]


(*end code for checkLinMod*)
@}

\subsection{checkMod}
\label{sec:checkmod}


@d checkModUsage
@{checkMod::usage=
"place holder for checkMod"
@}

@d checkMod
@{
(*begin code for checkMod*)



checkMod[@<theSolver@>,@<linMod@>,
@<gSpec@>,
@<distribSpec@>,anX_?MatrixQ,anEps_?MatrixQ,ss_?MatrixQ,
@<eqnsFunc@>]:=
With[{X0Z0=genX0Z0Funcs[linMod],numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{lilxz=
genLilXkZkFunc[linMod, {X0Z0,1}, Join[anX,anEps]]},
With[{xzFuncNow=theSolver[[1]][{numX,numEps,numZ},lilxz,eqnsFunc,Method->"JenkinsTraub"]},
With[{fp=genFPFunc[theSolver,linMod,{X0Z0,2},eqnsFunc]},
{Apply[lilxz,Flatten[Join[anX,anEps,Table[0,{numZ}]]]],
Apply[xzFuncNow,Flatten[Join[anX,anEps]]],
Apply[fp,Flatten[Join[anX,anEps]]],
Apply[eqnsFunc,Flatten[Join[ss,{{0}}]]]
}]]]]



(*end code for checkMod*)
@}

\subsection{genFRFunc}
\label{sec:genfrfunc}


@d genFRFuncUsage
@{genFRFunc::usage=
"place holder for genFRFunc"
@}

@d genFRFunc
@{

(*begin code for genFRFunc*)
 
genFRFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),@<eqnsFunc@>,opts:OptionsPattern[]]:=
With[{funcArgs=Flatten[genSlots[numX+numEps]],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
With[{zArgsInit=Map[Function[xx,{xx,0}],zArgs],funcName=Unique["fName"]},
funcName[theVars:{_?NumberQ..}]:=
Apply[eqnsFunc,Flatten[Apply[xkFunc,theVars]]];Off[FindRoot::nlnum];
With[{frRes=FindRoot[funcName[Join[funcArgs,zArgs]],zArgsInit],
xzRes=Drop[Apply[xkFunc,Join[funcArgs,zArgs]],numX][[Range[numX]]]},
With[{otherGuts=cmpXZVals[xzRes,zArgs,frRes]},
On[FindRoot::nlnum];
Function[otherGuts]]]]]

(* input   [function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)
 
cmpXZVals[xzVals_?MatrixQ,theZArgs:{_Symbol..},theResult:{(_->_)..}]:=
Transpose[{Flatten[Join[xzVals,theZArgs]/.theResult]}]


(*end code for genFRFunc*)
@}

\subsection{genFPFunc}
\label{sec:genfpfunc}


@d genFPFuncUsage
@{genFPFunc::usage=
"place holder for genFPFunc"
@}

@d genFPFunc
@{
(*begin code for genFPFunc*)
	
fixedPointLimit=30;
genFPFunc[@<theSolver@>,@<linMod@>,@<XZFuncs@>,@<eqnsFunc@>]:=
With[{numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}]},
ReplacePart[
Function[xxxx,Sow[
myFixedPoint[Function[xx,With[{
xzFuncNow=theSolver[[1]][
{numX,numEps,numZ},
genLilXkZkFunc[linMod,XZFuncs,xx[[Range[numX]]]],
eqnsFunc,{opts}]},
Apply[xzFuncNow,funcArgs]]],(Apply[XZFuncs[[1]],funcArgs])[[Range[numX]]],
fixedPointLimit]]],
1->funcArgs]]]
(* input   [linMod,XZ, xguess,function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)


(*end code for genFPFunc*)
@}

\subsection{myFixedPoint}
\label{sec:myfixedpoint}

@d myFixedPointUsage
@{myFixedPoint::usage=
"place holder for myFixedPoint"
@}

@d myFixedPoint
@{
(*begin code for myFixedPoint*)

myFixedPoint[firstArg_,secondArg_,thirdArg_]:=
Module[{},
FixedPoint[firstArg,secondArg,thirdArg]]
	

(*end code for myFixedPoint*)
@}


\subsection{makeInterpFunc}
\label{sec:makeinterpfunc}




@d makeInterpFuncUsage
@{makeInterpFunc::usage=
"place holder for makeInterpFunc"
@}

@d makeInterpFunc
@{
(*begin code for makeInterpFunc*)

makeInterpFunc[aVecFunc:(_Function|_CompiledFunction),@<gSpec@>]:=
With[{interpData=genInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["fArgs"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,Interpolation[
Map[Function[xx,{xx[[1]], xx[[2, funcIdx, 1]]}] , 
		interpData],InterpolationOrder -> iOrd]],Range[numFuncs]]},
		With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
	ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}]
	]
]]]]




(*end code for makeInterpFunc*)
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

parallelMakeInterpFunc[aVecFunc:(_Function|_CompiledFunction),@<gSpec@>]:=
With[{interpData=parallelGenInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["fArgs"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,Interpolation[
Map[Function[xx,{xx[[1]], xx[[2, funcIdx, 1]]}] , 
		interpData],InterpolationOrder -> iOrd]],Range[numFuncs]]},
		With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
	ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}]
	]
]]]]




(*end code for parallelMakeInterpFunc*)
@}


\subsection{genInterpData}
\label{sec:geninterpdata}



@d genInterpDataUsage
@{genInterpData::usage=
"place holder for genInterpData"
@}

@d genInterpData
@{
(*begin code for genInterpData*)

 
genInterpData[aVecFunc:(_Function|P_CompiledFunction),@<gSpec@>]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=Map[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=Map[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]





(*end code for genInterpData*)
@}


\subsection{parallelGenInterpData}
\label{sec:geninterpdata}



@d parallelGenInterpDataUsage
@{parallelGenInterpData::usage=
"place holder for parallelGenInterpData"
@}

@d parallelGenInterpData
@{
(*begin code for parallelGenInterpData*)

 
parallelGenInterpData[aVecFunc:(_Function|P_CompiledFunction),@<gSpec@>]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=ParallelMap[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=ParallelMap[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]





(*end code for parallelGenInterpData*)
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

fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
fillIn[{theRes,Sort[toIgnore],shortVec}]


(*end code for fillInSymb*)
@}


\subsection{doIterREInterp}
\label{sec:doiterreinterp}



@d doIterREInterpUsage
@{doIterREInterp::usage=
"place holder for doIterREInterp"
@}

@d doIterREInterp
@{
(*begin code for doIterREInterp*)
doIterREInterp[@<theSolver@>,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<gSpec@>,@<distribSpec@>]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=makeInterpFunc[genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],gSpec]},
{theFuncs,genXZREInterpFunc[{numX,numEps,numZ},theFuncs,gSpec,distribSpec]}]]




(*end code for doIterREInterp*)
@}

\subsection{nestIterREInterp}
\label{sec:nestiterreinterp}


@d nestIterREInterpUsage
@{nestIterREInterp::usage=
"place holder for nestIterREInterp"
@}

@d nestIterREInterp
@{
(*begin code for nestIterREInterp*)


nestIterREInterp[@<theSolver@>,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,
@<gSpec@>,
@<distribSpec@>,numIters_Integer]:=
NestList[Function[xx,doIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,gSpec,distribSpec]],{ig,XZFuncs[[1]]},numIters]




(*end code for nestIterREInterp*)
@}

\subsection{genXZREInterpFunc}
\label{sec:genxzreinterpfunc}


@d genXZREInterpFuncUsage
@{genXZREInterpFunc::usage=
"place holder for genXZREInterpFunc"
@}

@d genXZREInterpFunc
@{
(*begin code for genXZREInterpFunc*)
 
genXZREInterpFunc[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,@<gSpec@>,@<distribSpec@>]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
makeInterpFunc[theFuncNow,elimGSpecShocks[gSpec,numEps]]]
  
elimGSpecShocks[@<gSpec@>,numEps_Integer]:=
{toIgnore,gSpec[[2]],Drop[getGridPtTrips[gSpec],-(numEps)]}

(*end code for genXZREInterpFunc*)
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



(*end code for genXZFuncRE*)
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

myNExpectation[funcName_Symbol[farg_List,idx_Integer],nArgs_List]:=Chop[NExpectation[funcName[farg,idx],nArgs]]


myNewNExpectation[fff_[fargs___],anEpsVar_\[Distributed] PerfectForesight]:=Module[{},Print["there",{(Apply[fff,{fargs}]),{fargs}/.anEpsVar->0}];(Apply[fff,{fargs}])/.anEpsVar->0]


myNewNExpectation[fff_[fargs___],distStuff_]:=Module[{},Print["jhere",{(Apply[fff,{fargs}]),{fargs}}];Chop[NExpectation[Applyp[fff,{fargs}],distStuff]]]



(*end code for myNExpectation*)
@}


\subsection{iterateDRREIntegrate}
\label{sec:iteratedrreintegrate}



@d iterateDRREIntegrateUsage
@{iterateDRREIntegrate::usage=
"place holder for iterateDRREIntegrate"
@}

@d iterateDRREIntegrate
@{
(*begin code for iterateDRREIntegrate*)
iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction),initVec_?MatrixQ,
	@<distribSpec@>,numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec],firVal=Apply[drFunc,Flatten[initVec]]},
	With[{numX=Length[initVec]-numEps,iterFunc=makeREIterFunc[drFunc,distribSpec]},
With[{iterated=
NestList[Function[xx,((Transpose[{Flatten[Apply[iterFunc,Flatten[xx]]]}]))],firVal,numPers-1]},
Join[initVec[[Range[numX]]],Apply[Join,
(Map[Function[xx,Identity[xx[[Range[numX]]]]],iterated])]]]]]/;
And[numPers>0]


(*end code for iterateDRREIntegrate*)
@}

\subsection{makeREIterFunc}
\label{sec:makereiterfunc}





@d makeREIterFuncUsage
@{makeREIterFunc::usage=
"place holder for makeREIterFunc"
@}

@d makeREIterFunc
@{
(*begin code for makeREIterFunc*)

makeREIterFunc[drFunc:(_Function|_CompiledFunction),@<distribSpec@>]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[drFunc[[1]]]-numEps,numZ=0},
	genXZFuncRE[{numX,numEps,numZ},drFunc,distribSpec]]]


(*end code for makeREIterFunc*)
@}


\subsection{genNSFunc}
\label{sec:gennsfunc}



@d genNSFuncUsage
@{genNSFunc::usage=
"place holder for genNSFunc"
@}

@d genNSFunc
@{
(*begin code for genNSFunc*)
genNSFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),@<eqnsFunc@>,opts:OptionsPattern[]]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX+numEps}],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
funcName[funcArgsNot:{_(*?NumberQ*)..}]:=
Module[{theVars=Join[funcArgsNot]},
Apply[eqnsFunc,(Flatten[Apply[xkFunc,theVars]])]];
ReplacePart[
Function[xxxx,With[{zVals=zArgs/.NSolve[funcName[Join[funcArgs,zArgs]],zArgs,Reals,Apply[Sequence,FilterRules[{opts},Options[NSolve]]]][[1]]},
Join[(Apply[xkFunc,Join[funcArgs,zVals]])[[numX+Range[numX]]],
Transpose[{zVals}]]]],
1->funcArgs]]


(*end code for genNSFunc*)
@}



\subsection{iterateDRPF}
\label{sec:iteratedrpf}


@d iterateDRPFUsage
@{iterateDRPF::usage=
"place holder for iterateDRPF"
@}

@d iterateDRPF
@{
(*begin code for iterateDRPF*)
 
iterateDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,numPers_Integer]:=
With[{firVal=Apply[drFunc,Flatten[initVec]],numX=Length[initVec]-numEps,theZeros=Table[0,{numEps}]},
With[{iterated=
NestList[Function[xx,(Apply[drFunc,Flatten[Append[xx[[Range[numX]]],theZeros]]])],firVal,numPers-1]},
Join[initVec[[Range[numX]]],Apply[Join,(Map[Function[xx,xx[[Range[numX]]]],iterated])]]]]/;
And[numPers>0]


(*end code for iterateDRPF*)
@}


\subsection{genPath}
\label{sec:genpath}


@d genPathUsage
@{genPath::usage=
"place holder for genPath"
@}

@d genPath
@{
(*begin code for genPath*)


genPath[xzFunc_Function,
@<XZFuncs@>,xtm1Val_?MatrixQ,epsVal_?MatrixQ,numTerms_Integer:1]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=Apply[xzFunc,Flatten[Join[xtm1Val,epsVal]]]},
With[{xzRes=If[numTerms==1,{},
Apply[multiStepX[XZFuncs,numXVars,numTerms-1],Flatten[xtVal]]]},
	Join[xtm1Val,xtVal[[Range[numXVars]]],Apply[Join,xzRes]]]]]
(*end code for genPath*)
@}



\subsection{pathErrsDRPF}
\label{sec:patherrsdrpf}



@d pathErrsDRPFUsage
@{pathErrsDRPF::usage=
"place holder for pathErrsDRPF"
@}

@d pathErrsDRPF
@{
(*begin code for pathErrsDRPF*)
   
 
pathErrsDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>,numPers_Integer]:=
With[{pathNow=iterateDRPF[drFunc,initVec,numEps,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
	restArgs=(Map[Function[xx,
doFuncArg[pathNow,Table[{0},{numEps}],numX,xx-2]],Range[3,numPers]])},
With[{first=Transpose[{Apply[eqnsFunc,Flatten[firstArg]]}]},
	With[{theRest=Map[Function[xx,Transpose[{(Apply[eqnsFunc,Flatten[xx]])}]],restArgs]},
		Prepend[theRest,first]
]]]]/;
And[numPers>1]


(*end code for pathErrsDRPF*)
@}

\subsection{pathErrsDRREIntegrate}
\label{sec:patherrsdrreintegrate}



@d pathErrsDRREIntegrateUsage
@{pathErrsDRREIntegrate::usage=
"place holder for pathErrsDRREIntegrate"
@}

@d pathErrsDRREIntegrate
@{
(*begin code for pathErrsDRREIntegrate*)
pathErrsDRREIntegrate[drFunc_Function,initVec_?MatrixQ,@<distribSpec@>,@<eqnsFunc@>,numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{pathNow=iterateDRREIntegrate[drFunc,initVec,distribSpec,numPers],numX=Length[initVec]-numEps},(*Print["pathErrsDRREIntegrate:",pathNow];*)
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
	restArgs=(Map[doFuncArg[pathNow,Table[{0},{numEps}],numX,genSlot[1]-2]&,Range[3,numPers]])},
With[{first=Transpose[{Apply[eqnsFunc,Flatten[firstArg]]}]},
	With[{theRest=Map[Transpose[{(Apply[eqnsFunc,Flatten[genSlot[1]]])}]&,restArgs]},(*Print["pathErrs:",{pathNow,theRest,first}];*)
		Prepend[theRest,first]
]]]]]/;
And[numPers>1]
 

(*end code for pathErrsDRREIntegrate*)
@}


\subsection{doFuncArg}
\label{sec:dofuncarg}

@d doFuncArgUsage
@{doFuncArg::usage=
"place holder for doFuncArg"
@}

@d doFuncArg
@{
(*begin code for doFuncArg*)
doFuncArg[pathNow_?MatrixQ,epsVals_?MatrixQ,numX_Integer,oSet_Integer]:=
With[{firstArg=Join[Identity[pathNow[[oSet*numX+Range[3*numX]]]],Identity[epsVals]]},
firstArg]


(*end code for doFuncArg*)
@}



\subsection{evalPathErrDRREIntegrate}
\label{sec:evalp}

@d evalPathErrDRREIntegrateUsage
@{evalPathErrDRREIntegrate::usage=
"place holder for evalPathErrDRREIntegrate"
@}

@d evalPathErrDRREIntegrate
@{
(*begin code for evalPathErrDRREIntegrate*)
evalPathErrDRREIntegrate[drFunc_Function,initVec_?MatrixQ,@<distribSpec@>,@<eqnsFunc@>]:=
pathErrsDRREIntegrate[drFunc,initVec,distribSpec,eqnsFunc,2]//First



evalPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,initVec_?MatrixQ,@<distribSpec@>,@<eqnsFunc@>]:=
phi . (pathErrsDRREIntegrate[drFunc,initVec,distribSpec,eqnsFunc,2])//First




(*end code for evalPathErrDRREIntegrate*)
@}

\subsection{evalBadPathErrDRREIntegrate}
\label{sec:evalb}


@d evalBadPathErrDRREIntegrateUsage
@{evalBadPathErrDRREIntegrate::usage=
"place holder for evalBadPathErrDRREIntegrate"
@}

@d evalBadPathErrDRREIntegrate
@{
(*begin code for evalBadPathErrDRREIntegrate*)
evalBadPathErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,@<distribSpec@>,@<eqnsFunc@>]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
	With[{theVal=evalPathErrDRREIntegrate[drFunc,Join[noEpsVec,Transpose[{tryEps}]],distribSpec,eqnsFunc]},
		With[{theNorm=Norm[theVal,Infinity]},
		(*Print["stillex:",{tryEps,theVal,Norm[theVal,Infinity],theNorm}];*)theNorm]];
	With[{outerEVars=Table[Unique["eVs"],{getNumEpsVars[distribSpec]}]},
	With[{maxArgs=Map[Function[xx,{xx,0}],outerEVars],cons=Apply[And,  (Map[Function[xx,(-0.01<=xx<=0.01)], outerEVars])]},
	FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]


evalBadPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,noEpsVec_?MatrixQ,@<distribSpec@>,@<eqnsFunc@>]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
With[{theVal=evalPathErrDRREIntegrate[drFunc,Join[noEpsVec,Transpose[{tryEps}]],distribSpec,eqnsFunc]},
		(*Print["otherex:",theVal,Norm[theVal,Infinity]];*)Norm[theVal,Infinity]];
	With[{outerEVars=Table[Unique["eVs"],{getNumEpsVars[distribSpec]}]},
	With[{maxArgs=Map[Function[xx,{xx,0}],outerEVars],cons=Apply[And,  (Map[Function[xx,(-0.01<=xx<=0.01)], outerEVars])]},
	FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]

(*end code for evalBadPathErrDRREIntegrate*)
@}

\subsection{worstPathForErrDRREIntegrate}
\label{sec:worstp}




@d worstPathForErrDRREIntegrateUsage
@{worstPathForErrDRREIntegrate::usage=
"place holder for worstPathForErrDRREIntegrate"
@}

@d worstPathForErrDRREIntegrate
@{
(*begin code for worstPathForErrDRREIntegrate*)

worstPathForErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,@<distribSpec@>,@<eqnsFunc@>]:=
With[{fMinRes=evalBadPathErrDRREIntegrate[drFunc,noEpsVec,distribSpec,eqnsFunc]},
	With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},
	With[{badPath=iterateDRREIntegrate[drFunc,Join[noEpsVec,badEps],distribSpec,2]},
		Join[badPath,badEps]]]]

worstPathForErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,noEpsVec_?MatrixQ,@<distribSpec@>,@<eqnsFunc@>]:=
With[{fMinRes=
evalBadPathErrDRREIntegrate[phi,drFunc,noEpsVec,distribSpec,eqnsFunc]},
	With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},
	With[{badPath=iterateDRREIntegrate[drFunc,Join[noEpsVec,badEps],distribSpec,2]},
		Join[badPath,badEps]]]]

(*end code for worstPathForErrDRREIntegrate*)
@}


\subsection{Getters and Setters}
\label{sec:getters-setters}

@d gettersSettersUsage
@{
(*some getters usage*)
@}
@d gettersSetters
@{
(*some getters*)
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



@d getNumVarsUsage
@{getNumVars::usage=
"place holder for getNumVars"
@}

@d getNumVars
@{
(*begin code for getNumVars*)
getNumVars[@<gSpec@>]:=
(Length[getGridPtTrips[gSpec]])

(*end code for getNumVars*)
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


@d getPhiUsage
@{
getPhi::usage=
"getPhi[@<linMod@>]"<>
"number of z variables"
@}

@d getPhi
@{
getPhi[@<linMod@>]:=
phi
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



@d getPsiCUsage
@{
getPsiC::usage=
"getPsiC[@<linMod@>]"<>
"number of z variables"
@}

@d getPsiC
@{
getPsiC[@<linMod@>]:=
psiC
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


@d getNumIgnoredUsage
@{
getNumIgnored::usage=
"getNumIgnored[@<linMod@>]"<>
"number of eps variables"
@}

@d getNumIgnored
@{
getNumIgnored[@<gSpec@>]:=
Length[gSpec[[1]]]
@}

@d getNumInterpVarsUsage
@{
getNumInterpVars::usage=
"getNumInterpVars[@<linMod@>]"<>
"number of eps variables"
@}

@d getNumInterpVars
@{
getNumInterpVars[@<gSpec@>]:=
Length[gSpec[[3]]]
@}

@d getGridPtTripsUsage
@{getGridPtTrips::usage=
"place holder for getGridPtTrips"
@}

@d getGridPtTrips
@{
(*begin code for getGridPtTrips*)

getGridPtTrips[@<gSpec@>]:=gSpec[[3]]
  

(*end code for getGridPtTrips*)
@}



\subsection{nuweb Macro Definitions}
\label{sec:nuweb-macro-defin}


@d xxxxxxUsage
@{xxxxxx::usage=
"place holder for xxxxxx"
@}

@d xxxxxx
@{
(*begin code for xxxxxx*)

(*end code for xxxxxx*)
@}





@d gencall@{@1[@2]@}


\subsection{Identifiers}
\label{sec:identifiers}

@u
\subsection{Macros}
\label{sec:macros}

@m



\end{document}
