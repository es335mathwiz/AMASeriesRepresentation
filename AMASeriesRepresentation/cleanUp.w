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



@o cleanUp.m
@{
BeginPackage["cleanUp`",
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

@<genLilXkZkFuncUsage@>

@<getNumZUsage@>

@<fSumCUsage@>

@<fSumUsage@>

@<genX0Z0FuncsUsage@>

(*End Usage Definitions*)
@}

@d package code
@{
@<getNumZ@>

@<genLilXkZkFunc@>

@<fSumC@>

@<fSum@>

@<genXtm1Vars@>

@<genXtOfXtm1@>

@<genXtp1OfXtm1@>

@<genX0Z0Funcs@>

@}

\subsection{Argument Specifications}
\label{sec:argum-spec}


@d linMod
@{linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
@|
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
@{XZFuncs:({_Function,_Integer})@}

@d theZs
@{theZs:{_?MatrixQ..}@}


@d drvPairs
@{drvPairs:{{{aa_Integer,bb_Integer}...},
eqnFunc:(_Function|_CompiledFunction)}:{{},{}}@}

@d xtGuess
@{xtGuess_?MatrixQ@}

@d fCon
@{fCon_?MatrixQ@}

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

@d genLilXkZkFunc full call
@{@<gencall@(genLilXkZkFunc@,@<linMod@>,@<XZFuncs@>,@<xtGuess@>,@<drvPairs@>@)@>@}

@d genLilXkZkFunc fcon call
@{@<gencall@(genLilXkZkFunc@,@<linMod@>,@<fCon@>,@<drvPairs@>@)@>@}

@d genLilXkZkFunc theZs call
@{@<gencall@(genLilXkZkFunc@,@<linMod@>,@<theZs@>@)@>@}

@d genLilXkZkFunc noZs call
@{ @<gencall@(genLilXkZkFunc@,@<linMod@>,{}@)@>@}


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
@<Z Matrices Given@>
@}

@d Z Matrices Given
@{With[{fCon=fSumC[phi,FF,psiZ,theZs]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]

@}

@d genLilXkZkFunc
@{
@<genLilXkZkFunc full call@>:=
@<XZ Functions Given@>
@}

@d XZ Functions Given
@{With[{fCon=fSum[linMod,XZFuncs,xtGuess,2]},
With[{theRes=genLilXkZkFunc[linMod,fCon,drvPairs]},
theRes]]
@}

@d genLilXkZkFunc
@{
@<genLilXkZkFunc fcon call@>:=
@<apply formula F...@>
@}

@d apply formula F contribution given
@{With[{numXVars=Length[BB],numEpsVars=Length[psiEps[[1]]],
numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=Transpose[{genXtm1Vars[numXVars]}],
epsVars=Transpose[{genEpsVars[numEpsVars]}],
zVars=Transpose[{Reverse[Flatten[genZVars[0,numZVars]]]/.name_[t]->name}]},
With[{xtVals=genXtOfXtm1[linMod,xtm1Vars,epsVars,zVars,fCon]},
With[{xtp1Vals=genXtp1OfXt[linMod,xtVals,fCon]},
With[{fullVec=Join[xtm1Vars,xtVals,xtp1Vals,epsVars]},
With[{(*theDrvs=doImplicitDrv[linMod,fullVec,
zVars,xtm1Vars,epsVars,drvPairs]*)},(*Print["theDrvs",theDrvs];*)
ReplacePart[
Function[xxxx,fullVec],{1->Flatten[Join[xtm1Vars,epsVars,zVars]]}]
]]]]]]
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
With[{fPows=Drop[NestList[FF.#&,IdentityMatrix[numXVars],Length[zPath]],-1]},
Apply[Plus,
MapThread[Dot[#1,phi.psiZ.#2]&,{fPows , zPath}]]]]]

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
fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{},
	xtGuess_?MatrixQ]:=
ConstantArray[0,{Length[psiZ],1}]

fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{XZFunc_Function,numSteps_Integer},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xzRes=Apply[multiStepZ[{XZFunc,numSteps},numXVars,numZVars,numSteps], Flatten[xtGuess]]},
fSumC[phi,FF,psiZ,xzRes]]]
@}


\subsection{genXtm1Vars}
\label{sec:genxtm1vars}


@d genXtm1Vars
@{
(*begin code for genXtm1Vars*)
genXtm1Vars[numVars_Integer]:=
Module[{},
genXtm1Vars[numVars]=
Table[
makeProtectedSymbol["xxxtm1Var"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]

(*end code for genXtm1Vars*)
@}




\subsection{genXtOfXtm1}
\label{sec:genxtofxtm1}



@d genXtOfXtm1
@{
(*begin code for genXtOfXtm1*)
genXtOfXtm1[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xtm1Vars_?MatrixQ,epsVars_?MatrixQ,zVars_?MatrixQ,
	fCon_?MatrixQ]:=
With[{xtVals=BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . epsVars+
phi . psiZ . zVars +FF.fCon},xtVals]

(*end code for genXtOfXtm1*)
@}


\subsection{genXtp1OfXt}
\label{sec:genxtp1ofxt}



@d genXtp1OfXtm1
@{
(*begin code for genXtp1OfXtm1*)

genXtp1OfXtm1[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xtVals_?MatrixQ,
	fCon_?MatrixQ]:=
With[{xtp1Vals=BB.xtVals+Inverse[IdentityMatrix[Length[xtVals]]-FF] . phi . psiC+fCon},xtp1Vals]


(*end code for genXtp1OfXtm1*)
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
genX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars]},
With[{compArgs=xtm1Vars},
Apply[Function, {compArgs,Join[BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]}]]]]
(*end code for genX0Z0Funcs*)
@}


\subsection{Getters and Setters}
\label{sec:getters-setters}

@d getNumZUsage
@{
getNumZ::usage=
"@<gencall@(getNumZ@,@<linMod@>@)@>"<>
"number of z variables"
@}

@d getNumZ
@{
@<gencall@(getNumZ@,@<linMod@>@)@>:=
Length[getPsiZ[linMod][[1]]]
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
