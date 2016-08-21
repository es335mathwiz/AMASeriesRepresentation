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
BeginPackage["AMASeriesRepresentation`"
(*, {"JLink`","ProtectedSymbols`"}*)]
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

(*End Usage Definitions*)
@}

@d package code
@{
@<getNumZ@>

@<genLilXkZkFunc@>

@}

\subsection{Argument Specifications}
\label{sec:argum-spec}


@d linMod
@{linMod:{theHMat_?MatrixQ BB_?MatrixQ phi_?MatrixQ FF_?MatrixQ 
psiEps_?MatrixQ psiC_?MatrixQ psiZ_?MatrixQ psiZPreComp_?MatrixQ} 
@|
boo
linMod
BB
phi
FF
psiZ
psiEps
xxxxtheHMat
xxxxpsiC
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




@d gencall@{@1[@2]@}


\subsection{Identifiers}
\label{sec:identifiers}

@u
\subsection{Macros}
\label{sec:macros}

@m



\end{document}
