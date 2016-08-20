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
BeginPackage["AMASeriesRepresentation`"(*, {"JLink`","ProtectedSymbols`"}*)]
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
@{linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,
psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}@}

@d XZFuncs
@{XZFuncs:({_Function,_Integer})@}

@d theZMats
@{theZs:{_?MatrixQ..}@}


@d drvPairs
@{drvPairs:{{{aa_Integer,bb_Integer}...},
eqnFunc:(_Function|_CompiledFunction)}:{{},{}}@}
\subsection{genLilXkZkFunc}
\label{sec:genlilxkzkfunc}


@d genLilXkZkFuncUsage
@{
genLilXkZkFunc::usage=
"@<gencall@(genLilXkZkFunc@,@<linMod@>,@<XZFuncs@>@)@>"<>
"generate a function that computes x and z given a guess for xt"
@}



@d genLilXkZkFunc
@{
@<gencall@(genLilXkZkFunc@,@<linMod@>,@<XZFuncs@>@,@<fConZero@>@)@>:=
@<fConZero@>
@}

@d fConZero
@{
With[{numZ=getNumZ[linMod]},
With[{fCon=ConstantArray[0,{1,numZ,1}]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},theRes]]]
@}


@d genLilXkZkFunc
@{
@<gencall@(genLilXkZkFunc@,@<linMod@>,@<theZMats@>@,@<Z Matrices Given@>@)@>:=
@<Z Matrices Given@>
@}



@d Z Matrices Given
@{

With[{fCon=fSumC[phi,FF,psiZ,theZs]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]

@}

@d genLilXkZkFunc
@{
@<gencall@(genLilXkZkFunc@,@<linMod@>,@<XZFuncs@>@,@<XZ Functions Given@>@)@>:=
@<XZ Functions Given@>
@}


@d XZ Functions Given
@{

genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncs:({_Function,_Integer}),xtGuess_?MatrixQ,drvPairs:{{{aa_Integer,bb_Integer}...},eqnFunc:(_Function|_CompiledFunction)}:{{},{}}]:=
	With[{fCon=fSum[linMod,XZFuncs,xtGuess,2]},
		With[{theRes=genLilXkZkFunc[linMod,fCon,drvPairs]},
theRes]]


@}

@d genLilXkZkFunc
@{
@<gencall@(genLilXkZkFunc@,@<linMod@>,@<XZFuncs@>@,@<apply formula F...@>@)@>:=
@<apply formula F...@>
@}

@d apply formula F contribution given
@{
genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	fCon_?MatrixQ,drvPairs:{({}|{{aa_Integer,bb_Integer}...}),eqnFunc:({}|_Function|_CompiledFunction)}:{{},{}}]:=
With[{numXVars=Length[BB],numEpsVars=Length[psiEps[[1]]],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=Transpose[{genXtm1Vars[numXVars]}],epsVars=Transpose[{genEpsVars[numEpsVars]}],
zVars=Transpose[{Reverse[Flatten[genZVars[0,numZVars]]]/.name_[t]->name}]},
With[{xtVals=genXtOfXtm1[linMod,xtm1Vars,epsVars,zVars,fCon]},
With[{xtp1Vals=genXtp1OfXt[linMod,xtVals,fCon]},
With[{fullVec=Join[xtm1Vars,xtVals,xtp1Vals,epsVars]},
With[{(*theDrvs=doImplicitDrv[linMod,fullVec,zVars,xtm1Vars,epsVars,drvPairs]*)},(*Print["theDrvs",theDrvs];*)
ReplacePart[
Function[xxxx,fullVec],{1->Flatten[Join[xtm1Vars,epsVars,zVars]]}]
]]]]]]

@}



@d need to absorb
@{
(*

*)
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
@<gencall@(getNumZ@,@<linMod@>@,@)@>:=
Length[getPsiZ[linMod][[1]]]
@}

\subsection{nuweb Macro Definitions}
\label{sec:nuweb-macro-defin}







@d genusage@{@1::usage=
"@1[@2]"<>@}

@d gencall@{@1[@2]@}






\end{document}
