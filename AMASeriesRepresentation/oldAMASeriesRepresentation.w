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

parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoGenericIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},triples,smolGSpec,genericInterp,svmArgs,Apply[Sequence,FilterRules[{opts},Options[parallelDoGenericIterREInterp]]]]],{99,XZFuncs[[1]]},numIters]]


parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<bothXZFuncs@>,@<eqnsFunc@>,@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoGenericIterREInterp[genFRExtFunc,linMod,
{xx,numSteps},eqnsFunc,smolGSpec,genericInterp,svmArgs,Apply[Sequence,FilterRules[{opts},Options[parallelDoGenericIterREInterp]]]]],justBothXZFuncs,numIters]]

parallelNestGenericIterREInterp[genFRExtFunc,@<linMod@>,
@<bothXZFuncs@>,
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoGenericIterREInterp[genFRExtFunc,linMod,
{xx,numSteps},triples,smolGSpec,genericInterp,svmArgs,Apply[Sequence,FilterRules[{opts},Options[parallelDoGenericIterREInterp]]]]],justBothXZFuncs,numIters]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["nestIterREInterp"->#&,{"doGenericIterREInterp"}]];



(*end code for nestGenericIterREInterp*)
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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["doSmolyakIterREInterp"->#&,
{"makeSmolyakInterpFuncs","genFPFunc","getNumX",
"getNumEps","getNumZ","genXZREInterpFunc"}]];



(*end code for doSmolyakIterREInterp*)
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

parallelDoGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,triples,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Print["timing distributedefinitionsin parallelDoGenericIter:",AbsoluteTiming[Apply[DistributeDefinitions,Flatten[reapRes[[2]]]]]];
With[{theFuncs=
parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,
genericInterp,svmArgs]},
theFuncs]]

parallelDoGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<bothXZFuncs@>,
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

parallelDoGenericIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<bothXZFuncs@>,
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},opts:OptionsPattern[]]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,triples,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=
parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,
genericInterp,svmArgs]},
theFuncs]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["doSmolyakIterREInterp"->#&,
{"makeSmolyakInterpFuncs","genFPFunc","getNumX",
"getNumEps","getNumZ","genXZREInterpFunc"}]];



(*end code for doSmolyakIterREInterp*)
@}



\section{Smolyak Code}
\label{sec:smolyak-code}
This code implements anisotropic smolyak as described in \cite{Judd2014}.

\subsection{smolGSpec}
\label{sec:smolyakinterpolation}

This data provides a sapecification for the smolyak points.


@d smolyakInterpolationUsage
@{smolyakInterpolation::usage=
"place holder for makeSmolyakInterpFuncs"
@}

The Smolyak grid specification has 7 components:
\begin{description}
\item[smolToIgnore] Indicates which components of x are not
components of the essential state vector.
\item[smolRngs] a list of upper and lower bounds for each variable in the state.
This tally includes time t shocks.
\item[smolPts] a matrix containing the smolyak points.  ??? can generate these 
points.(?? why ranges needed ??)
\item[smolMat] for computing the weights
\item[smolPolys] the polynomials corresponding to the anisotropic formulation
\item[smolIntPolys] the polynomials integrated apriori to speed the calculation
\item[numEps] the number of shocks.  shocks are at the end of the vector x.
\end{description}

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


\begin{verbatim}
sgSpec//InputForm

Out[88]//InputForm= 
{{2}, {{0, 1}, {-0.03, 0.03}}, {{1/2, 0.}, {1/2, -0.03}, {1/2, 0.03}, 
  {0, 0.}, {1, 0.}}, {{1, 0, -1, 0, -1}, {1, -1, 1, 0, -1}, {1, 1, 1, 0, -1}, 
  {1, 0, -1, -1, 1}, {1, 0, -1, 1, 1}}, {1, xx[2], -1 + 2*xx[2]^2, xx[1], 
  -1 + 2*xx[1]^2}, {1, xx[2], -1 + 2*xx[2]^2, xx[1], -1 + 2*xx[1]^2}, 1}

\end{verbatim}



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

 
smolyakGenInterpData[
aVecFunc:(_Function|_CompiledFunction|_Symbol),@<smolGSpec@>]:=
Module[{},Print["smolyakGenInterpData"];
With[{filledPts=Map[
Function[xx,fillIn[{{},smolToIgnore,xx}]],N[smolPts]]},
With[{theVals=Map[checkEval[aVecFunc],filledPts]},
With[{interpData=Map[Flatten,theVals]},
interpData]]]]
 
smolyakGenInterpData[
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},@<smolGSpec@>]:=
Module[{},
With[{filledPts=Map[Function[xx,fillIn[{{},smolToIgnore,xx}]],N[smolPts]]},
With[{theVals=Table[evaluateTriple[aTriple,Flatten[aPt]],{aPt,filledPts},
{aTriple,triples[[1]]}]},
With[{interpData=
Catch[
Map[Apply[selectorFunc,#]&,{filledPts,theVals}//Transpose],
_,Function[{val,tag},Print["catchsmolGenInterp: aborting",
{val,tag,triples,filledPts}//InputForm];
Abort[]]]
},
interpData]]]]

defaultSelectorFunc=Function[{aPt,allRes},Print["default:",{aPt,allRes}];Flatten[DeleteCases[allRes,$Failed]]]


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





checkEval[aVecFunc_]:=
Catch[Function[xx,(Print["checkEval:",xx];Apply[aVecFunc,xx])],{$Failed,xx}]
 
parallelSmolyakGenInterpData[
aVecFunc:(_Function|_CompiledFunction|_Symbol),@<smolGSpec@>]:=
Module[{},
DistributeDefinitions[aVecFunc];
With[{filledPts=ParallelMap[
Check[Function[xx,fillIn[{{},smolToIgnore,xx}]],Print["aborting"];AbortKernels[];Throw["parallelSmolyakGenInterpData"]],N[smolPts]]},
With[{theVals=ParallelMap[Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Map[Flatten,theVals]},
interpData]]]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["smolyakGenInterpData"->#&,{"fillIn"}]];



(*end code for smolyakGenInterpData*)
@}
\begin{verbatim}
In[90]:= siData=smolyakGenInterpData[theFP,sgSpec]//InputForm

Out[90]//InputForm= 
{{0.2, 0.11428571428571428, 0., 0.}, {0.17, 0.09714285714285714, 
  5.421010797804037*^-19, -0.03}, {0.23, 0.13142857142857142, 
  -7.494005416736795*^-18, 0.03}, {0., 0., 0., 0.}, 
 {0.4, 0.22857142857142856, 0., 0.}}
\end{verbatim}

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



smolyakInterpolation[fVals:{{_?NumberQ..}..},@<smolGSpec@>,pMat_?MatrixQ]:=
With[{wtsProbs=applyWtsProbs[wts,thePolys,theIntPolys,pMat]},wtsProbs]

applyWtsProbs[wts_?MatrixQ,thePolys_?VectorQ,theIntPolys_?VectorQ,
pMat_?MatrixQ]:=
With[{allStates=Map[#.thePolys&,thePolys],
allStatesInt=Flatten[pMat .Transpose[{Map[#.thePolys&,theIntPolys]}]]},
{allStates,allStatesInt}]

polySubs[numVars_?NumberQ,numEps_?NumberQ]:=
With[{origXs=Table[xx[ii],{ii,numVars}],
theXs=Table[Unique["xx"],{ii,numVars}]},
With[{shortOrigXs=Drop[origXs,-numEps],
shortXs=Drop[theXs,-numEps]},
{Thread[origXs->theXs],Thread[shortOrigXs->shortXs]}]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["smolyakInterpolation"->#&,{"xformXValToCheb"}]];
@}


\subsection{svmRegressionLinear}
\label{sec:svmRegressionlinear}

The function returns both the level and the derivative approximating functions.

@d svmRegressionLinearUsage
@{svmRegressionLinear::usage=
"place holder for svmRegressionLinear"
@}

\begin{verbatim}
options:
-s svm_type : set type of SVM (default 0)
	0 -- C-SVC
	1 -- nu-SVC
	2 -- one-class SVM
	3 -- epsilon-SVR
	4 -- nu-SVR
-t kernel_type : set type of kernel function (default 2)
	0 -- linear: u'*v
	1 -- polynomial: (gamma*u'*v + coef0)^degree
	2 -- radial basis function: exp(-gamma*|u-v|^2)
	3 -- sigmoid: tanh(gamma*u'*v + coef0)
-d degree : set degree in kernel function (default 3)
-g gamma : set gamma in kernel function (default 1/num_features)
-r coef0 : set coef0 in kernel function (default 0)
-c cost : set the parameter C of C-SVC, epsilon-SVR, and nu-SVR (default 1)
-n nu : set the parameter nu of nu-SVC, one-class SVM, and nu-SVR (default 0.5)
-p epsilon : set the epsilon in loss function of epsilon-SVR (default 0.1)
-m cachesize : set cache memory size in MB (default 100)
-e epsilon : set tolerance of termination criterion (default 0.001)
-h shrinking: whether to use the shrinking heuristics, 0 or 1 (default 1)
-b probability_estimates: whether to train a SVC or SVR model for probability estimates, 0 or 1 (default 0)
-wi weight: set the parameter C of class i to weight*C, for C-SVC (default 1)

The k in the -g option means the number of attributes in the input data.
\end{verbatim}


@d svmRegressionLinear
@{
svmRegressionLinear[fVals:{_?NumberQ..},@<smolGSpec@>,svmArgs:{_?NumberQ,_?NumberQ},className_String]:=
Module[{svmtLinear = JavaNew["libsvm.trainGuts"],modLinear,
betterArgs=Table[Unique["arg"],{Length[smolRngs]}]},Print["svmRegressionLinea:",fVals];
svmtLinear[mmaUreadUproblemLinear[smolPts//N,fVals//N, svmArgs]];
modLinear = 
     libsvm`svm`svmUtrain[svmtLinear[prob],svmtLinear[param]];
Sow[huh=modLinear[svUindices],"svIndices"];Print["modLinear",modLinear]
linFunc=Function[xx,(libsvm`svm`mysvmUpredictUvalues[modLinear, xx,className] )];
expLinFunc=Function[xx,(libsvm`svm`mysvmUpredictUExpectedUvalues[modLinear, xx,className] )];Print["modLinear",{linFunc,expLinFunc}//InputForm]
{linFunc/.xx$->betterArgs,expLinFunc/.xx$->Drop[betterArgs,-numEps]}]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["svmRegressionLinear"->#&,{"xformXValToCheb"}]];
@}

\subsection{svmRegressionPoly}
\label{sec:smregressionpoly}

The function returns both the level and the derivative approximating functions.

@d svmRegressionPolyUsage
@{svmRegressionPoly::usage=
"place holder for svmRegressionPoly"
@}


@d svmRegressionPoly
@{
svmRegressionPoly[fVals:{_?NumberQ..},@<smolGSpec@>,svmArgs:{_?NumberQ,_?NumberQ,_?NumberQ,_?NumberQ,_?NumberQ},className_String]:=
Module[{svmtPoly = JavaNew["libsvm.trainGuts"],modPoly,
betterArgs=Table[Unique["arg"],{Length[smolRngs]}]},
svmtPoly[mmaUreadUproblemPoly[smolPts//N,fVals//N, svmArgs]];
modPoly = 
     libsvm`svm`svmUtrain[svmtPoly[prob],svmtPoly[param]];
Sow[huh=modPoly[svUindices],"svIndices"];
polyFunc=Function[xx,(libsvm`svm`mysvmUpredictUvalues[modPoly, xx,className] )];
expPolyFunc=Function[xx,(libsvm`svm`mysvmUpredictUExpectedUvalues[modPoly, xx,className] )];
{polyFunc/.xx$->betterArgs,expPolyFunc/.xx$->Drop[betterArgs,-numEps]}]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["svmRegressionPoly"->#&,{"xformXValToCheb"}]];
@}

\subsection{svmRegressionRBF}
\label{sec:smregressionrbf}

The function returns both the level and the derivative approximating functions.

@d svmRegressionRBFUsage
@{svmRegressionRBF::usage=
"place holder for svmRegressionRBF"
@}


@d svmRegressionRBF
@{
svmRegressionRBF[fVals:{_?NumberQ..},@<smolGSpec@>,svmArgs:{_?NumberQ,_?NumberQ,_?NumberQ},className_String]:=
Module[{svmtRBF = JavaNew["libsvm.trainGuts"],modRBF,
betterArgs=Table[Unique["arg"],{Length[smolRngs]}]},
svmtRBF[mmaUreadUproblemRBF[smolPts//N,fVals//N, svmArgs]];
modRBF = 
     libsvm`svm`svmUtrain[svmtRBF[prob],svmtRBF[param]];
Sow[huh=modRBF[svUindices],"svIndices"];
rbfFunc=Function[xx,(libsvm`svm`mysvmUpredictUvalues[modRBF, xx,className] )];
expRBFFunc=Function[xx,(libsvm`svm`mysvmUpredictUExpectedUvalues[modRBF, xx,className] )];
{rbfFunc/.xx$->betterArgs,expRBFFunc/.xx$->Drop[betterArgs,-numEps]}]

@}

\subsection{svmRegressionSigmoid}
\label{sec:smregressionsigmoid}

The function returns both the level and the derivative approximating functions.

@d svmRegressionSigmoidUsage
@{svmRegressionSigmoid::usage=
"place holder for svmRegressionSigmoid"
@}


@d svmRegressionSigmoid
@{
svmRegressionSigmoid[fVals:{_?NumberQ..},@<smolGSpec@>,svmArgs:{_?NumberQ,_?NumberQ,_?NumberQ,_?NumberQ},className_String]:=
Module[{svmtSigmoid = JavaNew["libsvm.trainGuts"],modSigmoid,
betterArgs=Table[Unique["arg"],{Length[smolRngs]}]},
svmtSigmoid[mmaUreadUproblemSigmoid[smolPts//N,fVals//N, svmArgs]];
modSigmoid = 
     libsvm`svm`svmUtrain[svmtSigmoid[prob],svmtSigmoid[param]];
Sow[huh=modSigmoid[svUindices],"svIndices"];
sigmoidFunc=Function[xx,(libsvm`svm`mysvmUpredictUvalues[modSigmoid, xx,className] )];
expSigmoidFunc=Function[xx,(libsvm`svm`mysvmUpredictUExpectedUvalues[modSigmoid, xx,className] )];
{sigmoidFunc/.xx$->betterArgs,expSigmoidFunc/.xx$->Drop[betterArgs,-numEps]}]

@}

@d expectation templates
@{
expKernTop=
"package forImport;
//``
import libsvm.PRECOMPUTED;
public class `` extends PRECOMPUTED {
public double [] testExpKern(double[] xNow)   {
   double [] theExpVals = new double[``];
``
``
   return(theExpVals);
} 
public double [] testKern(double[] xNow)   {
   double [] theVals = new double[``];
``
``
   return(theVals);
} 
public double [][] xVals(){
double[][]xRes=
``;
return(xRes);
}
}


"

expKernBottom=""



@}

@d writeExpKern
@{
writeExpKern[theFile_String,expEqns_List,
expCode_String,expDefines_String,kernCode_String,kernDefines_String,
theX_?MatrixQ]:=
Module[{javaFile="forImport/"<>theFile<>".java",
xRows=Length[theX],xCols=Length[theX[[1]]],kernDim=Length[theKern]},
With[{theClassCode=TemplateApply[expKernTop,
{DateString[],theFile,
Length[expEqns],expDefines,expCode,
Length[expEqns],kernDefines,kernCode,
theX,
theKern}]},Print[{javaFile,theClassCode,expKernBottom}];
WriteString[javaFile,theClassCode];
WriteString[javaFile,expKernBottom];
       Close[javaFile]]]

@<expectation templates@>


writePRECOMPUTEMatFile[theFile_String,theKern_?MatrixQ]:=
Module[{mlDouble,kernRows=Length[theKern]},
mlDouble=JavaNew["com.jmatio.types.MLDouble","kern_arr",
Flatten[Transpose[theKern]],kernRows];
aList=JavaNew["java.util.ArrayList"];
aList[add[mlDouble]];
matWriter=JavaNew["com.jmatio.io.MatFileWriter",theFile,aList]]




@}

@d theExpValsUsage
@{
theExpVals::usage="place holder"
@}
@d xNowUsage
@{
xNow::usage="place holder"
@}


@d writeExpKernUsage
@{
writeExpKern::usage="writeExpKern[theFile_String]"
writePRECOMPUTEMatFile::usage="writePRECOMPUTEMatFile[theFile_String,theKern_?MatrixQ]"

@}

@d ExpKernCode
@{
cnstrctExpKern[xData_?MatrixQ,
theKernel_Function,CC_?NumberQ,epsilon_?NumberQ,@<distribSpec@>]:=
With[{numEps=Length[expctSpec]},
With[{xvars=Transpose[{genXVars[Length[xData]-numEps]}],
epsvars=Map[{First[#]}&,expctSpec],
intVars=Map[#[[1]] \[Distributed]#[[2]]&,expctSpec]},
With[{numExamples=Length[xData[[1]]],
xSubs=genXSubs[xvars],epsSubs=genEpsSubs[xvars,epsvars]},
With[{kernelPart=Flatten[Map[theKernel[#,Flatten[Join[xvars,epsvars]]]&,
Transpose[xData]]]},
With[{expVals=myExpectation[kernelPart,intVars]},
With[{expCode=StringReplace[
myCAssign[theExpVals,expVals,AssignEnd->";\n",
AssignOptimize->True,OptimizationSymbol -> okay],
Join[xSubs,{"pow("->"Math.pow("}]],
kernCode=StringReplace[
myCAssign[theVals,kernelPart,AssignEnd->";\n",
AssignOptimize->True,OptimizationSymbol -> okay],
Join[xSubs,epsSubs,{"pow("->"Math.pow("}]]
},
{expVals,expCode,genDefines[expCode],kernelPart,kernCode,genDefines[kernCode]}]]]]]]/;
And[Length[xData]>0,Length[xData[[1]]]>0]


cnstrctF[qpSubs_List,xData_?MatrixQ,yData_?VectorQ,
theKernel_Function,CC_?NumberQ,epsilon_?NumberQ]:=
With[{xvars=Transpose[{genXVars[Length[xData]]}],
numExamples=Length[xData[[1]]]},
With[{pVars=genPlusVars[numExamples],mVars=genMinusVars[numExamples]},
With[{kernelPart=
Flatten[Map[
theKernel[Transpose[{#}],xvars]&,Transpose[xData]]]},
	 Function @@{Flatten[xvars],(-1)*((pVars-mVars)/.qpSubs) . kernelPart+
approxB[qpSubs,xData,yData,theKernel,CC,epsilon]}]]]/;
And[Length[xData]>0,Length[xData[[1]]]>0]



approxB[qpSubs_List,xData_?MatrixQ,yData_?VectorQ,
theKernel_Function,CC_?NumberQ,epsilon_?NumberQ]:=
With[{minusBs=(Flatten[
yData[[onMinusBoundaryTube[qpSubs,CC]]] +
(Map[(wExprn[xData,theKernel,Transpose[{#}]]/.qpSubs)&,
Transpose[xData[[All,onMinusBoundaryTube[qpSubs,CC]]]]])+
epsilon]),
plusBs=(Flatten[
yData[[onPlusBoundaryTube[qpSubs,CC]]] +
(Map[(wExprn[xData,theKernel,Transpose[{#}]]/.qpSubs)&,
Transpose[xData[[All,onPlusBoundaryTube[qpSubs,CC]]]]])-
epsilon])},
	 With[{finite=DeleteCases[{Max[minusBs],Min[plusBs]},Infinity|-Infinity]},If[Length[finite]==0,0,(Plus @@finite)/Length[finite]]]]


wExprn[xData_?MatrixQ]:=
With[{numExamples=Length[xData[[1]]]},
With[{pVars=genPlusVars[numExamples],mVars=genMinusVars[numExamples]},
(pVars-mVars).Transpose[xData]]]/;
And[Length[xData]>0,Length[xData[[1]]]>0]


wExprn[xData_?MatrixQ,kernelFunc_Function,particularX_?MatrixQ]:=
With[{numExamples=Length[xData[[1]]]},
With[{pVars=genPlusVars[numExamples],mVars=genMinusVars[numExamples]},
Flatten[
	(pVars-mVars). (Map[kernelFunc[Transpose[{#1}],particularX]&,Transpose[xData]])	
]]]/;
And[Length[xData]>0,Length[xData[[1]]]>0]




genXSubs[xVars_List]:=
MapIndexed[ToString[#]->"xNow["<>ToString[#2[[1]]-1]<>"]"&,Flatten[xVars]]
genEpsSubs[xVars_List,epsVars_List]:=
MapIndexed[StringReplace[ToString[#],{"`"->"_"}]->"xNow["<>ToString[#2[[1]]-1+Length[xVars]]<>"]"&,Flatten[epsVars]]


genXVars[numFeatures_Integer]:=
Table[
Symbol["x$"<>ToString[ii]],
{ii,numFeatures}]

genEpsVars[numFeatures_Integer]:=
Table[
Symbol["eps$"<>ToString[ii]],
{ii,numFeatures}]

cnstrctXYPRECOMPUTEKern[xData_?MatrixQ,theKernel_Function]:=
With[{kernRes=Outer[theKernel,xData,xData,1]},
kernRes]



dotProdKernel=Function[{xx,yy},xx . yy];



makeSymbolicKernel[]:=
With[{myX=Unique["xx"],myY=Unique["yy"]},
	 Apply[Function,{{myX,myY},ffff[myX,myY]}]]

makePolynomialKernel[aPow_Integer]:=
With[{myX=Unique["xx"],myY=Unique["yy"]},
Apply[Function,{{myX,myY},(1+myX . myY)^aPow}]]

makeRBFKernel[aRadius_Integer]:=
Function[{myX,myY},E^(-Outer[Norm[Plus[#1,#2]]&,myX,-myY,1]/(2*(aRadius^2)))]

@}
@d makeSymbolicKernelUsage
@{
makeSymbolicKernel::usage="place holder"
@}
@d dotProdKernelUsage
@{
dotProdKernel::usage="place holder"
@}
@d makePolynomialKernelUsage
@{
makePolynomialKernel::usage="place holder"
@}
@d makeRBFKernelUsage
@{
makeRBFKernel::usage="place holder"
@}

@d cnstrctFUsage
@{
cnstrctF::usage="cnstrctF[qpSubs_List,xData_?MatrixQ,yData_?VectorQ,theKernel_Function,CC_?NumberQ,epsilon_?NumberQ]"
@}

@d cnstrctExpKernUsage
@{
cnstrctExpKern::usage="cnstrctExpKern[xData_?MatrixQ,yData_?VectorQ,theKernel_Function,CC_?NumberQ,epsilon_?NumberQ,numEps_Integer]"

cnstrctXYPRECOMPUTEKern::usage="cnstrctXYPRECOMPUTEKern[xData_?MatrixQ,theKernel_Function]"

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





AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["smolyakInterpolationPrep"->#&,
{"sparseGridEvalPolysAtPts","xformToXVec"}]];

newSmolyakInterpolationPrep[approxLevels_?listOfIntegersQ,smolRngs_?MatrixQ,
momSubs:{{(_->_)..}...}]:=
Module[{smolRes=sparseGridEvalPolysAtPts[approxLevels],
numVars=Length[approxLevels],numEps=Length[momSubs]},
With[{thePts=smolRes[[1]],smolPolys=smolRes[[2]],smolMat=smolRes[[3]]},
With[{xPts=Map[Function[xx,xformToXVec[xx,smolRngs]],thePts]},
With[{numPolys=Length[smolPolys]},
{xPts,smolMat,smolPolys,compRawMoments[smolPolys,xx[3]]/.
Flatten[momSubs]}]]]]/;
And[Length[smolRngs]==Length[approxLevels]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["newSmolyakInterpolationPrep"->#&,
{"sparseGridEvalPolysAtPts","xformToXVec","compRawMoments"}]];

compRawMoments[expr_,errVar:anX_Symbol[ii_Integer]]:=
expr/.{errVar^nn_Integer->mom[ii,nn],errVar->mom[ii,1]}



xformXValToCheb[xVal_,
range:{lowVal_?NumberQ,highVal_?NumberQ}]:=
xFormToChebInterval[xVal,lowVal,highVal]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["xformXValToCheb"->#&,
{"xFormToChebInterval"}]];



xformChebValToX[chebVal_,
range:{lowVal_?NumberQ,highVal_?NumberQ}]:=
xFormFromChebInterval[chebVal,lowVal,highVal]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["xformChebValToX"->#&,
{"xFormFromChebInterval"}]];


xformToXVec[chebPt_?VectorQ,ranges_?MatrixQ]:=
MapThread[xformChebValToX,{chebPt,ranges}]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["xformToXVec"->#&,
{"xformChebValToX"}]];

@}


\subsection{parallelSmolyakGenInterpData}
\label{sec:geninterpdata}



@d parallelSmolyakGenInterpDataUsage
@{parallelSmolyakGenInterpData::usage=
"place holder for parallelSmolyakGenInterpData"
@}

@d parallelSmolyakGenInterpData
@{
(*begin code for parallelSmolyakGenInterpData*)






  
AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["parallelSmolyakGenInterpData"->#&,
{"fillIn"}]];

(*end code for parallelSmolyakGenInterpData*)
@}



\subsection{makeSmolyakInterpFuncs}
\label{sec:makesmolinterpfunc}




@d makeSmolyakInterpFuncsUsage
@{makeSmolyakInterpFuncs::usage=
"place holder for makeSmolyakInterpFuncs"
@}

@d makeSmolyakInterpFuncs
@{
(*begin code for makeSmolyakInterpFuncs*)


makeSmolyakInterpFuncs[aVecFunc:(_Function|_CompiledFunction|_Symbol),@<smolGSpec@>]:=
With[{interpData=smolyakGenInterpData[aVecFunc,smolGSpec],
numArgs=Length[smolPts[[1]]]},
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["f01Args"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,
With[{theInterps=smolyakInterpolation[interpData[[All,funcIdx]],smolGSpec]},
With[{smolApp=theInterps},
smolApp]]],Range[numFuncs]]},
With[
{applied=
Transpose[{Through[Apply[Map[First,interpFuncList],funcArgs]]}],
appliedExp=
Transpose[{Through[Apply[Map[Last,interpFuncList],funcArgs]]}]},
{
ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}],
ReplacePart[
	Function[xxxxxxx, appliedExp],
		{1->Drop[longFuncArgs,-numEps]}]
}
	]
]]]]


makeSmolyakInterpFuncs[triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},@<smolGSpec@>]:=
With[{interpData=smolyakGenInterpData[triples,smolGSpec],
numArgs=Length[smolPts[[1]]]},
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["f01Args"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,
With[{theInterps=smolyakInterpolation[interpData[[All,funcIdx]],smolGSpec]},
With[{smolApp=theInterps},
smolApp]]],Range[numFuncs]]},
With[
{applied=
Transpose[{Through[Apply[Map[First,interpFuncList],funcArgs]]}],
appliedExp=
Transpose[{Through[Apply[Map[Last,interpFuncList],funcArgs]]}]},
{
ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}],
ReplacePart[
	Function[xxxxxxx, appliedExp],
		{1->Drop[longFuncArgs,-numEps]}]
}
	]
]]]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["makeSmolyakInterpFuncs"->#&,{"smolyakInterpolation","smolyakGenInterpData","fillInSymb"}]];
(*end code for makeSmolyakInterpFuncs*)
@}

\subsection{makeGenericInterpFuncs}
\label{sec:makesmolinterpfunc}




@d makeGenericInterpFuncsUsage
@{
parallelMakeGenericInterpFuncs::usage=
"place holder for makeGenericInterpFuncs";
makeGenericInterpFuncs::usage=
"place holder for makeGenericInterpFuncs"
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



parallelMakeGenericInterpFuncs[triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
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


replaceEqnOrExp[vecFunc_Function,theVars_List,indx_Integer,
backLookingInfo:{{_Integer,_,_}...}]:=
With[{theRes=Map[Function[uu,{uu[[1]],Apply[uu[[indx]],
Flatten[theVars]]}],backLookingInfo]},
Fold[ReplacePart[#1,{2,#2[[1]]}->#2[[2]]]&,vecFunc,theRes]]


makeSubs[thisFunc_Function,someArgs_List]:=
MapThread[#1->#2&,{thisFunc[[1]],someArgs}]





AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["makeGenericInterpFuncs"->#&,{"smolyakInterpolation","smolyakGenInterpData","fillInSymb"}]];



(*end code for makeGenericInterpFuncs*)
@}


\subsection{doSmolyakIterREInterp}
\label{sec:doiterreinterp}



@d doSmolyakIterREInterpUsage
@{doSmolyakIterREInterp::usage=
"place holder for doSmolyakIterREInterp"
@}

@d doSmolyakIterREInterp
@{
(*begin code for doSmolyakIterREInterp*)

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["doSmolyakIterREInterp"->#&,
{"makeSmolyakInterpFuncs","genFPFunc","getNumX",
"getNumEps","getNumZ","genXZREInterpFunc"}]];



(*end code for doSmolyakIterREInterp*)
@}


\subsection{genSmolyakXZREInterpFunc}
\label{sec:genxzreinterpfunc}


@d genSmolyakXZREInterpFuncUsage
@{genSmolyakXZREInterpFunc::usage=
"place holder for genSmolyakXZREInterpFunc"
@}

@d genSmolyakXZREInterpFunc
@{
(*begin code for genSmolyakXZREInterpFunc*)
 
genSmolyakXZREInterpFunc[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,
toIgnore:{_Integer...},ranges_?MatrixQ,
smolPts_?MatrixQ,smolMat_?MatrixQ,
smolPolys_?VectorQ,@<gSpec@>,@<distribSpec@>]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
makeInterpFunc[theFuncNow,elimGSpecShocks[gSpec,numEps]]]
  

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["genSmolyakXZREInterpFunc"->#&,
{"genXZFuncRE","makeInterpFunc","elimGSpecShocks"}]];


elimGSpecShocks[@<gSpec@>,numEps_Integer]:=
{toIgnore,gSpec[[2]],Drop[getGridPtTrips[gSpec],-(numEps)]}

elimSmolGSpecShocks[@<smolGSpec@>,numEps_Integer]:=
Module[{},
smolRngsNow=Drop[smolRngs,-numEps];
approxLevelsNow=Drop[approxLevels,-numEps];
{pt,tf,ply}=smolyakInterpolationPrep[approxLevelsNow,smolRngsNow];
{smolToIgnore,smolRngsNow,pt,tf,ply,{},0,approxLevelsNow}]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["elimGSpecShocks"->#&,{"getGridPtTrips"}]];



(*end code for genSmolyakXZREInterpFunc*)
@}



\subsection{nestSmolyakIterREInterp}
\label{sec:nestiterreinterp}


@d nestSmolyakIterREInterpUsage
@{nestSmolyakIterREInterp::usage=
"place holder for nestSmolyakIterREInterp"
@}

@d nestSmolyakIterREInterp
@{
(*begin code for nestSmolyakIterREInterp*)



AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["nestIterREInterp"->#&,{"doSmolyakIterREInterp"}]];



(*end code for nestSmolyakIterREInterp*)
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

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["fillIn"->#&,{"fillIn"}]];

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

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["fillInSymb"->#&,{"fillInSymb"}]];

(*end code for fillInSymb*)
@}


\section{Interpolation Functions}
\label{sec:interp-funct}


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

genInterpData[triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},@<gSpec@>]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=Map[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=Table[evaluateTriple[aTriple,Flatten[aPt]],{aPt,filledPts},
{aTriple,triples[[1]]}]},
With[{interpData={thePts,theVals}},
MapThread[{#1,#2}&,{interpData[[1]],interpData[[2,1]]}]]]]]



parallelGenInterpData[triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},@<gSpec@>]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=Map[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=ParallelTable[evaluateTriple[aTriple,Flatten[aPt]],
{aPt,filledPts},{aTriple,triples[[1]]}]},
With[{interpData={thePts,Map[Flatten,Flatten[Map[DeleteCases[#,$Failed]&,Transpose[theVals]],1]]}},
MapThread[{#1,#2}&,{interpData[[1]],interpData[[2,1]]}]]]]]



AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genInterpData"->#&,{"gridPts","fillIn"}]];



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

 
parallelGenInterpData[aVecFunc:(_Function|_CompiledFunction|_Symbol),@<gSpec@>]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=ParallelMap[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=ParallelMap[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]



AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["parallelGenInterpData"->#&,{"gridPts","fillIn"}]];


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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["gridPts"->#&,{"oneDimGridPts"}]];


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


\subsection{makeInterpFunc}
\label{sec:makeinterpfunc}

Constructs a function that for a given index returns a function of 
 that yields a column vector of values 
\begin{gather*}
\begin{bmatrix}
  x_t\\z_t
\end{bmatrix}= f \left (
\begin{bmatrix}
x_{t-1} \\ \epsilon_t  
\end{bmatrix}\right )
\end{gather*}

the Takes a vector function and a grid point specification


@d makeInterpFuncUsage
@{makeInterpFunc::usage=
"place holder for makeInterpFunc"
@}

@d makeInterpFunc
@{
(*begin code for makeInterpFunc*)

makeInterpFunc[aVecFunc:(_Function|_CompiledFunction|_Symbol),@<gSpec@>]:=
With[{interpData=genInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["f04Args"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,Interpolation[
Map[Function[xx,{xx[[1]], xx[[2, funcIdx, 1]]}] , 
		interpData],InterpolationOrder -> iOrd]],Range[numFuncs]]},
		With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
	ReplacePart[
	Function[xxxxxxx, applied],
		{1->Drop[longFuncArgs]}]
	]
]]]]


makeInterpFunc[triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},@<gSpec@>]:=
With[{interpData=genInterpData[triples,gSpec],numArgs=getNumVars[gSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["f04Args"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,Interpolation[
Map[Function[xx,{xx[[1]], xx[[2, funcIdx, 1]]}] , 
		interpData],InterpolationOrder -> iOrd]],Range[numFuncs]]},
		With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
	ReplacePart[
	Function[xxxxxxx, applied],
		{1->Drop[longFuncArgs]}]
	]
]]]]


smolInterpToGrid[smolInterp_?MatrixQ,@<smolGSpec@>]:=
Transpose[{smolPts,Map[Transpose[{#}]&,smolInterp]}]




makeInterpFunc[aVecFunc:(_Function|_CompiledFunction|_Symbol),@<smolGSpec@>]:=
With[{sinterpData=smolyakGenInterpData[aVecFunc,smolGSpec],
numArgs=getNumVars[smolGSpec]},
With[{interpData=smolInterpToGrid[sinterpData,smolGSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["f05Args"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,Interpolation[
Map[Function[xx,{xx[[1]], xx[[2, funcIdx, 1]]}] , 
		interpData],InterpolationOrder -> 1]],Range[numFuncs]]},
		With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
	ReplacePart[
	Function[xxxxxxx, applied],
		{1->Drop[longFuncArgs]}]
	]
]]]]]

makeInterpFunc[triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},@<smolGSpec@>]:=
With[{sinterpData=smolyakGenInterpData[triples,smolGSpec],
numArgs=getNumVars[smolGSpec]},
With[{interpData=smolInterpToGrid[sinterpData,smolGSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["f05Args"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,Interpolation[
Map[Function[xx,{xx[[1]], xx[[2, funcIdx, 1]]}] , 
		interpData],InterpolationOrder -> 1]],Range[numFuncs]]},
		With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
	ReplacePart[
	Function[xxxxxxx, applied],
		{1->Drop[longFuncArgs]}]
	]
]]]]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["parallelMakeInterpFunc"->#&,{"fillInSymb","genInterpData","fillInSymb","getNumVars"}]];


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

parallelMakeInterpFunc[aVecFunc:(_Function|_CompiledFunction|_Symbol),@<gSpec@>]:=
Module[{},
With[{interpData=parallelGenInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["f10Args"],{numArgs}]},
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
]]]]]


parallelMakeInterpFunc[triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},@<gSpec@>]:=
Module[{},
With[{interpData=parallelGenInterpData[triples,gSpec],numArgs=getNumVars[gSpec]},
With[{numFuncs=Length[interpData[[1,2]]],
funcArgs=Table[Unique["f10Args"],{numArgs}]},
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
]]]]]


parallelMakeInterpFunc[triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),
_Function}..},selectorFunc_Function},backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>]:=
Module[{},
parallelMakeGenericInterpFuncs[triples,backLookingInfo,smolGSpec,smolyakInterpolation,{}]]


parallelMakeInterpFunc[aVecFunc:(_Function|_CompiledFunction|_Symbol),backLookingInfo:{{_Integer,_,_}...},@<smolGSpec@>]:=
Module[{},
parallelMakeGenericInterpFuncs[aVecFunc,backLookingInfo,smolGSpec,smolyakInterpolation,{}]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["parallelMakeInterpFunc"->#&,{"fillInSymb","parallelGenInterpData","fillInSymb","getNumVars"}]];


(*end code for parallelMakeInterpFunc*)
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





AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["doIterREInterp"->#&,{"makeInterpFunc","genFPFunc","getNumX","getNumEps","getNumZ","parallelGenXZREInterpFunc"}]];


(*end code for doIterREInterp*)
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


Options[parallelDoIterREInterp]={"xVarRanges"->{},"Traditional"->False}

parallelDoIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<gSpec@>,@<distribSpec@>,opts:OptionsPattern[]]:=
With[{numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[XZFuncs[[1]]];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,eqnsFunc,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theDRFuncs=parallelMakeInterpFunc[reapRes[[1]],gSpec]},
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{XZRE=parallelGenXZREInterpFunc[{numX,numEps,numZ},theDRFuncs,gSpec,distribSpec]},
{theDRFuncs,XZRE}]]]

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
parallelDoIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},selectorFunc_Function},@<gSpec@>,@<distribSpec@>,opts:OptionsPattern[]]:=
With[{numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[XZFuncs[[1]]];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];Print["parallelDoIterREInterp:"];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,triples,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theDRFuncs=parallelMakeInterpFunc[reapRes[[1]],gSpec]},
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{XZRE=parallelGenXZREInterpFunc[{numX,numEps,numZ},theDRFuncs,gSpec,distribSpec]},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
{theDRFuncs,XZRE}]]]

parallelDoIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<XZFuncs@>,triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},selectorFunc_Function},@<smolGSpec@>,opts:OptionsPattern[]]:=
With[{numX=getNumX[linMod],lclnumEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[XZFuncs[[1]]];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,XZFuncs,triples,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,smolyakInterpolation,{}]},
Print["done parallelmakegenericinterpfunc:"];
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
theFuncs]]]

parallelDoIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<bothXZFuncs@>,
@<eqnsFunc@>,@<gSpec@>,@<distribSpec@>,opts:OptionsPattern[]]:=
With[{numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[justBothXZFuncs];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,eqnsFunc,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theDRFuncs=parallelMakeInterpFunc[reapRes[[1]],gSpec]},
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{XZRE=parallelGenXZREInterpFunc[{numX,numEps,numZ},theDRFuncs,gSpec,distribSpec]},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
{theDRFuncs,XZRE}]]]

parallelDoIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<bothXZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,opts:OptionsPattern[]]:=
With[{numX=getNumX[linMod],lclnumEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[justBothXZFuncs];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];
Print["done distributing defs:"];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,eqnsFunc,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,smolyakInterpolation,{}]},
Print["done parallelmakegenericinterpfunc:"];
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
theFuncs]]]



Options[parallelDoIterREInterp]={"xVarRanges"->{},"Traditional"->False}
parallelDoIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<bothXZFuncs@>,triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},selectorFunc_Function},@<gSpec@>,@<distribSpec@>,opts:OptionsPattern[]]:=
With[{numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[justBothXZFuncs];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];Print["parallelDoIterREInterp:"];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,triples,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theDRFuncs=parallelMakeInterpFunc[reapRes[[1]],gSpec]},
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{XZRE=parallelGenXZREInterpFunc[{numX,numEps,numZ},theDRFuncs,gSpec,distribSpec]},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
{theDRFuncs,XZRE}]]]

parallelDoIterREInterp[genFRExtFunc,
	@<linMod@>,
	@<bothXZFuncs@>,triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},selectorFunc_Function},@<smolGSpec@>,opts:OptionsPattern[]]:=
With[{numX=getNumX[linMod],lclnumEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[justBothXZFuncs];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];
If[Length[Kernels[]]===0,LaunchKernels[]];reapRes=Reap[
genFRExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,triples,Apply[Sequence,FilterRules[{opts},Options[genFRExtFunc]]]],"theFuncs"];Apply[DistributeDefinitions,Flatten[reapRes[[2]]]];
With[{theFuncs=parallelMakeGenericInterpFuncs[reapRes[[1]],backLookingInfo,smolGSpec,smolyakInterpolation,{}]},
Print["done parallelmakegenericinterpfunc:"];
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
theFuncs]]]





AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["parallelDoIterREInterp"->#&,{"parallelMakeInterpFunc","genFPFunc","getNumX","getNumEps","getNumZ","parallelGenXZREInterpFunc"}]];


(*end code for parallelDoIterREInterp*)
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




AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["nestIterREInterp"->#&,{"doIterREInterp"}]];


(*end code for nestIterREInterp*)
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





Options[parallelNestIterREInterp]={"xVarRanges"->{},"Traditional"->False}

parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},selectorFunc_Function},
@<gSpec@>,
@<distribSpec@>,numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},triples,gSpec,distribSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],{ig,XZFuncs[[1]]},numIters]]

parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},selectorFunc_Function},
@<smolGSpec@>,
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},triples,smolGSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],{ig,XZFuncs[[1]]},numIters]]


parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,
@<gSpec@>,
@<distribSpec@>,numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},eqnsFunc,gSpec,distribSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],{ig,XZFuncs[[1]]},numIters]]


parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,
@<smolGSpec@>,
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],{ig,XZFuncs[[1]]},numIters]]

parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<bothXZFuncs@>,triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},selectorFunc_Function},
@<gSpec@>,
@<distribSpec@>,numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx,numSteps},triples,gSpec,distribSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],justBothXZFuncs,numIters]]

parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<bothXZFuncs@>,triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},selectorFunc_Function},
@<smolGSpec@>,
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx,numSteps},triples,smolGSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],justBothXZFuncs,numIters]]


parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<bothXZFuncs@>,@<eqnsFunc@>,
@<gSpec@>,
@<distribSpec@>,numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx,numSteps},eqnsFunc,gSpec,distribSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],justBothXZFuncs,numIters]]


parallelNestIterREInterp[genFRExtFunc,@<linMod@>,
@<bothXZFuncs@>,@<eqnsFunc@>,
@<smolGSpec@>,
numIters_Integer,opts:OptionsPattern[]]:=
Module[{},
NestList[Function[xx,parallelDoIterREInterp[genFRExtFunc,linMod,
{xx,numSteps},eqnsFunc,smolGSpec,Apply[Sequence,FilterRules[{opts},Options[parallelDoIterREInterp]]]]],justBothXZFuncs,numIters]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["parallelNestIterREInterp"->#&,{"parallelDoIterREInterp"}]];



(*end code for parallelNestIterREInterp*)
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
  
 
genXZREInterpFunc[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,@<smolGSpec@>,@<distribSpec@>]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
makeInterpFunc[theFuncNow,elimSmolGSpecShocks[smolGSpec,numEps]]]
  


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genXZREInterpFunc"->#&,{"genXZFuncRE","makeInterpFunc","elimGSpecShocks"}]];

(*end code for genXZREInterpFunc*)
@}

\subsection{truncErrorMat}
@d truncErrorMatUsage
@{
truncErrorMat::usage="truncErrorMat=Compile[{{fmat,_Real,2},{phimat,_Real,2},{kk,_Integer}}"

@}

@d truncErrorMat
@{
truncErrorMat=
Compile[{{fmat,_Real,2},{phimat,_Real,2},{kk,_Integer}},
With[{dim=Length[fmat]},
If[kk==0,Inverse[IdentityMatrix[dim] - fmat].phimat,
Inverse[IdentityMatrix[dim] - fmat] . MatrixPower[fmat,kk].phimat]]]
@}

\subsection{parallelGenXZREInterpFunc}
\label{sec:genxzreinterpfunc}


@d parallelGenXZREInterpFuncUsage
@{parallelGenXZREInterpFunc::usage=
"place holder for parallelGenXZREInterpFunc"
@}

@d parallelGenXZREInterpFunc
@{
(*begin code for parallelGenXZREInterpFunc*)
 
parallelGenXZREInterpFunc[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,@<gSpec@>,@<distribSpec@>]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
parallelMakeInterpFunc[theFuncNow,elimGSpecShocks[gSpec,numEps]]]

  
AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["parallelGenXZInterpFunc"->#&,{"genXZFuncRE","parallelMakeInterpFunc","elimGSpecShocks"}]];



AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["elimGSpecShock"->#&,{"getGridPtTrips"}]];


(*end code for parallelGenXZREInterpFunc*)
@}



\section{Package  Definition}
\label{sec:function-definitions}



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




\appendix

\section{Usage Definitions}
\label{sec:usage-definitions}



@d usage definitions
@{
(*Begin Usage Definitions*)
PerfectForesight::usage="degenerate distribution implementing perfect foresight"
@<truncErrorMatUsage@>
@<makeGenericInterpFuncsUsage@>
@<cnstrctFUsage@>
@<xNowUsage@>
@<theExpValsUsage@>
@<getterSetterTestsUsage@>
@<callGraphUsage@>
@<smolyakInterpolationUsage@>
@<smolyakInterpolationPrepUsage@>
@<genLilXkZkFuncUsage@>
@<gettersSettersUsage@>
@<doFuncArgUsage@>
@<iterateDRPFUsage@>
@<genMaxExtFuncUsage@>
@<genNSExtFuncUsage@>
@<makeREIterFuncUsage@>
@<myNExpectationUsage@>
@<getDistribsUsage@>
@<genXZFuncREUsage@>
@<genIntVarsUsage@>

@}


@d dispose
@{
@<svmRegressionSigmoid@>
@<svmRegressionRBF@>
@<svmRegressionLinear@>
@<svmRegressionPoly@>

@<pathErrsDRREIntegrate@>
@<pathErrsDRPF@>
@<pathErrsDRPFUsage@>
@<pathErrsDRREIntegrateUsage@>
@<svmRegressionSigmoidUsage@>
@<svmRegressionRBFUsage@>
@<makeRBFKernelUsage@>
@<makePolynomialKernelUsage@>
@<makeSymbolicKernelUsage@>
@<dotProdKernelUsage@>
@<cnstrctExpKernUsage@>
@<svmRegressionLinearUsage@>
@<svmRegressionPolyUsage@>


@<worstPathForErrDRREIntegrate@>
@<evalBadPathErrDRREIntegrate@>
@<evalPathErrDRREIntegrate@>

@<worstPathForErrDRREIntegrateUsage@>
@<evalBadEulerErrDRREIntegrateUsage@>
@<evalBadPathErrDRREIntegrateUsage@>
@<evalPathErrDRREIntegrateUsage@>

@}


@d usage definitions
@{


@<parallelGenXZREInterpFuncUsage@>
@<genSmolyakXZREInterpFuncUsage@>
@<genXZREInterpFuncUsage@>
@<genX0Z0FuncsUsage@>
@<genBothX0Z0FuncsUsage@>
@<genX0Z0DrvFuncsUsage@>
@<genFRExtFuncUsage@>
@<genMaxExtFuncUsage@>
@<genNSExtFuncUsage@>
@<genFPFuncUsage@>
@<myFixedPointUsage@>
@<getHUsage@>
@<getBUsage@>
@<getFUsage@>
@<getGridPtTripsUsage@>
@<getNumVarsUsage@>
@<makeSmolyakInterpFuncsUsage@>
@<parallelMakeInterpFuncUsage@>
@<makeInterpFuncUsage@>
@<parallelSmolyakGenInterpDataUsage@>
@<smolyakGenInterpDataUsage@>
@<parallelGenInterpDataUsage@>
@<genInterpDataUsage@>
@<oneDimGridPtsUsage@>

@}
@d usage definitions
@{

@<gridPtsUsage@>
@<fillInUsage@>
@<fillInSymbUsage@>
@<parallelNestIterREInterpUsage@> 
@<parallelDoIterREInterpUsage@> 
@<nestIterREInterpUsage@>
@<doIterREInterpUsage@> 
@<nestSmolyakIterREInterpUsage@>
@<doSmolyakIterREInterpUsage@>
@<getPhiUsage@>
@<getPsiZUsage@>
@<getPsiCUsage@>
@<getPsiEpsUsage@>
@<getNumZUsage@>
@<getNumXUsage@>
@<getBackLookingInfoUsage@>
@<getNumEpsUsage@>
@<multiStepUsage@>
@<multiStepZUsage@>
@<multiStepXUsage@>
@<fSumCUsage@>
@<fSumUsage@>
@<getNumEpsVarsUsage@>
@<iterateDRREIntegrateUsage@>
@<genPathUsage@>
@<getNumIgnoredUsage@>
@<getNumInterpVarsUsage@>
@<writeExpKernUsage@>
@<doGenericIterREInterpUsage@>
@<parallelDoGenericIterREInterpUsage@>
@<parallelNestGenericIterREInterpUsage@>
@<nestGenericIterREInterpUsage@>
@}

\section{Package Code}
\label{sec:usage-definitions}

@d package code
@{
@<ExpKernCode@>
@<writeExpKern@>

@<truncErrorMat@>
@<nestGenericIterREInterp@>
@<parallelNestGenericIterREInterp@>
@<doGenericIterREInterp@>
@<parallelDoGenericIterREInterp@>
@<makeGenericInterpFuncs@>
@<parallelSmolyakGenInterpData@>
@<smolyakGenInterpData@>
@<smolyakInterpolation@>
@<smolyakInterpolationPrep@>
@<gettersSetters@>
@<getNumIgnored@>
@<getNumInterpVars@>
@<doFuncArg@>
@<genPath@>
@<iterateDRPF@>
@<iterateDRREIntegrate@>
@<makeREIterFunc@>
@<getNumEpsVars@>
@<myNExpectation@>
@<getDistribs@>
@}
@d package code
@{
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
@<getBackLookingInfo@>
@<getNumEps@>
@<genLilXkZkFunc@>
@<fSumC@>
@<fSum@>
@<genSlots@>
@<genXtOfXtm1@>
@<genXtp1OfXt@>
@<genX0Z0DrvFuncs@>
@<genX0Z0Funcs@>
@<genBothX0Z0Funcs@>
@<multiStep@>
@}
@d package code
@{
@<multiStepZ@>
@<multiStepX@>
@<genFRExtFunc@>
@<genMaxExtFunc@>
@<genNSExtFunc@>
@<genFPFunc@>
@<myFixedPoint@>
@<makeSmolyakInterpFuncs@>
@<parallelMakeInterpFunc@>
@<makeInterpFunc@>
@<parallelGenInterpData@>
@<genInterpData@>
@<gridPts@>
@<oneDimGridPts@>
@<fillIn@>
@<fillInSymb@>
@<parallelNestIterREInterp@> 
@<parallelDoIterREInterp@> 
@<nestIterREInterp@>
@<doIterREInterp@>
@<nestSmolyakIterREInterp@>
@<doSmolyakIterREInterp@>
@<parallelGenXZREInterpFunc@>
@<genSmolyakXZREInterpFunc@>
@<genXZREInterpFunc@>
@<genXZFuncRE@>
@<genIntVars@>
@}
\subsection{Argument Specifications}
\label{sec:argum-spec}


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

@d XZFuncs
@{XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),numSteps_Integer}@}
@d bothXZFuncs
@{bothXZFuncs:{justBothXZFuncs:{xzFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),XZFuncs:(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol)},numSteps_Integer}@}

@d theZs
@{theZs:{_?MatrixQ..}@}


@d xtGuess
@{xtGuess_?MatrixQ@}

@d fCon
@{fCon_?MatrixQ@}

@d gSpec
@{gSpec:{toIgnore:{_Integer...},iOrd_Integer,rngs:{{_Integer,_?NumberQ,_?NumberQ}..}}@}


@d distribSpec
@{distribSpec:{expctSpec:{{_Symbol,_}..}}@}

@d eqnsFunc
@{eqnsFunc:(_Function|_CompiledFunction|_Symbol)@}
@d objFunc
@{objFunc:(_Function|_CompiledFunction|_Symbol)@}
@d consFunc
@{consFunc:(_Function|_CompiledFunction|_Symbol)@}


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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genLilXkZkFunc"->#&,{"genLilXkZkFunc"}]];

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genLilXkZkFunc"->#&,{"fSumC"}]];

@}


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


@d genLilXkZkFunc full call
@{genLilXkZkFunc[@<linMod@>,@<XZFuncs@>,@<xtGuess@>]@}

@d genLilXkZkFunc
@{
@<genLilXkZkFunc full call@>:=
With[{},
@<XZ Functions Given@>]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genLilXkZkFunc"->#&,{"fSum"}]];

@}

@d XZ Functions Given
@{
With[{},
With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},theRes]]]



@}



@d genLilXkZkFuncUsage
@{ genLilXkZkFunc::usage="place holder"@}

@d genLilXkZkFunc fcon call
@{genLilXkZkFunc[@<linMod@>,@<fCon@>]@}


@d genLilXkZkFunc
@{
@<genLilXkZkFunc fcon call@>:=
Module[{},
@<apply formula F...@>
]
AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genLilXkZkFunc"->#&,{"genSlots","getNumX","getNumEps","getNumZ","genXtOfXtm1","genXtp1OfXt"}]];
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

fSumNotC[phi_?MatrixQ,FF_?MatrixQ,psiZ_?MatrixQ,zPath:{_?MatrixQ...}]:=
With[{numXVars=Length[psiZ]},
With[{fPows=Drop[NestList[Function[xx,FF. xx],IdentityMatrix[numXVars],Length[zPath]],-1]},
Apply[Plus,
MapThread[Function[{xx,yy},Dot[xx,phi.psiZ.yy]],{fPows , zPath}]]]]
@}

\subsection{fSum}
\label{sec:fsum}

@d fSumUsage
@{
fSum::usage=
"place holder fSum"
@}
myAbortKernels[]:=AbortKernels[];
@d fSum
@{
fSum[@<linMod@>,
	{},
	xtGuess:{{_?NumberQ..}..}]:=
ConstantArray[0,{Length[psiZ],1}]

fSum[@<linMod@>,
	@<XZFuncs@>,xtGuess:{{_?NumberQ..}..}]:=
With[{numXVars=getNumX[linMod],numZVars=getNumZ[linMod]},
Print["fSum:modified"];
If[numSteps===0,{Table[{0},{numZVars}]},
With[{xzRes=
Check[
Apply[multiStepZ[XZFuncs,numXVars,numZVars,numSteps], 
Flatten[xtGuess]],
Print["problems with current DRCE in multistep,using at",initVec,"linMod!!!!!"];
Apply[multiStepZ[{genX0Z0Funcs[linMod],XZFuncs[[2]]},numXVars,numZVars,numSteps]
Flatten[xtGuess]]]
},
Check[fSumC[phi,FF,psiZ,xzRes],Print["trying to throw high"];Throw[xtGuess]]
]]]

okNums[theVals_?VectorQ]:=
Apply[And,Map[Or[MachineNumberQ[#],IntegerQ[#]]&,theVals]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["fSum"->#&,{"numXVars","numZVars","multiStepZ","fSumC"}]];


@}


\subsection{genSlots}
\label{sec:genxtm1vars}


@d genSlots
@{
(*begin code for genSlots*)
genSlots[numVars_Integer]:=
Module[{},
replaceMySlotStandIn[Table[{mySlotStandIn[ii]},{ii,numVars}]]]/;And[numVars>=0]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genSlots"->#&,{"replaceMySlotStandIn","mySlotStandin"}]];



replaceMySlotStandIn[xx_]:=xx/.mySlotStandIn->Slot








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
With[{numXVars=getNumX[linMod],numZVars=getNumZ[linMod]},
With[{xtm1Vars=genSlots[numXVars]},
With[{fromLinMod=Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]},
Apply[Function,{replaceLinPart[fromLinMod,xtm1Vars,backLookingInfo]}]]]]

replaceLinPart[flm_List,xtm1Vars_List,ble:{{_Integer,_,_}...}]:=
With[{theRes=Map[Function[uu,{uu[[1]],Apply[uu[[3]],Flatten[xtm1Vars]]}],ble]},
Fold[ReplacePart[#1,#2[[1]]->#2[[2]]]&,flm,theRes]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genX0Z0Funcs"->#&,{"getNumX","getNumZ","genSlots"}]];

(*end code for genX0Z0Funcs*)
@}
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

replaceLinPart[flm_List,xtm1Vars_List,ble:{{_Integer,_,_}...}]:=
With[{theRes=Map[Function[uu,{uu[[1]],Apply[uu[[3]],Flatten[xtm1Vars]]}],ble]},
Fold[ReplacePart[#1,#2[[1]]->#2[[2]]]&,flm,theRes]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genBothX0Z0Funcs"->#&,{"getNumX","getNumZ","genSlots"}]];

(*end code for genBothX0Z0Funcs*)
@}
\subsection{genX0Z0DrvFuncs}
\label{sec:genx0z0funcs}


@d genX0Z0DrvFuncsUsage
@{genX0Z0DrvFuncs::usage=
"place holder for genX0Z0DrvFuncs"
@}

@d genX0Z0DrvFuncs
@{
(*begin code for genX0Z0DrvFuncs*)
genX0Z0DrvFuncs[@<linMod@>]:=
With[{numXVars=getNumX[linMod],numZVars=getNumZ[linMod]},
With[{xtm1Vars=Table[Unique["xx"],{numXVars}]},
Apply[Function, {xtm1Vars,ArrayFlatten[{{BB},{ConstantArray[0,{numZVars,numXVars}]}}]}]]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genX0Z0DrvFuncs"->#&,{"getNumX","getNumZ","genSlots"}]];

(*end code for genX0Z0DrvFuncs*)
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

oldmultiStep[@<XZFuncs@>,numX_Integer,valRange:{_Integer..},numTerms_Integer]:=
With[{funcArgs=Flatten[genSlots[numX]]},
With[{appGuts=(Apply[XZFuncs[[1]],Flatten[funcArgs]][[Range[numX]]])},
With[{xtFunc01=Function[appGuts]},
With[{iterGuts=
NestList[Function[xx,Apply[xtFunc01,Flatten[xx]]],funcArgs,numTerms-1]},
With[{theXZGuts=Map[(Function[xx,
Apply[XZFuncs[[1]],Flatten[xx]][[valRange]]]),iterGuts]},
With[{theFunc=Function[theXZGuts]},
theFunc]]]]]]/;numSteps>0


multiStep[@<XZFuncs@>,numX_Integer,valRange:{_Integer..},numTerms_Integer]:=
Module[{xfName=Unique["msXFName"],xzfName=Unique["msXZFName"]},
With[{slotArgs=Flatten[genSlots[numX]],
funcArgs=Unique["msArgs"]},
With[{appGuts=(notApply[XZFuncs[[1]],theArgs$])},
xfName[theArgs:{_?NumberQ..}]:=
Flatten[Apply[XZFuncs[[1]],theArgs$]][[Range[numX]]];
xzfName[theArgs:{_?NumberQ..}]:=
Apply[XZFuncs[[1]],theArgs$];
With[{iterGuts=
NestList[xfName,funcArgs,numTerms-1]/.funcArgs-> slotArgs},
With[{theXZGuts=Map[xzfName,iterGuts]},
With[{theFunc=Function[Map[Function[xx,xx[[valRange]]],theXZGuts]]},
theFunc]]]]]]/;numSteps>0



AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["multiStep"->#&,{"genSlots"}]];


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

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["multiStepZ"->#&,{"multiStep"}]];


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

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["multiStepX"->#&,{"multiStep"}]];

(*end code for multiStepX*)
@}



\subsection{genFRExtFunc}
\label{sec:genfrfunc}


@d genFRExtFuncUsage
@{genFRExtFunc::usage=
"place holder for genFRExtFunc"
evaluateTriple::usage=
"place holder for genFRExtFunc"
makePatternArgs::usage="makePatternArgs"
makeBlankPatternArgs::usage="makePatternArgs"
simulateDR::usage="simulateDR[theDR:(_Symbol|_Function|_CompiledFunction),numVars_Integer,@<gSpec@>,@<distribSpec@>,initVec:{_?NumberQ..},numPers_Integer]"
iterateDRCE::usage="iterateDRCE[drExpFunc:(_Function|_CompiledFunction|_Symbol),initVec_?MatrixQ,numPers_Integer]"
genZsForFindRoot::usage="genZsForFindRoot[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,backLookingInfo:{{_Integer,backLooking_,backLookingExp_}...}},	initVec_?MatrixQ,theCondExp:(_Function|_CompiledFunction),iters_Integer]"



@}

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


@d makeArgPatterns
@{xtm1epsArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs]],
xtztArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makePatternArgs[xArgs],makePatternArgs[zArgs]],
xtNoZtArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makePatternArgs[xArgs]]@}

@d makeArgPatternsBoth
@{xtm1epsArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs]],
xtztArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makePatternArgs[xArgs],makePatternArgs[zArgs]],
xtNoZtArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makePatternArgs[xArgs]]@}


@d cmptXArgsInit
@{xArgsInit=If[varRanges==={},
MapThread[Function[{xx,yy},{xx,yy}],
{xArgs,theXInit[[Range[numX]]]}],
If[VectorQ[varRanges],
MapThread[{#1,#2}&,{xArgs(*,theXInit[[Range[numX]]]*),varRanges}],
MapThread[{#1,#2,#3[[1]],#3[[2]]}&,{xArgs,theXInit[[Range[numX]]],varRanges}]]]@}




@d prepFindRootXInit
@{theXInit=Flatten[Apply[XZFuncs[[1]],Join[xLagArgs,eArgs]]],
zArgsInit=Map[Function[xx,{xx,0}],zArgs],
funcOfXtm1Eps=Unique["fNameXtm1Eps"],
funcOfXtZt=Unique["fNameXtZt"]@}



@d prepFindRootXInitBoth
@{theXInit=Flatten[Apply[bothXZFuncs[[1,1]],Join[xLagArgs,eArgs]]],
zArgsInit=Map[Function[xx,{xx,0}],zArgs],
funcOfXtm1Eps=Unique["fNameXtm1Eps"],
funcOfXtZt=Unique["fNameXtZt"]@}



@d findRootArgNames
@{funcArgs=Flatten[genSlots[numX+numEps]],
zArgs=Table[Unique["theFRZArgs"],{numZ}],
xArgs=Table[Unique["theFRXArgs"],{numX}],
xLagArgs=Table[Unique["theFRXLagArgs"],{numX}],
eArgs=Table[Unique["theFREArgs"],{numEps}]@}




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
@d genFRExtFunc
@{


delayEvalXkRes[xkFunc:(_Function|_CompiledFunction|_Symbol),funcArgs:{_?NumberQ..},
zArgs:{_?NumberQ..},numX_Integer]:=
Part[Drop[Apply[xkFunc,Join[funcArgs,zArgs]],numX],Range[numX]]

 
cmpXZVals[xzVals_?MatrixQ,theZArgs:{_Symbol..},theResult:{(_->_)..}]:=
Transpose[{Flatten[Join[xzVals,theZArgs]/.theResult]}]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genFRExtFunc"->#&,{"genSlots","cmpXZVals"}]];


simulateDR[theDR:(_Symbol|_Function|_CompiledFunction),
@<gSpec@>,@<distribSpec@>,
initVec:{_?NumberQ..},numPers_Integer]:=
With[{numEps=Length[expctSpec]},
With[{numVars=Length[rngs]+Length[toIgnore]-numEps},
With[{draws=Flatten[
Transpose[Map[RandomVariate[#[[1,2]],numPers]&,distribSpec]],1]},
FoldList[Flatten[Apply[theDR, Append[Flatten[#1][[Range[numVars]]],#2]]]&,initVec,draws]]]]


(*end code for genFRExtFunc*)
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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genFPFunc"->#&,{"genSlots","cmpXZVals","getNumX","getNumEps","getNumZ","myFixedPoint","genLilXkZkFunc"}]];

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


\subsection{callGraph}
@d callGraphUsage
@{
AMASeriesRepCallGraph::usage="AMASeriesRepCallGraph";AMASeriesRepCallGraph={};
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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genXZFuncRE"->#&,{"myNExpectation","genIntVars"}]];


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

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genIntVars"->#&,{"getDistribs","getNumEpsVars"}]];

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

myNExpectation[funcName_Symbol[farg_List,idx_Integer],nArgs_List]:=NExpectation[funcName[farg,idx],nArgs]


myExpectation[farg_List,nArgs_List]:=
stringArgsToInt[
Expectation[intArgsToString[farg],intArgsToString[nArgs]]]


intArgsToString[exp_]:=exp/.xx[val_Integer]:>xx[ToString[val]]

stringArgsToInt[exp_]:=exp/.xx[val_String]:>xx[ToExpression[val]]



myNewNExpectation[fff_[fargs___],anEpsVar_\[Distributed] PerfectForesight]:=Module[{},Print["there",{(Apply[fff,{fargs}]),{fargs}/.anEpsVar->0}];(Apply[fff,{fargs}])/.anEpsVar->0]


myNewNExpectation[fff_[fargs___],distStuff_]:=Module[{},Print["jhere",{(Apply[fff,{fargs}]),{fargs}}];NExpectation[Apply[fff,{fargs}],distStuff]]



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
iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction|_Symbol),
drExpFunc:(_Function|_CompiledFunction|_Symbol),initVec_?MatrixQ,numEps_Integer,numPers_Integer]:=
With[{firVal=Apply[drFunc,Flatten[initVec]]},
	With[{numX=Length[initVec]-numEps},
With[{iterated=
NestList[Function[xx,((Transpose[{Flatten[Apply[drExpFunc,Flatten[xx]]]}]))],firVal,numPers-1]},
Join[initVec[[Range[numX]]],Apply[Join,
(Map[Function[xx,Identity[xx[[Range[numX]]]]],iterated])]]]]]/;
And[numPers>0]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["iterateDRREIntegrate"->#&,{}]];


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

makeREIterFunc[drFunc:(_Function|_CompiledFunction|_Symbol),@<distribSpec@>]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[drFunc[[1]]]-numEps,numZ=0},
	genXZFuncRE[{numX,numEps,numZ},drFunc,distribSpec]]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["makeREIterFunc"->#&,{"getNumEpsVars","genXZFuncRE"}]];

(*end code for makeREIterFunc*)
@}


\subsection{genNSExtFunc}
\label{sec:gennsfunc}



@d genNSExtFuncUsage
@{genNSExtFunc::usage=
"place holder for genNSExtFunc"
@}

@d genNSExtFunc
@{
 
genNSExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},@<linMod@>,@<XZFuncs@>,
@<eqnsFunc@>,opts:OptionsPattern[]]:=
Module[{},
With[{funcArgs=Flatten[genSlots[numX+numEps]],
zArgs=Table[Unique["theFRZArgs"],{numZ}],
xArgs=Table[Unique["theFRXArgs"],{numX}],
xLagArgs=Table[Unique["theFRXLagArgs"],{numX}],
eArgs=Table[Unique["theFREArgs"],{numEps}]
},
With[{theXInit=Flatten[Apply[XZFuncs[[1]],Join[xLagArgs,eArgs]]],
xkFunc=genLilXkZkFunc[linMod,XZFuncs,Transpose[{xArgs}]],
zArgsInit=Map[Function[xx,{xx,0}],zArgs],
funcOfXtm1Eps=Unique["fNameXtm1Eps"],
funcOfXtZt=Unique["fNameXtZt"]
},
With[{xArgsInit=MapThread[Function[{xx,yy},{xx,yy}],
{xArgs,theXInit[[Range[numX]]]}],
xtm1epsArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs]],
xtztArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makeBlankPatternArgs[xArgs],makeBlankPatternArgs[zArgs]]},
(**)
SetDelayed[
funcOfXtZt[
(**)
Apply[Sequence,xtztArgPatterns]],
Module[{},
With[{xkFunc=genLilXkZkFunc[linMod,XZFuncs,Transpose[{xArgs}]]},
With[{xkAppl=Apply[xkFunc,Join[xLagArgs,eArgs,zArgs]]},
With[{eqnAppl=Apply[eqnsFunc,Flatten[xkAppl]],
xDisc=xArgs-xkAppl[[numX+Range[numX]]]},
Flatten[Join[xDisc,eqnAppl]]]]]]];
(**)
SetDelayed[
funcOfXtm1Eps
[Apply[Sequence,xtm1epsArgPatterns]],
(**)
With[{frRes=NSolve[
funcOfXtZt[Apply[Sequence,Join[xLagArgs,eArgs,
xArgs,zArgs]]],
Join[xArgs,zArgs],Reals]},Print["frRes:",frRes//InputForm];
Flatten[Join[xArgs,zArgs]]/.frRes]];
(**)
DistributeDefinitions[funcOfXtZt,funcOfXtm1Eps]
funcOfXtm1Eps
]]]]




Options[genNSExtFunc]={"xVarRanges"->{},"Traditional"->False} 
genNSExtFunc[{numX_Integer,numEps_Integer,numZ_Integer},@<linMod@>,@<bothXZFuncs@>,
triples:{{{_Function,(_Function|_CompiledFunction|_Symbol),_Function}..},
selectorFunc_Function},
opts:OptionsPattern[]]:=
Module[{varRanges=OptionValue["xVarRanges"]},
With[{funcTrips=
Map[{#[[1]],genNSExtFunc[{numX,numEps,numZ},linMod,bothXZFuncs,#[[2]],
Apply[Sequence,
FilterRules[{opts},Options[genFRExtFunc]]]],#[[3]]}&,triples[[1]]]},
{funcTrips,selectorFunc}
]]




(*end code for genNSExtFunc*)
@}




\subsection{genMaxExtFunc}
\label{sec:gennsfunc}



@d genMaxExtFuncUsage
@{genMaxExtFunc::usage=
"place holder for genMaxExtFunc"
@}

@d genMaxExtFunc
@{
 
genMaxExtFunc[{numX_Integer,numEps_Integer,numZ_Integer,
numEqCons_Integer,numInEqCons_Integer},
@<linMod@>,@<XZFuncs@>,@<objFunc@>,@<consFunc@>,opts:OptionsPattern[]]:=
Module[{},
With[{funcArgs=Flatten[genSlots[numX+numEps]],
eqLamArgs=Table[Unique["theeqLamArgs"],{numEqCons}],
diffLamArgs=Table[Unique["thediffLamArgs"],{numX}],
inEqLamArgs=Table[Unique["theineqLamArgs"],{numInEqCons}],
zArgs=Table[Unique["theFRZArgs"],{numZ}],
xArgs=Table[Unique["theFRXArgs"],{numX}],
xDiffArgs=Table[Unique["theFRXDiffArgs"],{numX}],
xLagArgs=Table[Unique["theFRXLagArgs"],{numX}],
eArgs=Table[Unique["theFREArgs"],{numEps}]
},
With[{theXInit=Flatten[Apply[XZFuncs[[1]],Join[xLagArgs,eArgs]]],
xkFunc=genLilXkZkFunc[linMod,XZFuncs,Transpose[{xArgs}]],
zArgsInit=Map[Function[xx,{xx,0}],zArgs],
funcOfXtm1Eps=Unique["fNameMaxXtm1Eps"],
funcOfCon=Unique["fNameMaxCon"],
funcOfXtZt=Unique["fNameMaxXtZt"],
cmpXDisc=Unique["fNameMaxXDisc"],
cmpEqnsAppl=Unique["fNameMaxCmpEqnsAppl"],
cmpXKAppl=Unique["fNameMaxCmpXKAppl"],
consEqnsAppl=Unique["fNameMaxConsAppl"],
funcMakeLagrangean=Unique["fNameMaxLagrangean"],
objEqnsAppl=Unique["fNameMaxObjAppl"]
},
With[{
xArgsInit=MapThread[Function[{xx,yy},{xx,yy}],
{xArgs,theXInit[[Range[numX]]]}],
xtArgPatterns=makePatternArgs[xArgs],
xtm1epsArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs]],
xtztArgPatterns=Join[makePatternArgs[xLagArgs],
makePatternArgs[eArgs],
makeBlankPatternArgs[xArgs],makeBlankPatternArgs[zArgs]],
xkFuncArgs=Join[
makeBlankPatternArgs[xLagArgs],
makeBlankPatternArgs[eArgs],
makePatternArgs[xArgs],
makeBlankPatternArgs[zArgs]],
xkFuncDiffArgs=Join[
makeBlankPatternArgs[xLagArgs],
makeBlankPatternArgs[eArgs],
makeBlankPatternArgs[xDiffArgs],
makeBlankPatternArgs[zArgs]],
initXEpsFuncArgs=Join[
makeBlankPatternArgs[xLagArgs],
makeBlankPatternArgs[eArgs]]},
(**)
SetDelayed[
cmpXKAppl[
Apply[Sequence,xkFuncDiffArgs]],
(**)
With[{xkFunc=genLilXkZkFunc[linMod,XZFuncs,Transpose[{xDiffArgs}]]},
xkAppl=Apply[xkFunc,Join[xLagArgs,eArgs,zArgs]]
]];
(**)
SetDelayed[
objEqnsAppl[
Apply[Sequence,xkFuncDiffArgs]],
(**)
With[{xkAppl=Apply[cmpXKAppl,Join[xLagArgs,eArgs,xDiffArgs,zArgs]]},
Apply[objFunc,Flatten[xkAppl]]]];
(**)
SetDelayed[
consEqnsAppl[
Apply[Sequence,xkFuncDiffArgs]],
(**)
With[{xkAppl=Apply[cmpXKAppl,Join[xLagArgs,eArgs,xDiffArgs,zArgs]]},
Apply[Plus,eqLamArgs*Apply[consFunc,Flatten[xkAppl]]]]];
(**)
SetDelayed[
cmpXDisc[
Apply[Sequence,xkFuncDiffArgs]],
(**)
With[{xkAppl=Apply[cmpXKAppl,Join[xLagArgs,eArgs,xDiffArgs,zArgs]]},
Apply[Plus,diffLamArgs*(xDiffArgs-Flatten[xkAppl[[numX+Range[numX]]]])]]];
(**)
SetDelayed[
funcOfCon[
Apply[Sequence,xkFuncDiffArgs]],
(**)
Module[{},
With[{consAppl=Apply[consEqnsAppl,Join[xLagArgs,eArgs,xDiffArgs,zArgs]],
xDisc=Apply[cmpXDisc,Join[xLagArgs,eArgs,xDiffArgs,zArgs]]},
consAppl+xDisc]]];
(**)
SetDelayed[
funcMakeLagrangean[
Apply[Sequence,xkFuncDiffArgs]],
(**)
Module[{},
With[{objAppl=Apply[objEqnsAppl,Join[xLagArgs,eArgs,xDiffArgs,zArgs]],
consAppl=Apply[funcOfCon,Join[xLagArgs,eArgs,xDiffArgs,zArgs]]},
With[{lagr=objAppl-consAppl},
With[{theSys=Thread[(Map[D[lagr,#]&,Join[xDiffArgs,zArgs,eqLamArgs,diffLamArgs]])==0]},
(*Print["{obj,cons,lagr,theSys}=",{objAppl,"XXXXXXXXXXXXX",consAppl,"XXXXXXXXXXXXX",lagr,"XXXXXXXXXXXXX",theSys}//InputForm];*)
theSys
]]]]];
(**)
SetDelayed[
funcOfXtZt[
Apply[Sequence,xkFuncArgs]],
(**)
Module[{},
With[{objAppl=Apply[objEqnsAppl,Join[xLagArgs,eArgs,xArgs,zArgs]],
consAppl=Apply[consEqnsAppl,Join[xLagArgs,eArgs,xArgs,zArgs]],
xDisc=Apply[cmpXDisc,Join[xLagArgs,eArgs,xArgs,zArgs]]},
With[{lagr=objAppl-consAppl},
With[{theSys=Thread[(Map[D[lagr,#]&,Join[xArgs,zArgs,eqLamArgs]])==0]},
theSys
]]]]];
(**)
SetDelayed[
funcOfXtm1Eps[
Apply[Sequence,initXEpsFuncArgs]],
(**)
With[{theXInit=Flatten[Apply[XZFuncs[[1]],Join[xLagArgs,eArgs]]],
restArgsInit=Map[Function[xx,{xx,0}],Join[zArgs,eqLamArgs,diffLamArgs]]},
With[{lagr=Apply[funcMakeLagrangean,Join[xLagArgs,eArgs,xArgs,zArgs]]},
With[{fmRes=FindRoot[lagr,
Join[xArgsInit,restArgsInit]]},
Transpose[{Flatten[Join[xArgs,zArgs]]}]/.fmRes]]]];
(**)
funcOfXtm1Eps
]]]]

delayJoin[theArgs:_List..]:=Join[theArgs]

(*end code for genMaxExtFunc*)
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
With[{firVal=Apply[drFunc,Flatten[initVec]],
numX=Length[initVec]-numEps,theZeros=Table[0,{numEps}]},Print["firVal=",firVal];
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

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genPath"->#&,{"multiStep"}]];

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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["pathErrsDRPF"->#&,{"doFuncArg","iterateDRPF"}]];

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
pathErrsDRREIntegrate[drFunc_Function,drExpFunc_Function,
initVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>,numPers_Integer]:=
With[{pathNow=iterateDRREIntegrate[drFunc,drExpFunc,initVec,numEps,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
restArgs=(Map[Function[xx,
doFuncArg[pathNow,Table[{0},{numEps}],numX,xx-2]],Range[3,numPers]])
},
With[{first=Transpose[{Apply[eqnsFunc,Flatten[firstArg]]}]},
	With[{theRest=Map[Function[xx,Transpose[{(Apply[eqnsFunc,Flatten[xx]])}]],restArgs]
},
		Prepend[theRest,first]
]]]]/;
And[numPers>1]
 

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["pathErrsDRPF"->#&,{"doFuncArg","iterateDRREIntegrate"}]];
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
evalPathErrDRREIntegrate[drFunc_Function,drExpFunc_Function,
initVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>]:=
pathErrsDRREIntegrate[drFunc,drExpFunc,initVec,numEps,eqnsFunc,2]//First



evalPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,drExpFunc_Function,initVec_?MatrixQ,numEps_Integer,
@<eqnsFunc@>]:=
phi . (pathErrsDRREIntegrate[drFunc,drExpFunc,initVec,numEps,eqnsFunc,2]//First)


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["evalPathErrorDRREIntegrate"->#&,{"pathErrsDRREIntegrate"}]];


(*end code for evalPathErrDRREIntegrate*)
@}

\subsection{evalBadPathErrDRREIntegrate}
\label{sec:evalb}

@d evalBadEulerErrDRREIntegrateUsage
@{evalBadEulerErrDRREIntegrate::usage=
"place holder for evalBadEulerErrDRREIntegrate"
@}





@d evalBadPathErrDRREIntegrateUsage
@{evalBadPathErrDRREIntegrate::usage=
"place holder for evalBadPathErrDRREIntegrate"
@}

@d evalBadPathErrDRREIntegrate
@{
(*begin code for evalBadPathErrDRREIntegrate*)
evalBadPathErrDRREIntegrate[drFunc_Function,drExpFunc_Function,
noEpsVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
	With[{theVal=evalPathErrDRREIntegrate[drFunc,drExpFunc,Join[noEpsVec,Transpose[{tryEps}]],numEps,eqnsFunc]},
		With[{theNorm=Norm[theVal,Infinity]},
		theNorm]];
	With[{outerEVars=Table[Unique["eVs"],{numEps}]},
	With[{maxArgs=Map[Function[xx,{xx,0}],outerEVars],cons=Apply[And,  (Map[Function[xx,(-0.01<=xx<=0.01)], outerEVars])]},
	FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]

evalBadEulerErrDRREIntegrate[drFunc_Function,drExpFunc_Function,
noEpsVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
	With[{theVal=evalPathErrDRREIntegrate[drFunc,drExpFunc,Join[noEpsVec,Transpose[{tryEps}]],numEps,eqnsFunc]},
		With[{theNorm=theVal[[1,1]]},
		theNorm]];
	With[{outerEVars=Table[Unique["eVs"],{numEps}]},
	With[{maxArgs=Map[Function[xx,{xx,0}],outerEVars],cons=Apply[And,  (Map[Function[xx,(-0.01<=xx<=0.01)], outerEVars])]},
	FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]


evalBadPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,drExpFunc_Function,
noEpsVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
With[{theVal=phi .evalPathErrDRREIntegrate[drFunc,drExpFunc,Join[noEpsVec,Transpose[{tryEps}]],numEps,eqnsFunc]},
		Norm[theVal,Infinity]];
	With[{outerEVars=Table[Unique["eVs"],{numEps}]},
	With[{maxArgs=Map[Function[xx,{xx,0}],outerEVars],cons=Apply[And,  (Map[Function[xx,(-0.01<=xx<=0.01)], outerEVars])]},
	FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["evalBadPathErrorDRREIntegrate"->#&,{"evalPathErrDRREIntegrate","iterateDRREIntegrate"}]];

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

worstPathForErrDRREIntegrate[drFunc_Function,drExpFunc_Function,noEpsVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>]:=
With[{fMinRes=evalBadPathErrDRREIntegrate[drFunc,drExpFunc,noEpsVec,numEps,eqnsFunc]},
	With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},
	With[{badPath=iterateDRREIntegrate[drFunc,drExpFunc,Join[noEpsVec,badEps],numEps,2]},
		Join[badPath,badEps]]]]

worstPathForErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,drExpFunc_Function,noEpsVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>]:=
With[{fMinRes=
evalBadPathErrDRREIntegrate[phi,drFunc,drExpFunc,noEpsVec,numEps,eqnsFunc]},
	With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},
	With[{badPath=iterateDRREIntegrate[drFunc,drExpFunc,Join[noEpsVec,badEps],numEps,2]},
		Join[badPath,badEps]]]]



AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["worstPathForErrorDRREIntegrate"->#&,{"evalBadPathErrDRREIntegrate","iterateDRREIntegrate"}]];



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

@d getterSetterTests
@{
getterSetterTests=Append[getterSetterTests,
VerificationTest[getNumEpsVars[theDist]==1,TestID->"getNumEpsVars:linMod"]]

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
@d getterSetterTests
@{
getterSetterTests=Append[getterSetterTests,
VerificationTest[getDistribs[theDist]=={NormalDistribution[0, 1/100]},
TestID->"getDistribs:theDist"]]

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
getNumVars[@<smolGSpec@>]:=
Length[smolGSpec[[2]]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["getNumVars"->#&,{"getGridPtTrips"}]];



(*end code for getNumVars*)
@}

@d getterSetterTests
@{
getterSetterTests=Append[getterSetterTests,
VerificationTest[getNumVars[aGSpec]==3,
TestID->"getNumVars:aGSpec"]]

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
@d getterSetterTests
@{
getterSetterTests=Append[getterSetterTests,
VerificationTest[getH[linMod]==simpleRBCModel`Private`hmatSymbRE//N,
TestID->"getH:linMod"]]
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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["getNumZ"->#&,{"getPsiZ"}]];


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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["getNumX"->#&,{"getB"}]];



@}


@d getBackLookingInfoUsage
@{
getBackLookingInfo::usage=
"getBackLookingInfo[@<linMod@>]"<>
"number of x variables"
@}

@d getBackLookingInfo
@{
getBackLookingInfo[@<linMod@>]:=linMod[[8]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["getBackLookingInfo"->#&,{"getB"}]];



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

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["getNumEps"->#&,{"getPsiEps"}]];

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


\section{Tests}

@o AMASeriesRepresentationTests.m
@{
$Path=Join[$Path,{"../../ProtectedSymbols","../../mathSmolyak"}]
Needs["AMASeriesRepresentation`"]
Needs["simpleRBCModel`"]
getterSetterTests={};
@<getterSetterTests@>

@}
@d getterSetterTestsUsage
@{
getterSetterTests::usage="getterSetterTestsUsage"
@}



\subsection{Identifiers}
\label{sec:identifiers}

@u
\subsection{Macros}
\label{sec:macros}

@m

\bibliographystyle{plainnat}
\bibliography{files}

collect
genlilxkzk

@d XZ Functions Given
@{
With[{},
With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},theRes]]]



@}

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


@d genLilXkZkFunc full call
@{genLilXkZkFunc[@<linMod@>,@<XZFuncs@>,@<xtGuess@>]@}

@d genLilXkZkFunc
@{
@<genLilXkZkFunc full call@>:=
With[{},
@<XZ Functions Given@>]

@}

@d genLilXkZkFunc fcon call
@{genLilXkZkFunc[@<linMod@>,@<fCon@>]@}


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

@d XZFuncs
@{XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),numSteps_Integer}@}

@d theZs
@{theZs:{_?MatrixQ..}@}


@d xtGuess
@{xtGuess_?MatrixQ@}

@d fCon
@{fCon_?MatrixQ@}




\end{document}
