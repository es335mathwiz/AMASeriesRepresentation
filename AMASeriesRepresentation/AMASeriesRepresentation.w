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
numEps_Integer}@}


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
@}

@d smolyakGenInterpData
@{
(*begin code for smolyakGenInterpData*)

 
smolyakGenInterpData[
aVecFunc:(_Function|_CompiledFunction),@<smolGSpec@>]:=
Module[{},
With[{filledPts=Map[
Function[xx,fillIn[{{},smolToIgnore,xx}]],N[smolPts]]},
With[{theVals=Map[Function[xx,(Apply[aVecFunc,xx])],filledPts]},
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
smolyakInterpolation[fVals:{_?NumberQ..},@<smolGSpec@>,{}]:=
smolyakInterpolation[fVals,smolGSpec]


smolyakInterpolation[fVals:{_?NumberQ..},@<smolGSpec@>]:=
With[{wts=LinearSolve[smolMat,fVals],numVars=Length[smolRngs]},
With[{origXs=Table[xx[ii],{ii,numVars}],
theXs=Table[Unique["xx"],{ii,numVars}]},
{Apply[Function,({theXs,Simplify[
(wts.(smolPolys/.Thread[origXs->theXs]))/.
Thread[theXs->MapThread[xformXValToCheb,{theXs,smolRngs}]]]})],
Apply[Function,({Drop[theXs,-numEps],Simplify[
(wts.(smolIntPolys/.Thread[Drop[origXs,-numEps]->Drop[theXs,-numEps]]))/.
Thread[theXs->MapThread[xformXValToCheb,{theXs,smolRngs}]]]})]}]]

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
svmRegressionLinear[fVals:{_?NumberQ..},@<smolGSpec@>,svmArgs:{_?NumberQ,_?NumberQ}]:=
Module[{svmtLinear = JavaNew["libsvm.trainGuts"],modLinear,
betterArgs=Table[Unique["arg"],{Length[smolRngs]}]},
svmtLinear[mmaUreadUproblemLinear[smolPts//N,fVals//N, svmArgs]];
modLinear = 
     libsvm`svm`svmUtrain[svmtLinear[prob],svmtLinear[param]];
Sow[huh=modLinear[svUindices],"svIndices"];
linFunc=Function[xx,(libsvm`svm`mysvmUpredictUvalues[modLinear, xx] )];
expLinFunc=Function[xx,(libsvm`svm`mysvmUpredictUExpectedUvalues[modLinear, xx] )];
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
svmRegressionPoly[fVals:{_?NumberQ..},@<smolGSpec@>,svmArgs:{_?NumberQ,_?NumberQ,_?NumberQ,_?NumberQ,_?NumberQ}]:=
Module[{svmtPoly = JavaNew["libsvm.trainGuts"],modPoly,
betterArgs=Table[Unique["arg"],{Length[smolRngs]}]},
svmtPoly[mmaUreadUproblemPoly[smolPts//N,fVals//N, svmArgs]];
modPoly = 
     libsvm`svm`svmUtrain[svmtPoly[prob],svmtPoly[param]];
polyFunc=Function[xx,(libsvm`svm`mysvmUpredictUvalues[modPoly, xx] )];
expPolyFunc=Function[xx,(libsvm`svm`mysvmUpredictUExpectedUvalues[modPoly, xx] )];
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
svmRegressionRBF[fVals:{_?NumberQ..},@<smolGSpec@>,svmArgs:{_?NumberQ,_?NumberQ,_?NumberQ}]:=
Module[{svmtRBF = JavaNew["libsvm.trainGuts"],modRBF,
betterArgs=Table[Unique["arg"],{Length[smolRngs]}]},
svmtRBF[mmaUreadUproblemRBF[smolPts//N,fVals//N, svmArgs]];
modRBF = 
     libsvm`svm`svmUtrain[svmtRBF[prob],svmtRBF[param]];
rbfFunc=Function[xx,(libsvm`svm`mysvmUpredictUvalues[modRBF, xx] )];
expRBFFunc=Function[xx,(libsvm`svm`mysvmUpredictUExpectedUvalues[modRBF, xx] )];
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
svmRegressionSigmoid[fVals:{_?NumberQ..},@<smolGSpec@>,svmArgs:{_?NumberQ,_?NumberQ,_?NumberQ,_?NumberQ}]:=
Module[{svmtSigmoid = JavaNew["libsvm.trainGuts"],modSigmoid,
betterArgs=Table[Unique["arg"],{Length[smolRngs]}]},
svmtSigmoid[mmaUreadUproblemSigmoid[smolPts//N,fVals//N, svmArgs]];
modSigmoid = 
     libsvm`svm`svmUtrain[svmtSigmoid[prob],svmtSigmoid[param]];
sigmoidFunc=Function[xx,(libsvm`svm`mysvmUpredictUvalues[modSigmoid, xx] )];
expSigmoidFunc=Function[xx,(libsvm`svm`mysvmUpredictUExpectedUvalues[modSigmoid, xx] )];
{sigmoidFunc/.xx$->betterArgs,expSigmoidFunc/.xx$->Drop[betterArgs,-numEps]}]

@}

@d expectation templates
@{
expKernTop=
"package forImport;
//``
public class `` {
public static double [] testExpKern(double[] xNow)   {
   double [] theExpVals = new double[``];
``
``
   return(theExpVals);
} 
public static double [] testKern(double[] xNow)   {
   double [] theVals = new double[``];
``
``
   return(theVals);
} 
}
"

expKernBottom=""



@}

@d writeExpKern
@{
writeExpKern[theFile_String,expEqns_List,
expCode_String,expDefines_String,kernCode_String,kernDefines_String]:=
Module[{javaFile="forImport/"<>theFile<>".java"},
With[{theClassCode=TemplateApply[expKernTop,
{DateString[],theFile,
Length[expEqns],expDefines,expCode,
Length[expEqns],kernDefines,kernCode}]},
WriteString[javaFile,theClassCode];
WriteString[javaFile,expKernBottom];
       Close[javaFile]]]

@<expectation templates@>

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
With[{kernelPart=Flatten[Map[theKernel[Transpose[{#}],Join[xvars,epsvars]]&,
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


dotProdKernel=Function[{xx,yy},Transpose[xx] . yy];



makeSymbolicKernel[]:=
With[{myX=Unique["xx"],myY=Unique["yy"]},
	 Apply[Function,{{myX,myY},ffff[myX,myY]}]]

makePolynomialKernel[aPow_Integer]:=
With[{myX=Unique["xx"],myY=Unique["yy"]},
Apply[Function,{{myX,myY},(1+Transpose[myX] . myY)^aPow}]]


makeRBFKernel[aRadius_Integer]:=
Function[{myX,myY},E^(-Outer[Norm[Plus[#1,#2]]&,Transpose[myX],-Transpose[myY],1]/(2*(aRadius^2)))]

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
cnstrctExpKern::usage="cnstrctExpKern[xData_?MatrixQ,yData_?VectorQ,
theKernel_Function,CC_?NumberQ,epsilon_?NumberQ,numEps_Integer]"
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
@{smolyakInterpolationPrep::usage="place holder"@}

@d smolyakInterpolationPrep
@{

smolyakInterpolationPrep[approxLevels_?listOfIntegersQ,smolRngs_?MatrixQ,
@<distribSpec@>]:=
Module[{smolRes=sparseGridEvalPolysAtPts[approxLevels],
numVars=Length[approxLevels],numEps=Length[distribSpec[[1]]]},
With[{thePts=smolRes[[1]],smolPolys=smolRes[[2]],smolMat=smolRes[[3]]},
With[{xPts=Map[Function[xx,xformToXVec[xx,smolRngs]],thePts]},
With[{numPolys=Length[smolPolys]},
{xPts,smolMat,smolPolys,smolPolyExp[smolPolys,distribSpec]}]]]]/;
And[Length[smolRngs]==Length[approxLevels]]\

smolPolyExp[aSmolPoly_,@<distribSpec@>]:=
With[{numEps=Length[distribSpec[[1]]],
polyVars=Sort[Cases[aSmolPoly,xx[_Integer]]]},
With[{numX=Length[polyVars]-numEps},
With[{intVarRes=genIntVars[numX,distribSpec]},
With[{polyEps=Drop[polyVars,numX],intEps=Drop[intVarRes[[2]],numX]},
With[{epsSubs=MapThread[#1->#2&,{polyEps,intEps}]},
With[{funcGuts=(aSmolPoly/.epsSubs)},
myExpectation[funcGuts,intVarRes[[3]]]]]]]]]



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

 
parallelSmolyakGenInterpData[aVecFunc:(_Function|_CompiledFunction),
@<gSpec@>,@<smolGSpec@>]:=
With[{filledPts=ParallelMap[
Function[xx,fillIn[{{},smolToIgnore,xx}]],smolyakPts]},
With[{theVals=ParallelMap[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{smolyakPts,theVals}]},
interpData]]]



  
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


makeSmolyakInterpFuncs[aVecFunc:(_Function|_CompiledFunction),@<smolGSpec@>]:=
With[{interpData=smolyakGenInterpData[aVecFunc,smolGSpec],
numArgs=Length[smolPts[[1]]]},
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["fArgs"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,
With[{theInterps=smolyakInterpolation[interpData[[All,funcIdx]],smolGSpec]},
With[{smolApp=theInterps},Print["interpdata=",interpData];
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


makeGenericInterpFuncs[aVecFunc:(_Function|_CompiledFunction),@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
Module[{},
With[{interpData=smolyakGenInterpData[aVecFunc,smolGSpec],
numArgs=Length[smolPts[[1]]]},
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["fArgs"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,
With[{theInterps=genericInterp[interpData[[All,funcIdx]],smolGSpec,svmArgs]},
(*Print["theInterps=",{interpData,theInterps,interpFuncList}//InputForm];*)
With[{smolApp=theInterps},
smolApp]]],Range[numFuncs]]},
With[
{applied=Transpose[{Map[notApply[#,funcArgs]/.makeSubs[#,funcArgs]&,
Map[First,interpFuncList]]}],
appliedExp=Transpose[{Map[notApply[#,funcArgs]/.makeSubs[#,Drop[funcArgs,-numEps]]&,
Map[Last,interpFuncList]]}]},
(*Print["applied=",{applied,appliedExp}//InputForm];*)
With[{thePair=
{
ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}]/.notApply->Apply,
ReplacePart[
	Function[xxxxxxx, appliedExp],
		{1->Drop[longFuncArgs,-numEps]}]/.notApply->Apply
}},
thePair
	]
]]]]]]

parallelMakeGenericInterpFuncs[aVecFunc:(_Function|_CompiledFunction),@<smolGSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
Module[{},
With[{interpData=smolyakGenInterpData[aVecFunc,smolGSpec],
numArgs=Length[smolPts[[1]]]},
With[{numFuncs=Length[interpData[[1]]],
funcArgs=Table[Unique["fArgs"],{numArgs}]},
With[{longFuncArgs=fillInSymb[{{},smolToIgnore,funcArgs}]},
With[{interpFuncList=
Map[Function[funcIdx,
With[{theInterps=genericInterp[interpData[[All,funcIdx]],smolGSpec,svmArgs]},
(*Print["theInterps=",{interpData,theInterps,interpFuncList}//InputForm];*)
With[{smolApp=theInterps},
smolApp]]],Range[numFuncs]]},
With[
{applied=Transpose[{Map[notApply[#,funcArgs]/.makeSubs[#,funcArgs]&,
Map[First,interpFuncList]]}],
appliedExp=Transpose[{Map[notApply[#,funcArgs]/.makeSubs[#,Drop[funcArgs,-numEps]]&,
Map[Last,interpFuncList]]}]},
(*Print["applied=",{applied,appliedExp}//InputForm];*)
With[{thePair=
{
ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}]/.notApply->Apply,
ReplacePart[
	Function[xxxxxxx, appliedExp],
		{1->Drop[longFuncArgs,-numEps]}]/.notApply->Apply
}},
thePair
	]
]]]]]]

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
doSmolyakIterREInterp[@<theSolver@>,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,@<distribSpec@>]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
With[{theFuncs=
makeSmolyakInterpFuncs[
genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],smolGSpec]},
theFuncs]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,
Map["doSmolyakIterREInterp"->#&,
{"makeSmolyakInterpFuncs","genFPFunc","getNumX",
"getNumEps","getNumZ","genXZREInterpFunc"}]];



(*end code for doSmolyakIterREInterp*)
@}


@d doGenericIterREInterpUsage
@{
parallelDoGenericIterREInterp::usage=
"place holder for info";
doGenericIterREInterp::usage=
"place holder for info";
@}


@d doGenericIterREInterp
@{
(*begin code for doSmolyakIterREInterp*)
doGenericIterREInterp[@<theSolver@>,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,@<distribSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
With[{theFuncs=
makeGenericInterpFuncs[
genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],smolGSpec,
genericInterp,svmArgs]},
theFuncs]]

parallelDoGenericIterREInterp[@<theSolver@>,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<smolGSpec@>,@<distribSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...}]:=
With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
tn=AbsoluteTime[];
With[{theFuncs=
parallelMakeGenericInterpFuncs[
genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],smolGSpec,
genericInterp,svmArgs]},
theFuncs]]

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


nestSmolyakIterREInterp[@<theSolver@>,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,@<smolGSpec@>,
@<distribSpec@>,numIters_Integer]:=
NestList[Function[xx,doSmolyakIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,distribSpec]],{99,XZFuncs[[1]]},numIters]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["nestIterREInterp"->#&,{"doSmolyakIterREInterp"}]];



(*end code for nestSmolyakIterREInterp*)
@}



@d nestGenericIterREInterpUsage
@{
parallelNestGenericIterREInterp::usage=
"place holder for info";
nestGenericIterREInterp::usage=
"place holder for info"
@}


@d nestGenericIterREInterp
@{
(*begin code for nestGenericIterREInterp*)


nestGenericIterREInterp[@<theSolver@>,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,@<smolGSpec@>,
@<distribSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},
numIters_Integer]:=
NestList[Function[xx,doGenericIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,distribSpec,genericInterp,svmArgs]],{99,XZFuncs[[1]]},numIters]


parallelNestGenericIterREInterp[@<theSolver@>,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,@<smolGSpec@>,
@<distribSpec@>,
genericInterp:(smolyakInterpolation|svmRegressionLinear|svmRegressionPoly|svmRegressionRBF|svmRegressionSigmoid),svmArgs:{_?NumberQ...},
numIters_Integer]:=
NestList[Function[xx,parallelDoGenericIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,smolGSpec,distribSpec,genericInterp,svmArgs]],{99,XZFuncs[[1]]},numIters]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["nestIterREInterp"->#&,{"doGenericIterREInterp"}]];



(*end code for nestGenericIterREInterp*)
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

 
genInterpData[aVecFunc:(_Function|_CompiledFunction),@<gSpec@>]:=
With[{thePts=gridPts[gSpec]},
With[{filledPts=Map[
Function[xx,fillIn[{{},toIgnore,xx}]],thePts]},
With[{theVals=Map[
Function[xx,(Apply[aVecFunc,xx])],filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genInterpData"->#&,{"gridPts","fillIn"}]];



(*end code for genInterpData*)
@}


\begin{verbatim}
In[102]:= genInterpData[theFP,aGSpecFV]//InputForm

Out[102]//InputForm= 
{{{0., -0.03}, {{-0.03}, {-0.01714285714285714}, {8.326672684688674*^-19}, 
   {-0.03}}}, {{0., -0.010000000000000002}, {{-0.010000000000000002}, 
   {-0.005714285714285715}, {2.775557561562892*^-19}, 
   {-0.010000000000000002}}}, {{0., 0.009999999999999995}, 
  {{0.009999999999999995}, {0.005714285714285711}, {-2.77555756156289*^-19}, 
   {0.009999999999999995}}}, {{0., 0.029999999999999992}, 
  {{0.029999999999999992}, {0.017142857142857137}, {-8.326672684688672*^-19}, 
   {0.029999999999999992}}}, {{0.3333333333333333, -0.03}, 
  {{0.10333333333333333}, {0.05904761904761904}, {-2.359223926294482*^-18}, 
   {-0.03}}}, {{0.3333333333333333, -0.010000000000000002}, 
  {{0.12333333333333334}, {0.07047619047619047}, {1.526556661896894*^-18}, 
   {-0.009999999999999995}}}, {{0.3333333333333333, 0.009999999999999995}, 
  {{0.14333333333333334}, {0.08190476190476188}, {8.881784192348361*^-18}, 
   {0.010000000000000009}}}, {{0.3333333333333333, 0.029999999999999992}, 
  {{0.16333333333333333}, {0.09333333333333332}, {-2.195292561640384*^-17}, 
   {0.029999999999999985}}}, {{0.6666666666666666, -0.03}, 
  {{0.23666666666666666}, {0.1352380952380952}, {-6.409803268485097*^-18}, 
   {-0.03}}}, {{0.6666666666666666, -0.010000000000000002}, 
  {{0.25666666666666665}, {0.14666666666666667}, {9.428222052448997*^-18}, 
   {-0.009999999999999995}}}, {{0.6666666666666666, 0.009999999999999995}, 
  {{0.2766666666666666}, {0.15809523809523807}, {4.458239270769484*^-18}, 
   {0.009999999999999981}}}, {{0.6666666666666666, 0.029999999999999992}, 
  {{0.2966666666666667}, {0.16952380952380952}, {-5.290906515780026*^-19}, 
   {0.030000000000000013}}}, {{1., -0.03}, {{0.37000000000000005}, 
   {0.2114285714285714}, {2.886579861543865*^-17}, {-0.02999999999999997}}}, 
 {{1., -0.010000000000000002}, {{0.39}, {0.2228571428571429}, 
   {-3.6620012609148165*^-17}, {-0.009999999999999995}}}, 
 {{1., 0.009999999999999995}, {{0.41000000000000003}, {0.2342857142857143}, 
   {-3.27515792569444*^-17}, {0.009999999999999981}}}, 
 {{1., 0.029999999999999992}, {{0.43000000000000005}, {0.24571428571428572}, 
   {1.276756475630593*^-17}, {0.030000000000000013}}}}

\end{verbatim}

\subsection{parallelGenInterpData}
\label{sec:geninterpdata}



@d parallelGenInterpDataUsage
@{parallelGenInterpData::usage=
"place holder for parallelGenInterpData"
@}

@d parallelGenInterpData
@{
(*begin code for parallelGenInterpData*)

 
parallelGenInterpData[aVecFunc:(_Function|_CompiledFunction),@<gSpec@>]:=
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
		{1->Drop[longFuncArgs]}]
	]
]]]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["parallelMakeInterpFunc"->#&,{"fillInSymb","genInterpData","fillInSymb","getNumVars"}]];


(*end code for makeInterpFunc*)
@}
\begin{verbatim}
prinIn[103]:= {tm,tintf}=Timing[theFPInterp=makeInterpFunc[theFP,aGSpecFV]]

Out[103]= {0.028, Function[{fArgs1992, ig1994, fArgs1993}, 
 
>     {{InterpolatingFunction[{{0., 1.}, {-0.03, 0.03}}, <>][fArgs1992, 
 
>        fArgs1993]}, {InterpolatingFunction[{{0., 1.}, {-0.03, 0.03}}, <>][
 
>        fArgs1992, fArgs1993]}, 
 
>      {InterpolatingFunction[{{0., 1.}, {-0.03, 0.03}}, <>][fArgs1992, 
 
>        fArgs1993]}, {InterpolatingFunction[{{0., 1.}, {-0.03, 0.03}}, <>][
 
>        fArgs1992, fArgs1993]}}]}
\end{verbatim}
\begin{verbatim}
In[107]:= tintf @@ {0.2, ignored, 0.01}

Out[107]= {{0.09}, {0.0514286}, {6.84799 10   }, {0.01}}

\end{verbatim}
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
doIterREInterp[@<theSolver@>,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<gSpec@>,@<distribSpec@>]:=
With[{numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
With[{theFuncs=makeInterpFunc[genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],gSpec]},
With[{XZRE=genXZREInterpFunc[{numX,numEps,numZ},theFuncs,gSpec,distribSpec]},
{theFuncs,XZRE}]]]


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
parallelDoIterREInterp[@<theSolver@>,
	@<linMod@>,
	@<XZFuncs@>,
@<eqnsFunc@>,@<gSpec@>,@<distribSpec@>]:=
With[{numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
tn=AbsoluteTime[];
DistributeDefinitions[XZFuncs[[1]]];
DistributeDefinitions[eqnsFunc];
DistributeDefinitions[linMod];
With[{theFuncs=parallelMakeInterpFunc[genFPFunc[theSolver,linMod,XZFuncs,eqnsFunc],gSpec]},
Print["parallelMakeInterpTime=",(tn2=AbsoluteTime[])-tn];
With[{XZRE=parallelGenXZREInterpFunc[{numX,numEps,numZ},theFuncs,gSpec,distribSpec]},
Print["parallelgenXZREInterpTime=",(AbsoluteTime[])-tn2];
{theFuncs,XZRE}]]]



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


nestIterREInterp[@<theSolver@>,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,
@<gSpec@>,
@<distribSpec@>,numIters_Integer]:=
NestList[Function[xx,doIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,gSpec,distribSpec]],{99,XZFuncs[[1]]},numIters]


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


parallelNestIterREInterp[@<theSolver@>,@<linMod@>,
@<XZFuncs@>,@<eqnsFunc@>,
@<gSpec@>,
@<distribSpec@>,numIters_Integer]:=
NestList[Function[xx,parallelDoIterREInterp[theSolver,linMod,
{xx[[2]],numSteps},eqnsFunc,gSpec,distribSpec]],{ig,XZFuncs[[1]]},numIters]

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
  
AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genXZREInterpFunc"->#&,{"genXZFuncRE","makeInterpFunc","elimGSpecShocks"}]];

(*end code for genXZREInterpFunc*)
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
@<makeGenericInterpFuncsUsage@>
@<svmRegressionSigmoidUsage@>
@<svmRegressionRBFUsage@>
@<cnstrctFUsage@>
@<xNowUsage@>
@<theExpValsUsage@>
@<makeRBFKernelUsage@>
@<makePolynomialKernelUsage@>
@<makeSymbolicKernelUsage@>
@<dotProdKernelUsage@>
@<cnstrctExpKernUsage@>
@<svmRegressionLinearUsage@>
@<svmRegressionPolyUsage@>
@<getterSetterTestsUsage@>
@<callGraphUsage@>
@<smolyakInterpolationUsage@>
@<smolyakInterpolationPrepUsage@>
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
@}
@d usage definitions
@{


@<parallelGenXZREInterpFuncUsage@>
@<genSmolyakXZREInterpFuncUsage@>
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
@<writeExpKernUsage@>
@<doGenericIterREInterpUsage@>
@<nestGenericIterREInterpUsage@>
@}

\section{Package Code}
\label{sec:usage-definitions}

@d package code
@{
@<nestGenericIterREInterp@>
@<doGenericIterREInterp@>
@<ExpKernCode@>
@<writeExpKern@>
@<makeGenericInterpFuncs@>
@<parallelSmolyakGenInterpData@>
@<svmRegressionSigmoid@>
@<svmRegressionRBF@>
@<svmRegressionLinear@>
@<svmRegressionPoly@>
@<smolyakGenInterpData@>
@<smolyakInterpolation@>
@<smolyakInterpolationPrep@>
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
@}
@d usage definitions
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
@<getNumEps@>
@<genLilXkZkFunc@>
@<fSumC@>
@<fSum@>
@<genSlots@>
@<genXtOfXtm1@>
@<genXtp1OfXt@>
@<genX0Z0Funcs@>
@<multiStep@>
@}
@d usage definitions
@{
@<multiStepZ@>
@<multiStepX@>
@<checkLinMod@>
@<checkMod@>
@<genFRFunc@>
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
@{XZFuncs:{(_Function|_InterpolatingFunction|_CompiledFunction|_Symbol),numSteps_Integer}@}

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


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genLilXkZkFunc"->#&,{"genLilXkZkFunc"}]];


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

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genLilXkZkFunc"->#&,{"fSumC"}]];


@}


@d genLilXkZkFunc full call
@{genLilXkZkFunc[@<linMod@>,@<XZFuncs@>,@<xtGuess@>]@}

@d genLilXkZkFunc
@{
@<genLilXkZkFunc full call@>:=
@<XZ Functions Given@>

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genLilXkZkFunc"->#&,{"fSum"}]];

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
	xtGuess:{{_?NumberQ..}..}]:=
ConstantArray[0,{Length[psiZ],1}]

fSum[@<linMod@>,
	@<XZFuncs@>,xtGuess:{{_?NumberQ..}..}]:=
With[{numXVars=getNumX[linMod],numZVars=getNumZ[linMod]},
With[{xzRes=Apply[multiStepZ[XZFuncs,numXVars,numZVars,numSteps], Flatten[xtGuess]]},
fSumC[phi,FF,psiZ,xzRes]]]

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
genSlots[numVars]=
replaceMySlotStandIn[Table[{mySlotStandIn[ii]},{ii,numVars}]]]/;And[numVars>=0]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genSlots"->#&,{"replaceMySlotStandIn","mySlotStandin"}]];



replaceMySlotStandIn[xx_]:=xx/.mySlotStandIn->Slot



genSlot[slotNum_Integer]:=
Module[{},
genSlot[slotNum]=
replaceMySlotStandIn[mySlotStandIn[slotNum]]]/;And[slotNum>0]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genSlot"->#&,{"replaceMySlotStandIn","mySlotStandIn"}]];


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
Apply[Function, {Join[BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]}]]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genX0Z0Funcs"->#&,{"getNumX","getNumZ","genSlots"}]];

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
With[{X0Z0=genX0Z0Funcs[linMod],numZ=genNumZ[linMod]},
With[{lilxz=genLilXkZkFunc[linMod, {X0Z0,2}, Join[anX,anEps]]},
	{Eigenvalues[BB]//Abs,Eigenvalues[FF]//Abs,Apply[X0Z0,Flatten[anX]],Apply[lilxz,Flatten[Join[anX,anEps,Table[{0},{numZ}]]]]}]]

genNumZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
            psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} ,
anX_?MatrixQ,anEps_?MatrixQ]:=Length[psiZ[[1]]]




AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["chkLinMod"->#&,{"genLilXkZkFunc","genX0Z0Funcs"}]];


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
With[{X0Z0=genX0Z0Funcs[linMod],numX=getNumX[linMod],numEps=getNumEps[linMod],numZ=getNumZ[linMod]},
With[{lilxz=
genLilXkZkFunc[linMod, {X0Z0,1}, Join[anX,anEps]]},
With[{xzFuncNow=theSolver[[1]][{numX,numEps,numZ},lilxz,eqnsFunc,Method->"JenkinsTraub"]},
With[{fp=genFPFunc[theSolver,linMod,{X0Z0,2},eqnsFunc]},
{Apply[lilxz,Flatten[Join[anX,anEps,Table[0,{numZ}]]]],
Apply[xzFuncNow,Flatten[Join[anX,anEps]]],
Apply[fp,Flatten[Join[anX,anEps]]],
Apply[eqnsFunc,Flatten[Join[ss,{{0}}]]]
}]]]]


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["checkMod"->#&,{"genLilXkZkFunc","genX0Z0Funcs","getNumX","getNumEps","getNumZ","genFPFunc"}]];


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
Module[{},
With[{funcArgs=Flatten[genSlots[numX+numEps]],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
With[{zArgsInit=Map[Function[xx,{xx,0}],zArgs],funcName=Unique["fName"]},
funcName[theVars:{_?NumberQ..}]:=
Apply[eqnsFunc,Flatten[Apply[xkFunc,theVars]]];Off[FindRoot::nlnum];
With[{frRes=FindRoot[funcName[Join[funcArgs,zArgs]],zArgsInit],
xzRes=Drop[Apply[xkFunc,Join[funcArgs,zArgs]],numX][[Range[numX]]]},
If[Not[FreeQ[xzRes,$Failed]],Throw[$Failed,"xzRes"]];
With[{otherGuts=cmpXZVals[xzRes,zArgs,frRes]},
If[Not[Apply[And,Map[Or[MachineNumberQ[#],IntegerQ[#]]&,Cases[xzRes,_?NumberQ,Infinity]]]],
Throw[{$Failed,{xzRes,funcName[Join[funcArgs,zArgs]],zArgsInit}//InputForm},"otherGuts not machine number"]];
On[FindRoot::nlnum];
Function[otherGuts]]]]]]

(* input   [function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)
 
cmpXZVals[xzVals_?MatrixQ,theZArgs:{_Symbol..},theResult:{(_->_)..}]:=
Transpose[{Flatten[Join[xzVals,theZArgs]/.theResult]}]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["genFRFunc"->#&,{"genSlots","cmpXZVals"}]];



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

myNExpectation[funcName_Symbol[farg_List,idx_Integer],nArgs_List]:=Chop[NExpectation[funcName[farg,idx],nArgs]]


myExpectation[farg_List,nArgs_List]:=
Chop[Expectation[farg,nArgs]]






myNewNExpectation[fff_[fargs___],anEpsVar_\[Distributed] PerfectForesight]:=Module[{},Print["there",{(Apply[fff,{fargs}]),{fargs}/.anEpsVar->0}];(Apply[fff,{fargs}])/.anEpsVar->0]


myNewNExpectation[fff_[fargs___],distStuff_]:=Module[{},Print["jhere",{(Apply[fff,{fargs}]),{fargs}}];Chop[NExpectation[Apply[fff,{fargs}],distStuff]]]



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
iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction),
drExpFunc:(_Function|_CompiledFunction),initVec_?MatrixQ,numEps_Integer,numPers_Integer]:=
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

makeREIterFunc[drFunc:(_Function|_CompiledFunction),@<distribSpec@>]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[drFunc[[1]]]-numEps,numZ=0},
	genXZFuncRE[{numX,numEps,numZ},drFunc,distribSpec]]]

AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["makeREIterFunc"->#&,{"getNumEpsVars","genXZFuncRE"}]];

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
With[{pathNow=iterateDRREIntegrate[drFunc,drExpFunc,initVec,numEps,numPers],numX=Length[initVec]-numEps},Print["pathErrsDRREIntegrate:",pathNow];
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
restArgs=(Map[Function[xx,
doFuncArg[pathNow,Table[{0},{numEps}],numX,xx-2]],Range[3,numPers]])
},Print["args",{firstArg,restArgs}];
With[{first=Transpose[{Apply[eqnsFunc,Flatten[firstArg]]}]},
	With[{theRest=Map[Function[xx,Transpose[{(Apply[eqnsFunc,Flatten[xx]])}]],restArgs]
},
(*Print["pathErrs:",{pathNow,theRest,first}];*)
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
drFunc_Function,drExpFunc_Function,initVec_?MatrixQ,@<eqnsFunc@>]:=
phi . (pathErrsDRREIntegrate[drFunc,drExpFunc,
initVec,eqnsFunc,2])//First


AMASeriesRepCallGraph=
Join[AMASeriesRepCallGraph,Map["evalPathErrorDRREIntegrate"->#&,{"pathErrsDRREIntegrate"}]];


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
evalBadPathErrDRREIntegrate[drFunc_Function,drExpFunc_Function,
noEpsVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
	With[{theVal=evalPathErrDRREIntegrate[drFunc,drExpFunc,Join[noEpsVec,Transpose[{tryEps}]],numEps,eqnsFunc]},Print["fromgeneratedfunc:",theVal];
		With[{theNorm=Norm[theVal,Infinity]},
		(*Print["stillex:",{tryEps,theVal,Norm[theVal,Infinity],theNorm}];*)theNorm]];
	With[{outerEVars=Table[Unique["eVs"],{numEps}]},
	With[{maxArgs=Map[Function[xx,{xx,0}],outerEVars],cons=Apply[And,  (Map[Function[xx,(-0.01<=xx<=0.01)], outerEVars])]},
	FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]


evalBadPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,drExpFunc_Function,
noEpsVec_?MatrixQ,numEps_Integer,@<eqnsFunc@>]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
With[{theVal=evalPathErrDRREIntegrate[drFunc,drExpFunc,Join[noEpsVec,Transpose[{tryEps}]],numEps,eqnsFunc]},
		(*Print["otherex:",theVal,Norm[theVal,Infinity]];*)Norm[theVal,Infinity]];
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
	With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},Print["fminres",fMinRes];
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
\bibliography{files.bib}

\end{document}
