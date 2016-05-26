(* Mathematica Package *)

(* Created by the Wolfram Workbench Jul 27, 2015 *)

Print["reading AMASeriesRepresentation`"]

BeginPackage["AMASeriesRepresentation`", {"JLink`","ProtectedSymbols`",
	"DifferentialEquations`InterpolatingFunctionAnatomy`"}]

	
$noTransFunc::usage="transfuncinfo";
	
$transFuncHasShocks::usage="transfuncinfo";
	
$transFuncNoShocks::usage="transfuncinfo";



makeDREvalInterp::usage="makeDREValInterp[drFunc_Function,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..}}]"


X0Z0::usage="from genX0Z0Funcs[linMod];"

evalExpctPathErrDRREIntegrate::usage="evalExpctPathErrDRREIntegrate[drFunc_Function,initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]"

genZsRE::usage="genZsRE[anHmat_?MatrixQ,PsiEps_?MatrixQ,PsiC_?MatrixQ,theDRFunc:(_Function|_CompiledFunction),initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theSysFunc:(_Function|_CompiledFunction),iters_Integer]"


PerfectForesight::usage="degenerate distribution implementing perfect foresight"
makeFunc::usage="makeFunc[funcArgsNow_List,numX_Integer,{theS_Function,thePairs:{{(_Function|CompiledFunction),(_Function|CompiledFunction)}..}}]"



(*usage updated*)
fSum::usage="fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:{_Function..},xtGuess_?MatrixQ]

returns matrix of the F weigthed sum of z's"

fSumC::usage="fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},...]


returns matrix of the F weigthed sum of z's
"

genPath::usage="genPath[xzFunc_Function,
XZFuncs:{_Function..},xtm1Val_?MatrixQ,epsVal_?MatrixQ]

generate a path using the xz and XZ funcs

returns a matrix of the path values

"


pathErrs::usage="pathErrs[{numX_Integer,numEps_Integer,numZs_Integer},
{lilXZFunc_Function,bigXZFuncs:{_Function..}},eqnsFunc:(_Function|_CompiledFunction),
anX_?MatrixQ,anEps_?MatrixQ,X0Z0_Function]

drops the first bigXZFunc which by construction is the expected value of the lilXZFunc and tacks on X0Z0 to compute a path
returns a matrix of equation errors along the path
"

checkMod::usage="checkMod[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},anX_?VectorQ,anEps_?VectorQ,
eqnsFunc:(_Function|_CompiledFunction)]


"



genXZFuncPF::usage="genXZFuncPF[{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function]

given problem dimensions and xz functions 
returns  perfect foresight version functions for future unconditional expectations recalculates all values along the path
"

genXZFuncRE::usage="genXZFuncRE[{numX_Integer,ignored_Integer,numZ_Integer},
aLilXkZkFunc_Function,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]


given problem dimensions and xz functions 
returns  perfect foresight version functions for future unconditional expectations using interpolation to avoid recalculating all values along the path

"

genXZFuncPFInterp::usage="genXZFuncPFInterp[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]



given problem dimensions and xz functions 
returns  rational expectations version functions for future unconditional expectations recalculates all values along the path

"

genXZFuncREInterp::usage="genXZFuncREInterp[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]


given problem dimensions and xz functions 
returns  retional expectations version functions for future unconditional expectations using interpolation to avoid recalculating all values along the path


"
nestIterPF::usage="nestIterPF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),numIters_Integer]

recursively apply doIterPF numIters times"

nestIterPFInterp::usage="nestIterPFInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction)..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},numIters_Integer]

recursively apply doIterPFInterp numIters times
"

nestIterRE::usage="nestIterRE[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numIters_Integer]

recursively apply doIterRE numIters times
"

nestIterREInterp::usage="nestIterREInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction)..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numIters_Integer]

recursively apply doIterREInterp numIters times
"


doIterPF::usage="doIterPF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction)]

given a reference linear model, (x,z,X,Z) function and an initial guess for the time t state and equation system
extends (x,z,X,Z) an additional period using perfect foresight and recalculation of all the points along the path

returns a pair of functions xz and XZ giving the xt values of x and z and giving the path of future values of X and Z
"


doIterPFInterp::usage="doIterPFInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction)..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]


given a reference linear model, (x,z,X,Z) function and an initial guess for the time t state and equation system
extends (x,z,X,Z) an additional period using perfect foresight using interpolation to avoid recalculation of all the points along the path

returns a pair of functions xz and XZ giving the xt values of x and z and giving the path of future values of X and Z

"


doIterRE::usage="doIterRE[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]


given a reference linear model, (x,z,X,Z) functions and an initial guess for the time t state and an equation system the function
extends (x,z,X,Z) an additional period imposing rational expectations along with recalculation of all the points along the path

returns a pair of functions xz and XZ giving the xt values of x and z and giving the unconditional expected path of future values of x and z


"


doIterREInterp::usage="

doIterPF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction)]




given a reference linear model, (x,z,X,Z) function and an initial guess for the time t state and equation system
extends (x,z,X,Z) an additional period imposing rational expectations using interpolation to avoid recalculation of all the points along the path

returns a pair of functions xz and XZ giving the xt values of x and z and giving the unconditional expected path of future values of x and z
"



genX0Z0Funcs::usage="genX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]
returns functions  expected X0(xtm1,eps) and Z0(xtm1,eps)
"
genx0z0Funcs::usage="genX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]
returns functions x0(xtm1,eps) and z0(xtm1,eps)
"

truncErrorMat::usage="truncErrorMat[{{fmat,_Real,2},{phimat,_Real,2},{kk,_Integer}}]

computes the truncation error matrix

"



pathErrsDRPF::usage="pathErrsDRPF[drFunc_Function,initVec_?VectorQ,numEps_Integer,eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]

uses decision rule to compute a perfect foresight path of length numPers, then applies eqnsFunc along the path
returns List of vectors of equations errors
"


pathErrsDRREIntegrate::usage="pathErrsDRREIntegrate[drFunc_Function,initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]

uses decision rule to compute an unconditional expectations path of length numPers, then applies eqnsFunc along the path
returns List of vectors of equations errors
"



evalPathErrDRREIntegrate::usage="evalPathErrDRREIntegrate[drFunc_Function,initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]

uses decision rule to compute an unconditional expectations path of length 2, then applies eqnsFunc 
returns a list containing  1 vector of equations errors

"



evalBadPathErrDRREIntegrate::usage="evalBadPathErrDRREIntegrate[drFunc_Function,noEpsVec_?VectorQ,
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]

given the initial state vector and the decision rule uses FindMaximum to find the time t shock that produces the largest infinity norm for the error in the
system equations
returns the infintiy norm of the error and the shock value
"


genZsREWorst::usage="genZsREWorst[theDRFunc:(_Function|_CompiledFunction),initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},
	theSysFunc:(_Function|_CompiledFunction),iters_Integer]
	
	iterates the decision rule using iterateDRREIntegrate
	applies worstPathForErrDRREIntegrate all along the path to return a list of
	vectors of maximum infinity norm errors in the sysFunc equations
	"

genSeriesRepFunc::usage="genSeriesRepFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),terms_Integer]
	
generates  Zs using genZsREExact, then 
returns a function that 
uses genASeriesRep to generate the value for  {xtm1,xt,xtp1,eps} from the the series representation with the given number of terms 

"


genSeriesReps::usage="genSeriesReps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),maxIters_Integer]
	
generates  Zs using genZsREExact, then uses genASeriesRep to generates a list of the possible  values for {xtm1,xt,xtp1,eps} from the series representations using 
from just the first z matrix to all the z's
"

genZsFromPath::usage="genZsFromPath[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
thePath_?VectorQ,theEps_?VectorQ]

    given the path
	computes the z values using the hmat in linMod
	returns a list of z matrices(one column matrices)

"




genZsREExact::usage="genZsREExact[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),iters_Integer]
	
	iterates a decision rule forward using iterateDRIntegrate
	computes the z values using the hmat in linMod
	returns a list of z matrices(one column matrices)
	"


iterateDRPF::usage=" 
iterateDRPF[drFunc_Function,initVec_?VectorQ,numEps_Integer,numPers_Integer]
Iterates a decision rule forward from initVec(includes time zero shocks) for numPers time intervals assuming 
all future shocks  zero
produces a matrix containing the path
"


iterateDRREIntegrate::usage="iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction),initVec_?VectorQ,
	distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numPers_Integer]
	
Iterates a decision rule forward from initVec(includes time zero shocks) for numPers time intervals by integrating out the shocks 
using the distribution
and, if not {},the regime change transition probablities

produces a matrix containing the path"


checkLinMod::usage="checkLinMod[linMod,anX_?VectorQ,anEps_?VectorQ,numRegimes_Integer:0]
computes eigenvalues and checks consistency of dimensions of model components
returns {Eigenvalues[BB]//Abs,Eigenvalues[FF]//Abs,X0Z0 @@anX,lilxz @@Join[anX,anEps,Table[0,{numZ}]]}
numRegimes currently has no impact
"



getB::usage="getB[linMod] B from linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}"
getF::usage="getF[linMod] F from linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}"
getPhi::usage="getPhi[linMod] Phi from linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}"


Begin["Private`"]

getB[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
BB
getF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
FF

getPhi[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
phi

checkLinMod[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
anX_?VectorQ,anEps_?VectorQ,numRegimes_Integer:0]:=
With[{X0Z0=genX0Z0Funcs[linMod],numZ=Length[psiZ[[1]]]},
With[{lilxz=genLilXkZkFunc[linMod, {X0Z0}, Transpose[{Join[anX,anEps]}]]},
	{Eigenvalues[BB]//Abs,Eigenvalues[FF]//Abs,X0Z0 @@anX,lilxz @@Join[anX,anEps,Table[0,{numZ}]]}]]



iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction),initVec_?VectorQ,
	distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec],firVal=drFunc @@ initVec},
	With[{numX=Length[initVec]-numEps,iterFunc=makeREIterFunc[drFunc,distribSpec]},
With[{iterated=
NestList[((*Print[#//InputForm];*)(Transpose[{Flatten[iterFunc @@ Flatten[#]]}]))&,firVal,numPers-1]},
Join[Transpose[{initVec}][[Range[numX]]],Join @@ (Identity[#[[Range[numX]]]]&/@iterated)]]]]/;
And[numPers>0]


 

genZsREExact[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),iters_Integer]:=
Module[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[initVec]-numEps,
 	thePath=Flatten[iterateDRREIntegrate[theExactDR,initVec,distribSpec,iters+1]]},(*Print["done thePath"];*)
 	With[{firstVal=theHMat .thePath[[Range[3*numX]]]- psiC - psiEps . Take[initVec,-numEps]},
 		With[{restVals=
      (theHMat .thePath[[Range[3*numX]+numX*#]] -psiC)&/@Range[(Length[thePath]/numX)-3]},
      Join[{firstVal},restVals]
]]]]

genZsFromPath[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	thePath_?VectorQ,theEps_?VectorQ]:=
		Module[{numX=Length[theHMat],theMatPath=Transpose[{thePath}]},
 			   With[{firstVal=theHMat .theMatPath[[Range[3*numX]]]-
							  psiC - psiEps . Transpose[{theEps}]},
 					With[{restVals=
						  (theHMat .theMatPath[[Range[3*numX]+numX*#]] -psiC)&/@
								  Range[(Length[thePath]/numX)-3]},
     Join[{firstVal},restVals]]]]

genSeriesReps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),maxIters_Integer]:=
With[{theZs=genZsREExact[linMod,initVec,distribSpec,theExactDR,maxIters]},
	genASeriesRep[linMod,initVec,theZs,#]&/@Range[Length[theZs]]]
	
genSeriesRepFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),terms_Integer]:=
Module[{numX=Length[BB[[1]]],numEps=Length[psiEps[[1]]]},
With[{theArgs=Table[Unique["funArgs"],{numX+numEps}]},
With[{theFunc=
Function[xxxx,	
With[{theZs=genZsREExact[linMod,xxxx,distribSpec,theExactDR,terms]},
	genASeriesRep[linMod,xxxx,theZs,terms]]]},
With[{xxxxPos=Position[theFunc,xxxx$]},
ReplacePart[theFunc,
	xxxxPos->theArgs
]]]]]




genZsREWorst[theDRFunc:(_Function|_CompiledFunction),initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},
	theSysFunc:(_Function|_CompiledFunction),iters_Integer]:=
Module[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[initVec]-numEps,
 	thePath=Flatten[iterateDRREIntegrate[theDRFunc,initVec,distribSpec,iters+1]]},Print["done thePath"];
With[{worsePaths=
  worstPathForErrDRREIntegrate[theDRFunc,thePath[[Range[numX]+numX*(#)]],distribSpec,theSysFunc]&/@
				       Range[(Length[thePath]/numX)-1]},Print["done worstPaths",worsePaths,Range[(Length[thePath]/numX)-1]];
      theSysFunc @@ Flatten[#]&/@worsePaths]
]]



   
evalBadPathErrDRREIntegrate[drFunc_Function,noEpsVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
	With[{theVal=evalPathErrDRREIntegrate[drFunc,Join[noEpsVec,tryEps],distribSpec,eqnsFunc]},
		(*Print["ex:",theVal,Norm[theVal,Infinity]];*)Norm[Transpose[theVal],Infinity]];
	With[{outerEVars=Table[Unique["eVs"],{getNumEpsVars[distribSpec]}]},
	With[{maxArgs={#,0}&/@outerEVars,cons=And @@  ((-0.01<=#<=0.01)&/@ outerEVars)},
	FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]

evalPathErrDRREIntegrate[drFunc_Function,initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
pathErrsDRREIntegrate[drFunc,initVec,distribSpec,eqnsFunc,2]


pathErrsDRREIntegrate[drFunc_Function,initVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{pathNow=iterateDRREIntegrate[drFunc,initVec,distribSpec,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Flatten[Reverse[initVec[[-Range[numEps]]]]],numX,0],
	restArgs=(doFuncArg[pathNow,Table[0,{numEps}],numX,#-2]&/@Range[3,numPers])},
With[{first=eqnsFunc@@firstArg},
	With[{theRest=(eqnsFunc@@#)&/@restArgs},
		Prepend[theRest,first]
]]]]]/;
And[numPers>1]
 
iterateDRPF[drFunc_Function,initVec_?VectorQ,numEps_Integer,numPers_Integer]:=
With[{firVal=drFunc @@ initVec,numX=Length[initVec]-numEps,theZeros=Table[0,{numEps}]},
With[{iterated=
NestList[(drFunc @@ Flatten[Append[#[[Range[numX]]],theZeros]])&,firVal,numPers-1]},
Join[Transpose[{initVec}][[Range[numX]]],Join @@ (#[[Range[numX]]]&/@iterated)]]]/;
And[numPers>0]

   
 
pathErrsDRPF[drFunc_Function,initVec_?VectorQ,numEps_Integer,eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]:=
With[{pathNow=iterateDRPF[drFunc,initVec,numEps,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Flatten[Reverse[initVec[[-Range[numEps]]]]],numX,0],
	restArgs=(doFuncArg[pathNow,Table[0,{numEps}],numX,#-2]&/@Range[3,numPers])},
With[{first=eqnsFunc@@firstArg},
	With[{theRest=(eqnsFunc@@#)&/@restArgs},
		Prepend[theRest,first]
]]]]/;
And[numPers>1]

  

pathErrs[{numX_Integer,numEps_Integer,numZs_Integer},
{lilXZFunc_Function,bigXZFuncs:{_Function..}},eqnsFunc:(_Function|_CompiledFunction),
anX_?MatrixQ,anEps_?MatrixQ,X0Z0_Function]:=
With[{aPath=genPath[lilXZFunc,Append[Drop[bigXZFuncs,1],X0Z0],
anX,anEps]},
With[{useEps={eqnsFunc @@ Flatten[Append[
aPath[[Range[3*numX]]],anEps]]}},
Join[useEps,
Map[(eqnsFunc @@ Append[
Flatten[aPath[[numX*(#-1)+Range[3*numX]]]],0])&,
Range[2,Length[bigXZFuncs]]]]]]




truncErrorMat=
Compile[{{fmat,_Real,2},{phimat,_Real,2},{kk,_Integer}},
With[{dim=Length[fmat]},
If[kk==0,Inverse[IdentityMatrix[dim] - fmat].phimat,
Inverse[IdentityMatrix[dim] - fmat] . MatrixPower[fmat,kk].phimat]]]


genX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars]},
With[{compArgs=xtm1Vars},
Function @@ {compArgs,Join[BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]}]]]


genx0z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]],numEpsVars=Length[psiEps[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars],epsVars=genEpsVars[numEpsVars]},
With[{compArgs=Join[xtm1Vars,epsVars]},
Function @@ {compArgs,Join[BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC+phi . psiEps . epsVars,ConstantArray[0,{numZVars,1}]]}]]]






fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},{},xtGuess_?MatrixQ]:=
ConstantArray[0,{Length[psiZ],1}]

fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xzRes=#[[numXVars+Range[numZVars]]]&/@(Drop[FoldList[#2@@(Flatten[#1][[Range[numXVars]]])&,
xtGuess,XZFuncs],1])},
fSumC[phi,FF,psiZ,xzRes]]]
getZtAfterXt[vecOrMat:(_?VectorQ|_?matrixQ),numX_Integer]:=vecOrMat[[numX+Range[numX]]]




fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},
With[{numXVars=Length[psiZ]},
With[{fPows=Drop[NestList[FF.#&,IdentityMatrix[numXVars],Length[zPath]],-1]},
Plus @@
MapThread[Dot[#1,phi.psiZ.#2]&,{fPows , zPath}]]]]




genPath[xzFunc_Function,
XZFuncs:{_Function..},xtm1Val_?MatrixQ,epsVal_?MatrixQ]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=xzFunc @@ Flatten[Join[xtm1Val,epsVal]]},
With[{xzRes=FoldList[(#2@@(Flatten[#1][[Range[numXVars]]]))[[Range[numXVars]]]&,
xtVal[[Range[numXVars]]],XZFuncs]},Join[xtm1Val,Join @@xzRes]]]]
(*eqnsfuncs func of xtm1,xt,xtp1,eps  returns discrep*)
(*xkfunc func of xtm1, eps zs returns xtm1,xt,xtp1,eps as matrices*)


doIterPF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction)]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=genFPFunc[linMod,XZFuncsNow,xtGuess,eqnsFunc]},
{theFuncs,Prepend[XZFuncsNow,genXZFuncPF[{numX,numEps,numZ},theFuncs]]}]]


doIterPFInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction)..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=makeInterpFunc[genFPFunc[linMod,XZFuncsNow,xtGuess,eqnsFunc],gSpec]},
{theFuncs,Prepend[XZFuncsNow,genXZFuncPFInterp[{numX,numEps,numZ},theFuncs,gSpec]]}]]



doIterRE[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=genFPFunc[linMod,XZFuncsNow,xtGuess,eqnsFunc]},
{theFuncs,Prepend[XZFuncsNow,genXZFuncRE[{numX,numEps,numZ},theFuncs,distribSpec]]}]]


doIterREInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction)..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=makeInterpFunc[genFPFunc[linMod,XZFuncsNow,xtGuess,eqnsFunc],gSpec]},
{theFuncs,Prepend[XZFuncsNow,genXZFuncREInterp[{numX,numEps,numZ},theFuncs,gSpec,distribSpec]]}]]



nestIterPF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),numIters_Integer]:=
NestList[doIterPF[linMod,#[[2]],xtGuess,
eqnsFunc]&,{ig,XZFuncsNow},numIters]


nestIterPFInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction)..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},numIters_Integer]:=
NestList[doIterPFInterp[linMod,#[[2]],xtGuess,
eqnsFunc,gSpec]&,{ig,XZFuncsNow},numIters]


nestIterRE[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numIters_Integer]:=
NestList[doIterRE[linMod,#[[2]],xtGuess,
eqnsFunc,distribSpec]&,{ig,XZFuncsNow},numIters]


nestIterREInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction)..},
xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numIters_Integer]:=
NestList[doIterREInterp[linMod,#[[2]],xtGuess,
eqnsFunc,gSpec,distribSpec]&,{ig,XZFuncsNow},numIters]


    
genXZFuncPF[{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX}],
theZeroes=Table[0,{numEps}]},
With[{theFuncNow=
ReplacePart[
Function[xxxx,aLilXkZkFunc@@Join[funcArgs,theZeroes]],
1->funcArgs]},
	theFuncNow
]]


	

genXZFuncRE[{numX_Integer,ignored_Integer,numZ_Integer},
aLilXkZkFunc_Function,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{intVarRes=genIntVars[numX,distribSpec],
funcName=Unique["fName"],numRegimes=getNumRegimes[distribSpec]},
funcName[fNameArgs:{_?NumberQ..},idx_Integer]:=Module[{},
(aLilXkZkFunc@@ fNameArgs)[[idx,1]]];
With[{funcGuts=
Switch[getRegimeTransProbFuncType[distribSpec],
	$noTransFunc,Function[xxxx,Module[{},
	Transpose[{myNExpectation[
	(funcName[intVarRes[[2]],#]),intVarRes[[3]]]&/@Range[numX+numZ]}]]],
	$transFuncNoShocks,Function[xxxx,Module[{},
	Sum[(getProbFunc[distribSpec] @@
		Append[intVarRes[[2]],ii-1])*
	Transpose[{myNExpectation[(*Print["curious:",{getProbFunc[distribSpec] @@Append[intVarRes[[2]],ii-1],intVarRes[[2]],ii-1,#,(funcName[Append[intVarRes[[2]],ii-1],#])}];*)
	(funcName[Append[intVarRes[[2]],ii-1],#]),intVarRes[[3]]]&/@Range[numX+numZ]}],{ii,numRegimes}]]],
	$transFuncHasShocks,Function[xxxx,Module[{},
	Transpose[{myNExpectation[(*Print["curious:",{getProbFunc[distribSpec] @@Append[intVarRes[[2]],ii-1],intVarRes[[2]],ii-1,#,(funcName[Append[intVarRes[[2]],ii-1],#])}];*)
	Sum[(getProbFunc[distribSpec] @@
		Append[intVarRes[[2]],ii-1])*(funcName[Append[intVarRes[[2]],ii-1],#]),{ii,numRegimes}],intVarRes[[3]]]&/@Range[numX+numZ]}]]]		
	]},
	ReplacePart[funcGuts,1->intVarRes[[1]]]]]



checkMod[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},anX_?VectorQ,anEps_?VectorQ,
eqnsFunc:(_Function|_CompiledFunction)]:=
With[{X0Z0=genX0Z0Funcs[linMod],numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{lilxz=
genLilXkZkFunc[linMod, {X0Z0}, Transpose[{Join[anX,anEps]}]]},
With[{fr=genFRFunc[{numX,numEps,numZ},lilxz,eqnsFunc]},
With[{fp=genFPFunc[linMod,{X0Z0},{{}},eqnsFunc]},
{lilxz @@ Join[anX,anEps,Table[0,{numZ}]],
fr @@ Join[anX,anEps],
fp @@Join[anX,anEps]
}]]]]




(* the following are all private*)

getProbFunc[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
If[regimeTransProbFunc=={},Function[1],regimeTransProbFunc[[3]]]

getNumRegimes[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
If[regimeTransProbFunc=={},0,regimeTransProbFunc[[1]]]

getDistribs[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:= Last/@expctSpec
(*{numReg,tranType,tranFunc}*)
getRegimeTransProbFuncType[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
If[regimeTransProbFunc=={},$noTransFunc,regimeTransProbFunc[[2]]]
getRegimeTransProbFunc[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=regimeTransProbFunc[[3]]
getNumEpsVars[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=Length[expctSpec]





makeFunc[funcArgsNow_List,numX_Integer,{theS_Function,
thePairs:{{(_Function|CompiledFunction),(_Function|CompiledFunction)}..}}]:=
With[{xtPos=Range[numX]+2*numX},
With[{preArgs=
(Function[xxxx,With[{indx=(theS@@xxxx+1)},
thePairs[[indx,1]]@@xxxx + 
(thePairs[[indx,2]]@@xxxx).(xxxx[[xxxxXtPos]])]])},
With[{xxxxLocs=Position[preArgs,xxxx$],
xxxxXtPos=Position[preArgs,xxxxXtPos]},
ReplacePart[preArgs,{xxxxLocs->funcArgsNow,xxxxXtPos->xtPos}]]]]

  
makeFunc[funcArgsNow_List,numX_Integer,
thePair:{(_Function|CompiledFunction),(_Function|CompiledFunction)}]:=
With[{xtPos=Range[numX]+2*numX},
With[{preArgs=
(Function[xxxx,
thePair[[1]]@@xxxx + 
(thePair[[2]]@@xxxx).(xxxx[[xxxxXtPos]])])},
With[{xxxxLocs=Position[preArgs,xxxx$],
xxxxXtPos=Position[preArgs,xxxxXtPos]},
ReplacePart[preArgs,{xxxxLocs->funcArgsNow,xxxxXtPos->xtPos}]]]]


 
 
gridPts[rngs:{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0]:=
With[{funcForPts=(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]] @#) &},
With[{oneDimPts=funcForPts/@rngs},
	With[{maybeRegimes=If[numRegimes==0,oneDimPts,
		Prepend[Append[oneDimPts,Range[0,numRegimes-1]],Range[0,numRegimes-1]]]},
With[{theOuter=Outer[List,Sequence@@#]&[maybeRegimes]},
Flatten[theOuter,Depth[theOuter]-3]]]]]


oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
If[iPts==0,{{(xLow+xHigh)2}},
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]]/;iPts>=0


fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
	If[MemberQ[toIgnore,Length[theRes]+1],fillIn[{Append[theRes,1],Drop[toIgnore,1],shortVec}],
		fillIn[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]

fillInSymb[{theRes:{___},toIgnore:{_Integer...},shortVec:{___}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
	If[MemberQ[toIgnore,Length[theRes]+1],fillInSymb[{Append[theRes,Unique["ig"]],Drop[toIgnore,1],shortVec}],
		fillInSymb[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]

fillInSymb[{theRes:{___},toIgnore:{_Integer...},shortVec:{___}}]:=
fillInSymb[{theRes,Sort[toIgnore],shortVec}]

fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
fillIn[{theRes,Sort[toIgnore],shortVec}]


Print["makeInterpFunc not generic, tied to RBC"];
makeInterpFunc[aVecFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{interpData=genInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
	With[{numFuncs=Length[interpData[[1,2]]],funcArgs=Table[Unique["fArgs"],{numArgs}]},
	With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}],
		interpFuncList=
	Function[funcIdx,Interpolation[{#[[1]], #[[2, funcIdx, 1]]} & /@ 
		interpData,InterpolationOrder -> iOrd]]/@Range[numFuncs]},
	(*	Print[	Function[xxxxxxx, Transpose[{Through[interpFuncList@@yyyyyyy]}]]//InputForm];*)
	ReplacePart[
	Function[xxxxxxx, Transpose[{Through[interpFuncList@@yyyyyyy]}]],
		{1->longFuncArgs,{2, 1, 1, 1, 2}->funcArgs}]/.{xxxxxxx$->longFuncArgs}
	]
]]



 
genInterpData[aVecFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{thePts=gridPts[getGridPtTrips[gSpec],numRegimes]},
With[{interpData=Map[{#,aVecFunc@@fillIn[{{},toIgnore,#}]}&,thePts]},
interpData]]



Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]


makeREIterFunc[drFunc:(_Function|_CompiledFunction),distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[drFunc[[1]]]-numEps,numZ=0},
	genXZFuncRE[{numX,numEps,numZ},drFunc,distribSpec]]]



myNExpectation[funcName_Symbol[funcArgs_List,idx_Integer],anEpsVar_\[Distributed] PerfectForesight]:=
funcName@@Append[ReplacePart[{funcArgs},{{(1),(-1)}->0}],idx]
myNExpectation[funcName_Symbol[funcArgs_List,idx_Integer],{anEpsVar_\[Distributed] PerfectForesight}]:=
funcName@@Append[ReplacePart[{funcArgs},{{1,(-1)}->0}],idx]

myNExpectation[funcName_Symbol[farg_List,idx_Integer],nArgs_List]:=Chop[NExpectation[funcName[farg,idx],nArgs]]

genASeriesRep[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	initVec_?VectorQ,theZs:{_?MatrixQ..},len_Integer]:=
	Module[{theZFuncs = Function @@ {{}, Join[Table[{0}, {Length[BB[[1]]]}], #]} & /@ 
   Drop[theZs[[Range[len]]], 1]},(*Print["theZFuncs",theZFuncs];*)
   With[{maybe = genLilXkZkFunc[linMod,theZFuncs, Table[{0},{Length[BB]}]]},(*	Print["zzts",Join[initVec,theZs[[1]]]];*)
   	maybe@@ Join[initVec,Flatten[theZs[[1]]]]]]



worstPathForErrDRREIntegrate[drFunc_Function,noEpsVec_?VectorQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{fMinRes=evalBadPathErrDRREIntegrate[drFunc,noEpsVec,distribSpec,eqnsFunc]},
	With[{badEps=(First/@fMinRes[[2]])/.fMinRes[[2]]},
	With[{badPath=iterateDRREIntegrate[drFunc,Join[noEpsVec,badEps],distribSpec,2]},
		Join[badPath,Transpose[{badEps}]]]]]



makeDREvalInterp[drFunc_Function,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{xEpsArgs=Table[Unique["xeArgs"],{Length[toIgnore]+getNumVars[gSpec]}]},
	With[{preInterp=
	ReplacePart[
	Function[xxxxxx,  
  Transpose[
     evalPathErrDRREIntegrate[drFunc, #, distribSpec,eqnsFunc]] & @xxxxxx],{1->xEpsArgs,{2,1}->xEpsArgs}]},
     {makeInterpFunc[preInterp,gSpec],preInterp}
]]

doFuncArg[pathNow_?MatrixQ,epsVals_?VectorQ,numX_Integer,oSet_Integer]:=
With[{firstArg=Join[Flatten[pathNow[[oSet*numX+Range[3*numX]]]],Flatten[epsVals]]},
firstArg]



genXtm1Vars[numVars_Integer]:=
Module[{},
genXtm1Vars[numVars]=
Table[
makeProtectedSymbol["xxxtm1Var$"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]


genEpsVars[numShocks_Integer]:=
Module[{},
genEpsVars[numShocks]=
Table[
makeProtectedSymbol["epsVar$"<>ToString[ii]],{ii,numShocks}]]/;And[numShocks>=0]


genZVars[horizons_Integer,numConstr_Integer]:=
genZVars[horizons,numConstr,0]
	
genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Module[{},
genZVars[horizons,numConstr,offset]=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]]/;offset<=0


genZVars[numConstr_Integer]:=
Reverse[Flatten[genZVars[0,numConstr]]](*
Module[{},
genZVars[numConstr]=
Table[
makeProtectedSymbol["zzzVar$"<>ToString[ii]],{ii,numConstr}]]*)/;And[numConstr>=0]


(*funxzfunc of xtm1vars,epsvars,zvars and a guess for xt*)
genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncs:{_Function...},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],
numEpsVars=Length[psiEps[[1]]],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars],
epsVars=genEpsVars[numEpsVars],
zVars=Reverse[Flatten[genZVars[0,numZVars]]]/.name_[t]->name},
With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
ReplacePart[
Function[xxxx,
With[{xtVals=BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . Transpose[{epsVars}]+
phi . psiZ . Transpose[{zVars}] +FF.fCon},
Join[Transpose[{xtm1Vars}],xtVals,
BB.xtVals+Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC+fCon,
Transpose[{epsVars}]]]],
1->Join[xtm1Vars,epsVars,zVars]
]]]]

getXt[vecOrMat:(_?VectorQ|_?matrixQ),numX_Integer]:=vecOrMat[[Range[numX]]]



genPathCompare[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	xzFunc_Function,XZFuncs:{_Function..},xtm1Val_?MatrixQ,epsVal_?MatrixQ]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=xzFunc @@ Flatten[Join[xtm1Val,epsVal]]},
With[{xzRes=FoldList[(#2@@(Flatten[#1][[Range[numXVars]]]))[[Range[numXVars]]]&,
xtVal[[Range[numXVars]]],XZFuncs]},{Join[xtm1Val,Join @@xzRes],
compareFormula[linMod,XZFuncs,xtm1Val,epsVal,xtVal]}
]]]

compareFormula[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncs:{_Function..},xtm1Vars_?MatrixQ,epsVars_?MatrixQ,xtztVal_?MatrixQ]:=
	With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
		With[{xtVal=xtztVal[[Range[numX]]],ztVal=xtztVal[[numX+Range[numZ]]]},
With[{fCon=fSum[linMod,XZFuncs,xtVal]},
	With[{xtVals=BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . epsVars+
phi . psiZ . ztVal +FF.fCon},
Join[xtm1Vars,xtVals,
BB.xtVals+Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC+fCon,
epsVars]]]]]


(*returns function of xtm1 eps that gives xt and z*)
 
genFRFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc:(_Function|_CompiledFunction)]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX+numEps}],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
With[{zArgsInit={#,0}&/@zArgs,funcName=Unique["fName"]},
funcName[funcArgsNot:{_?NumberQ..}]:=
Module[{theVars=Join[funcArgsNot]},(*Print["genFRFunc func",theVars,Flatten[xkFunc@@theVars]];*)
eqnsFunc@@(Flatten[xkFunc@@theVars])];
ReplacePart[
Function[xxxx,With[{zVals=zArgs/.FindRoot[funcName@Join[funcArgs,zArgs],zArgsInit]},
Join[(xkFunc@@Join[funcArgs,zVals])[[numX+Range[numX]]],
Transpose[{zVals}]]]],
1->funcArgs]]]
(* input   [function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)


$fixedPointLimit=30;
genFPFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:{_Function..},xtGuess_?MatrixQ,eqnsFunc:(_Function|_CompiledFunction)]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}]},
ReplacePart[
Function[xxxx,Sow[
FixedPoint[With[{xzFuncNow=
genFRFunc[{numX,numEps,numZ},genLilXkZkFunc[linMod,XZFuncs,#[[Range[numX]]]],
eqnsFunc]},(*Print["infp:",XZFuncs[[1]]@@funcArgs];*)
xzFuncNow @@funcArgs]&,(XZFuncs[[1]]@@funcArgs)[[Range[numX]]],$fixedPointLimit]]],
1->funcArgs]]]
(* input   [linMod,XZ, xguess,function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)

$fixedPointLimit=30;
genFPFuncAgain[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:{_Function..},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}]},
ReplacePart[
Function[xxxx,
FixedPoint[With[{xzFuncNow=
genFRFunc[{numX,numEps,numZ},genLilXkZkFunc[linMod,XZFuncs,#[[Range[numX]]]],
eqnsFunc]},xzFuncNow @@funcArgs]&,xtGuess,$fixedPointLimit]],
1->funcArgs]]]


Print["genXZFuncPFInterp: tied to RBC Model not generic"]'
 
genXZFuncPFInterp[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{theFuncNow=genXZFuncPF[{numX,numEps,numZ},aLilXkZkFunc]},
makeInterpFunc[theFuncNow,{toIgnore,getIOrd[gSpec],Drop[getGridPtTrips[gSpec],-numEps]}]]

genXZFuncREInterp[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
makeInterpFunc[theFuncNow,{toIgnore,gSpec[[2]],Drop[getGridPtTrips[gSpec],-(numEps-If[numRegimes>0,1,0])],numRegimes}]]
  


getToIgnore[gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=toIgnore
 
 
getIOrd[gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=iOrd


getGridPtTrips[gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=gSpec[[3]]
  
getNumVars[gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
(Length[getGridPtTrips[gSpec]]+If[numRegimes>0,2,0])

 genIntVars[numX_Integer,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{xVars=Table[Unique["xV"],{numX}],
	dists=getDistribs[distribSpec],
	distVars=Table[Unique["epIntV"],{getNumEpsVars[distribSpec]}]},
With[{xEpsVars=If[regimeTransProbFunc=={},
	Join[xVars,distVars],Join[xVars,distVars(*,{Unique["regV"]}*)]],
	intArg=MapThread[#1 \[Distributed] #2&,{distVars,dists}]},
	{xVars,xEpsVars,intArg}]]


prepMeansForHApp[theMeans_List,initVec_List]:=
Join[Transpose[{initVec}],Transpose[{Flatten[theMeans,1]}]]

prepStdDevsForHApp[theStdDevs_List]:=
Join[Transpose[{{0,0,0}}],Transpose[{Flatten[theStdDevs,1]}]]


End[]
EndPackage[]

Print["done reading AMASeriesRepresentation`"]
