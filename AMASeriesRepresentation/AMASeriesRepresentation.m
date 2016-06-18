(* Mathematica Package *)

(* Created by the Wolfram Workbench Jul 27, 2015 *)

Print["reading AMASeriesRepresentation`"]

BeginPackage["AMASeriesRepresentation`", {"JLink`","ProtectedSymbols`",
	"DifferentialEquations`InterpolatingFunctionAnatomy`"}]

genDrvX0Z0Funcs::usage="genDrvX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]"	
$noTransFunc::usage="transfuncinfo";
	
$transFuncHasShocks::usage="transfuncinfo";
	
$transFuncNoShocks::usage="transfuncinfo";




X0Z0::usage="from genX0Z0Funcs[linMod];"


PerfectForesight::usage="degenerate distribution implementing perfect foresight"



(*usage updated*)


makeInterpFunc::usage="makeInterpFunc[aVecFunc_Function,
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]

returns an interpolating function based on the vector function and grid specification
"

genPathCompare::usage="genPathCompare[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	xzFunc_Function,{XZFunc_Function,numSteps_Integer},xtm1Val_?MatrixQ,epsVal_?MatrixQ]	
"

compareFormula::usage="compareFormula[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{XZFunc_Function,numSteps_Integer},xtm1Vars_?MatrixQ,epsVars_?MatrixQ,xtztVal_?MatrixQ]	
	"

genNSFunc::usage="genNSFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc:(_Function|_CompiledFunction)]

returns a function that uses NSolve to solve the eqnsFunc system using the xkFuncs in the series approximation for the conditional expectations function
"
genFRFunc::usage="genFRFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc:(_Function|_CompiledFunction)]

returns a function that uses FindRoot to solve the eqnsFunc system using the xkFuncs in the series approximation for the conditional expectations function
"


genFPFunc::usage="genFPFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:({_Function,_Integer}),eqnsFunc:(_Function|_CompiledFunction)]

returns a function (xtm1,eps) by  iterating  to get  the correct value of xt (zt) to use in the conditional expectations function and iterates until the guess is consitent with the root finding solution
"

genLilXkZkFunc::usage="genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncs:({_Function,_Integer}),xtGuess_?MatrixQ]

uses the guess to compute the F sum in AMA series expansion
returns a function	(xtm1,eps,zvars) that returns a vector of{xtm1,xt,xtp1,eps) values
	
"
	
	
	
makeRegimeFunc::usage="makeRegimeFunc[funcArgsNow_List,numX_Integer,{theS_Function,
thePairs:{{(_Function|CompiledFunction),(_Function|CompiledFunction)}..}}]



"

makeDREvalInterp::usage="makeDREvalInterp[drFunc_Function,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},
eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]

returns a list of two functions: the first evaluates the error in the equations without interpolation, the second uses interpolation
"

fSum::usage="fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{},
	xtGuess_?MatrixQ]

returns matrix of the F weigthed sum of z's"

fSumC::usage="fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},...]


returns matrix of the F weigthed sum of z's
"

genPath::usage="genPath[xzFunc_Function,
{XZFunc_Function,numSteps_Integer},xtm1Val_?MatrixQ,epsVal_?MatrixQ]

generate a path using the xz and XZ funcs

returns a matrix of the path values

"


pathErrs::usage="pathErrs[{numX_Integer,numEps_Integer,numZs_Integer},
{lilXZFunc_Function,bigXZFuncs:{XZFunc_Function,numSteps_Integer}},eqnsFunc:(_Function|_CompiledFunction),
anX_?MatrixQ,anEps_?MatrixQ,X0Z0_Function]
 compute a path
returns a matrix of equation errors along the path
"

checkMod::usage="checkMod[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},anX_?MatrixQ,anEps_?MatrixQ,
eqnsFunc:(_Function|_CompiledFunction)]


"



genDrvLilXkZkFunc::usage="
genDrvLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncs:({_Function,_Integer}),drvXZFuncs:({_Function,_Integer}),xtGuess_?MatrixQ,
	drvPairs:{{{aa_Integer,bb_Integer}...},eqnFunc:(_Function|_CompiledFunction)}:{{},{}}]

given problem dimensions and xz functions 
returns  perfect foresight version functions for future unconditional expectations using interpolation to avoid recalculating all values along the path

"


genXZFuncRE::usage="genXZFuncRE[{numX_Integer,ignored_Integer,numZ_Integer},
aLilXkZkFunc_Function,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]


given problem dimensions and xz functions 
returns  perfect foresight version functions for future unconditional expectations using interpolation to avoid recalculating all values along the path

"

genXZFuncREInterp::usage="genXZFuncREInterp[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]


given problem dimensions and xz functions 
returns  retional expectations version functions for future unconditional expectations using interpolation to avoid recalculating all values along the path


"

nestIterREInterp::usage="nestIterREInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
{XZFuncNow:(_Function|_InterpolatingFunction|_CompiledFunction),numTerms_Integer},eqnsFunc:(_Function|_CompiledFunction),
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numIters_Integer]

recursively apply doIterREInterp numIters times
"


doIterREInterp::usage="doIterREInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction),_Integer},
eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]




given a reference linear model, (x,z,X,Z) function and  and equation system
extends (x,z,X,Z) an additional period imposing rational expectations using interpolation to avoid recalculation of all the points along the path

returns a pair of functions xz and XZ giving the xt values of x and z and giving the unconditional expected path of future values of x and z
"



genX0Z0Funcs::usage="genX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]
returns functions  expected X0(xtm1) and Z0(xtm1)   based on the linear reference model
"
genx0z0Funcs::usage="genX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]
returns functions x0(xtm1,eps) and z0(xtm1,eps)  based on the linear reference model
"

truncErrorMat::usage="truncErrorMat[{{fmat,_Real,2},{phimat,_Real,2},{kk,_Integer}}]

computes the truncation error matrix

"



pathErrsDRPF::usage="pathErrsDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]

uses decision rule to compute a perfect foresight path of length numPers, then applies eqnsFunc along the path
returns List of vectors of equations errors
"


pathErrsDRREIntegrate::usage="pathErrsDRREIntegrate[drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]

uses decision rule to compute an unconditional expectations path of length numPers, then applies eqnsFunc along the path
returns List of vectors of equations errors
"



evalPathErrDRREIntegrate::usage="evalPathErrDRREIntegrate[drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]

uses decision rule to compute an unconditional expectations path of length 2, then applies eqnsFunc 
returns a  vector of equations errors

"



evalBadPathErrDRREIntegrate::usage="evalBadPathErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]

given the initial state vector and the decision rule uses FindMaximum to find the time t shock that produces the largest infinity norm for the error in the
system equations
returns the infintiy norm of the error and the shock value
"

worstPathForErrDRREIntegrate::usage="worstPathForErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]

returns the path corresponding to evalBadPathErrDRREIntegrate
"

genErrsREWorst::usage="genErrsREWorst[theDRFunc:(_Function|_CompiledFunction),initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},
	theSysFunc:(_Function|_CompiledFunction),iters_Integer]
	
	iterates the decision rule using iterateDRREIntegrate
	applies worstPathForErrDRREIntegrate all along the path to return a list of
	vectors corresponding to maximum infinity norm errors in the sysFunc equations
	"

genSeriesRepFunc::usage="genSeriesRepFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),terms_Integer]
	
generates  Zs using genZsREExact, then 
returns a function that 
uses genASeriesRep to generate the value for  {xtm1,xt,xtp1,eps} from the the series representation with the given number of terms 

"


genSeriesReps::usage="genSeriesReps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),maxIters_Integer]
	
generates  Zs using genZsREExact, then uses genASeriesRep to generate a list of the possible  values for {xtm1,xt,xtp1,eps} from the series representations using 
from just the first z matrix to all the z's
"


genASeriesRep::usage="genASeriesRep[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	initVec_?MatrixQ,theZs:{_?MatrixQ..},len_Integer]
	
matrix of the  values for {xtm1,xt,xtp1,eps} from the series representations using 
from the first len z matrices
	"
	
	
genZsFromPath::usage="genZsFromPath[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
thePath_?MatrixQ,theEps_?MatrixQ]

    given the path
	computes the z values using the hmat in linMod
	returns a list of z matrices(one column matrices)

"




genZsREExact::usage="genZsREExact[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),iters_Integer]
	
	iterates a decision rule forward using iterateDRIntegrate (can use PerfectForesight as distribution for specific shocks)
	computes the z values using the hmat in linMod
	returns a list of z matrices(one column matrices)
	"


iterateDRPF::usage=" 
iterateDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,numPers_Integer]
Iterates a decision rule forward from initVec(includes time zero shocks) for numPers time intervals assuming 
all future shocks  zero
produces a matrix containing the path
"


iterateDRREIntegrate::usage="iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction),initVec_?MatrixQ,
	distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numPers_Integer]
	
Iterates a decision rule forward from initVec(includes time zero shocks) for numPers time intervals by integrating out the shocks 
using the distribution
and, if not {},the regime change transition probablities

produces a matrix containing the path"


checkLinMod::usage="checkLinMod[linMod,anX_?MatrixQ,anEps_?MatrixQ,numRegimes_Integer:0]
computes eigenvalues and checks consistency of dimensions of model components
returns {Eigenvalues[BB]//Abs,Eigenvalues[FF]//Abs,X0Z0 @@anX,lilxz @@Join[anX,anEps,Table[0,{numZ}]]}
numRegimes currently has no impact
"



getB::usage="getB[linMod] B from linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}"
getF::usage="getF[linMod] F from linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}"
getPhi::usage="getPhi[linMod] Phi from linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}"
getPsiZ::usage="getPsiz[linMod] PsiZ from linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}"

	
	


Begin["`Private`"]

getB[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
BB
getF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
FF

getPhi[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
phi

getPsiZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
psiZ

checkLinMod[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
anX_?MatrixQ,anEps_?MatrixQ,numRegimes_Integer:0]:=
With[{X0Z0=genX0Z0Funcs[linMod],numZ=Length[psiZ[[1]]]},
With[{lilxz=genLilXkZkFunc[linMod, {X0Z0,2}, Join[anX,anEps]]},
	{Eigenvalues[BB]//Abs,Eigenvalues[FF]//Abs,X0Z0 @@Flatten[anX],lilxz @@ Flatten[Join[anX,anEps,Table[{0},{numZ}]]]}]]



iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction),initVec_?MatrixQ,
	distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec],firVal=drFunc @@ Flatten[initVec]},
	With[{numX=Length[initVec]-numEps,iterFunc=makeREIterFunc[drFunc,distribSpec]},
With[{iterated=
NestList[((Transpose[{Flatten[iterFunc @@ Flatten[#]]}]))&,firVal,numPers-1]},
Join[initVec[[Range[numX]]],Join @@ (Identity[#[[Range[numX]]]]&/@iterated)]]]]/;
And[numPers>0]


 

genZsREExact[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),iters_Integer]:=
Module[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[initVec]-numEps,
 	thePath=Flatten[iterateDRREIntegrate[theExactDR,initVec,distribSpec,iters+1]]},
 	With[{firstVal=theHMat .thePath[[Range[3*numX]]]- psiC - psiEps . Take[initVec,-numEps]},
 		With[{restVals=
      (theHMat .thePath[[Range[3*numX]+numX*#]] -psiC)&/@Range[(Length[thePath]/numX)-3]},
      Join[{firstVal},restVals]
]]]]

genZsFromPath[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	thePath_?MatrixQ,theEps_?MatrixQ]:=
		Module[{numX=Length[theHMat],theMatPath=thePath},
 			   With[{firstVal=theHMat .theMatPath[[Range[3*numX]]]-
							  psiC - psiEps . theEps},
 					With[{restVals=
						  (theHMat .theMatPath[[Range[3*numX]+numX*#]] -psiC)&/@
								  Range[(Length[thePath]/numX)-3]},
     Join[{firstVal},restVals]]]]

genSeriesReps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},theExactDR:(_Function|_CompiledFunction),maxIters_Integer]:=
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



genErrsREWorst[theDRFunc:(_Function|_CompiledFunction),initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},
	theSysFunc:(_Function|_CompiledFunction),iters_Integer]:=
Module[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[initVec]-numEps,
 	thePath=Identity[iterateDRREIntegrate[theDRFunc,initVec,distribSpec,iters+1]]},Print["done thePath"];
With[{worstPaths=
  worstPathForErrDRREIntegrate[theDRFunc,thePath[[Range[numX]+numX*(#)]],distribSpec,theSysFunc]&/@
				       Range[(Length[thePath]/numX)-1]},Print["done worstPaths",worstPaths,Range[(Length[thePath]/numX)-1]];
      Transpose[{theSysFunc @@ Flatten[#]}]&/@worstPaths]
]]



   
evalBadPathErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
	With[{theVal=evalPathErrDRREIntegrate[drFunc,Join[noEpsVec,Transpose[{tryEps}]],distribSpec,eqnsFunc]},
		(*Print["ex:",theVal,Norm[theVal,Infinity]];*)Norm[Transpose[theVal],Infinity]];
	With[{outerEVars=Table[Unique["eVs"],{getNumEpsVars[distribSpec]}]},
	With[{maxArgs={#,0}&/@outerEVars,cons=And @@  ((-0.01<=#<=0.01)&/@ outerEVars)},
	FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]

evalPathErrDRREIntegrate[drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
pathErrsDRREIntegrate[drFunc,initVec,distribSpec,eqnsFunc,2]//First


pathErrsDRREIntegrate[drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{pathNow=iterateDRREIntegrate[drFunc,initVec,distribSpec,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
	restArgs=(doFuncArg[pathNow,Table[{0},{numEps}],numX,#-2]&/@Range[3,numPers])},
With[{first=Transpose[{eqnsFunc@@ Flatten[firstArg]}]},
	With[{theRest=Transpose[{(eqnsFunc@@Flatten[#])}]&/@restArgs},
		Prepend[theRest,first]
]]]]]/;
And[numPers>1]
 
iterateDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,numPers_Integer]:=
With[{firVal=drFunc @@ Flatten[initVec],numX=Length[initVec]-numEps,theZeros=Table[0,{numEps}]},
With[{iterated=
NestList[(drFunc @@ Flatten[Append[#[[Range[numX]]],theZeros]])&,firVal,numPers-1]},
Join[initVec[[Range[numX]]],Join @@ (#[[Range[numX]]]&/@iterated)]]]/;
And[numPers>0]

   
 
pathErrsDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]:=
With[{pathNow=iterateDRPF[drFunc,initVec,numEps,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
	restArgs=(doFuncArg[pathNow,Table[{0},{numEps}],numX,#-2]&/@Range[3,numPers])},
With[{first=Transpose[{eqnsFunc@@Flatten[firstArg]}]},
	With[{theRest=Transpose[{(eqnsFunc@@Flatten[#])}]&/@restArgs},
		Prepend[theRest,first]
]]]]/;
And[numPers>1]

  

pathErrs[{numX_Integer,numEps_Integer,numZs_Integer},
{lilXZFunc_Function,bigXZFuncs:{XZFunc_Function,numSteps_Integer}},eqnsFunc:(_Function|_CompiledFunction),
anX_?MatrixQ,anEps_?MatrixQ,X0Z0_Function]:=
With[{aPath=genPath[lilXZFunc,{XZFunc,numSteps},
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


genDrvX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars]},
With[{compArgs=xtm1Vars},
Function @@ {compArgs,Join[BB,ConstantArray[0,{numZVars,numXVars}]]}]]]




genx0z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]],numEpsVars=Length[psiEps[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars],epsVars=genEpsVars[numEpsVars]},
With[{compArgs=Join[xtm1Vars,epsVars]},
Function @@ {compArgs,Join[BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC+phi . psiEps . epsVars,ConstantArray[0,{numZVars,1}]]}]]]






fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{},
	xtGuess_?MatrixQ]:=
ConstantArray[0,{Length[psiZ],1}]


fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{XZFunc_Function,numSteps_Integer},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xzRes=multiStepZ[{XZFunc,numSteps},numXVars,numZVars]@@ Flatten[xtGuess]},
fSumC[phi,FF,psiZ,xzRes]]]


multiStepZ[{XZfunc_Function,numSteps_Integer},numX_Integer,numZ_Integer]:=
multiStep[{XZfunc,numSteps},numX,numX+Range[numZ]]
multiStepX[{XZfunc_Function,numSteps_Integer},numX_Integer]:=
multiStep[{XZfunc,numSteps},numX,Range[numX]]

multiStep[{XZfunc_Function,numSteps_Integer},numX_Integer,valRange:{_Integer..}]:=
With[{funcArgs=XZfunc[[1]]},
With[{xtFunc01=
ReplacePart[
Function[xxxxx,
	Flatten[(XZfunc@@ xxxxx)[[Range[numX]]]]],{1->funcArgs}]},
With[{theFunc=
	ReplacePart[
	Function[xxxxx,
 With[{theXVals=NestList[xtFunc01@@ Flatten[#]&,xxxxx,numSteps-1]},(*Print["multiStep:theXVals=",{theXVals,((XZfunc @@Flatten[#])[[valRange]] )& /@ theXVals}];*)
	  ((XZfunc @@Flatten[#])[[valRange]] )& /@ theXVals]],1->funcArgs]},
With[{xxxxxPos={{2,1,1,2,1,1,1,2,1,1,2},{2,1,1,2,2}}(*Position[theFunc,xxxxx$]*)},
ReplacePart[
theFunc,
	  {xxxxxPos->funcArgs}]]]]]/;numSteps>0

getZtAfterXt[vecOrMat:_?MatrixQ,numX_Integer]:=vecOrMat[[numX+Range[numX]]]




fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},
With[{numXVars=Length[psiZ]},
With[{fPows=Drop[NestList[FF.#&,IdentityMatrix[numXVars],Length[zPath]],-1]},
Plus @@
MapThread[Dot[#1,phi.psiZ.#2]&,{fPows , zPath}]]]]




drvFSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{},
	xtGuess_?MatrixQ]:=
ConstantArray[0,{Length[psiZ],Length[BB]}]


drvFSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{XZFunc_Function,numSteps_Integer},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xzDrvRes=drvMultiStepZ[{XZFunc,numSteps},numXVars,numZVars]@@ Flatten[xtGuess]},
drvFSumC[phi,FF,psiZ,xzDrvRes]]]


drvMultiStepZ[{XZfunc_Function,drvXZfunc_Function,numSteps_Integer},numX_Integer,numZ_Integer]:=
drvMultiStep[{XZfunc,drvXZfunc,numSteps},numX,numX+Range[numZ]]
drvMultiStepX[{XZfunc_Function,drvXZfunc_Function,numSteps_Integer},numX_Integer]:=
drvMultiStep[{XZfunc,drvXZfunc,numSteps},numX,Range[numX]]

drvMultiStep[{XZfunc_Function,drvXZfunc_Function,numSteps_Integer},numX_Integer,valRange:{_Integer..}]:=
With[{funcArgs=XZfunc[[1]]},
With[{xtFunc01=
ReplacePart[
Function[xxxxx,
	XZfunc[[1]] @@ Flatten[(XZfunc@@ xxxxx)[[Range[numX]]]]],{1->funcArgs}]},
With[{theFunc=
	ReplacePart[
	Function[xxxxx,
 With[{theXVals=NestList[xtFunc01@@ Flatten[#]&,xxxxx,numSteps-1]},Print["drvMultiStep:theXVals=",{theXVals,((XZfunc @@Flatten[#])[[valRange]] )& /@ theXVals}];
	  ((XZfunc @@Flatten[#])[[valRange]] )& /@ theXVals]],1->funcArgs]},
With[{xxxxxPos=Position[theFunc,xxxxx$]},
ReplacePart[
theFunc,
	  {xxxxxPos->funcArgs}]]]]]/;numSteps>0




drvFSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},
With[{numXVars=Length[psiZ]},
With[{fPows=Drop[NestList[FF.#&,IdentityMatrix[numXVars],Length[zPath]],-1]},
Plus @@
MapThread[Dot[#1,phi.psiZ.#2]&,{fPows , zPath}]]]]



genPath[xzFunc_Function,
{XZFunc_Function,numSteps_Integer},xtm1Val_?MatrixQ,epsVal_?MatrixQ]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=xzFunc @@ Flatten[Join[xtm1Val,epsVal]]},
With[{xzRes=multiStepX[{XZFunc,numSteps+1},numXVars]@@ Flatten[xtVal]},
	Join[xtm1Val,Join @@xzRes]]]]
(*eqnsfuncs func of xtm1,xt,xtp1,eps  returns discrep*)
(*xkfunc func of xtm1, eps zs returns xtm1,xt,xtp1,eps as matrices*)


doIterREInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction),_Integer},
eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=makeInterpFunc[genFPFunc[linMod,XZFuncsNow,eqnsFunc],gSpec]},
{theFuncs,genXZFuncREInterp[{numX,numEps,numZ},theFuncs,gSpec,distribSpec]}]]


nestIterREInterp[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
{XZFuncNow:(_Function|_InterpolatingFunction|_CompiledFunction),numTerms_Integer},eqnsFunc:(_Function|_CompiledFunction),
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numIters_Integer]:=
NestList[doIterREInterp[linMod,{#[[2]],numTerms},eqnsFunc,gSpec,distribSpec]&,{ig,XZFuncNow},numIters]




genDrvlilXkZkFuncs[varSpec:{numX_Integer,numEps_Integer,numZ_Integer},
toIgnore:{_Integer...},
aLilXkZkFunc_Function,drvPrs:{{_Integer,_Integer}...}]:=
Module[{funcArg=aLilXkZkFunc[[1]]},
Function @@{funcArg,	If[FreeQ[aLilXkZkFunc,InterpolatingFunction],
		doSymbolicDrv[varSpec,toIgnore,aLilXkZkFunc,drvPrs],
		doInterpFuncDrv[varSpec,toIgnore,aLilXkZkFunc,drvPrs]
]}]


doInterpFuncDrv[varSpec:{numX_Integer,numEps_Integer,numZ_Integer},toIgnore:{_Integer...},
aLilXkZkFunc_Function,drvPrs:{{_Integer,_Integer}...}]:=
Module[{},
doPairInterp[aLilXkZkFunc,#,varSpec,toIgnore]&/@drvPrs]
	
doPairInterp[xzFunc_Function,aPair:{dyIndx_Integer,dxIndx_Integer},varSpec:{numX_Integer,numEps_Integer,numZ_Integer},toIgnore:{_Integer...}]:=
		With[{forDeriv=ReplacePart[Table[0,{numX-Length[toIgnore]}],dxIndx->1],theFunc=xzFunc[[2,numX+dyIndx,1]]},
theFunc/.{ff_[xx__]->Derivative[Sequence @@forDeriv][ff]@@ {xx}}
	]/;And[0<=dyIndx<=numX,0<=dxIndx<=numX-Length[toIgnore]]

doPairInterp[___]:="doPairInterpConfused"



doSymbolicDrv[varSpec:{numX_Integer,numEps_Integer,numZ_Integer},toIgnore:{_Integer...},
aLilXkZkFunc_Function,drvPrs:{{_Integer,_Integer}...}]:=
Module[{theXVars=Table[Unique["x"],{numX}],theEpsVars=Table[Unique["e"],{numEps}],theZVars=Table[Unique["z"],{numZ}]},
	With[{theEval=aLilXkZkFunc @@ Join[theXVars,theEpsVars,theZVars]},doPairSymb[theEval,theXVars,#,varSpec,toIgnore]&/@drvPrs]]
	
doPairSymb[theEval_?MatrixQ,theXVars_?VectorQ,aPair:{dyIndx_Integer,dxIndx_Integer},varSpec:{numX_Integer,numEps_Integer,numZ_Integer},toIgnore:{_Integer...}]:=
With[{dxIndxKeep=Complement[Range[numX],toIgnore]},
	With[{theX=theXVars[[dxIndxKeep[[dxIndx]]]]},
D[theEval[[numX+dyIndx,1]],theX]
	]]/;And[0<=dyIndx<=numX,0<=dxIndx<=numX-Length[toIgnore]]

doPairSymb[___]:="doPairSymbConfused"



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
	Transpose[{myNExpectation[
	(funcName[Append[intVarRes[[2]],ii-1],#]),intVarRes[[3]]]&/@Range[numX+numZ]}],{ii,numRegimes}]]],
	$transFuncHasShocks,Function[xxxx,Module[{},
	Transpose[{myNExpectation[
	Sum[(getProbFunc[distribSpec] @@
		Append[intVarRes[[2]],ii-1])*(funcName[Append[intVarRes[[2]],ii-1],#]),{ii,numRegimes}],intVarRes[[3]]]&/@Range[numX+numZ]}]]]		
	]},
	ReplacePart[funcGuts,1->intVarRes[[1]]]]]



checkMod[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},anX_?MatrixQ,anEps_?MatrixQ,
eqnsFunc:(_Function|_CompiledFunction)]:=
With[{X0Z0=genX0Z0Funcs[linMod],numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{lilxz=
genLilXkZkFunc[linMod, {X0Z0,1}, Join[anX,anEps]]},
With[{	xzFuncNow=If[Head[eqnsFunc]===CompiledFunction,
genFRFunc[{numX,numEps,numZ},lilxz,eqnsFunc],
genNSFunc[{numX,numEps,numZ},lilxz,eqnsFunc]]},
With[{fp=genFPFunc[linMod,{X0Z0,2},eqnsFunc]},
{lilxz @@ Flatten[Join[anX,anEps,Table[0,{numZ}]]],
xzFuncNow @@ Flatten[Join[anX,anEps]],
fp @@ Flatten[Join[anX,anEps]]
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





makeRegimeFunc[funcArgsNow_List,numX_Integer,{theS_Function,
thePairs:{{(_Function|CompiledFunction),(_Function|CompiledFunction)}..}}]:=
With[{xtPos=Range[numX]+2*numX},
With[{preArgs=
(Function[xxxx,With[{indx=(theS@@xxxx+1)},
thePairs[[indx,1]]@@xxxx + 
(thePairs[[indx,2]]@@xxxx).(xxxx[[xxxxXtPos]])]])},
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

fillIn[args___]:=Print["wrong args for fillIn",{args}];
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




makeInterpFunc[aVecFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{interpData=genInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
	With[{numFuncs=Length[interpData[[1,2]]],funcArgs=Table[Unique["fArgs"],{numArgs}]},
	With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}]},
		With[{
		interpFuncList=
	Function[funcIdx,Interpolation[{#[[1]], #[[2, funcIdx, 1]]} & /@ 
		interpData,InterpolationOrder -> iOrd]]/@Range[numFuncs]},
		With[{applied=Transpose[{Through[interpFuncList@@funcArgs]}]},
	(*	Print[	Function[xxxxxxx, Transpose[{Through[interpFuncList@@yyyyyyy]}]]//InputForm];*)
	ReplacePart[
	Function[xxxxxxx, applied],
		{1->longFuncArgs}]
	]
]]]]



 
genInterpData[aVecFunc:(_Function|P_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{thePts=gridPts[getGridPtTrips[gSpec],numRegimes]},
With[{filledPts=ParallelMap[fillIn[{{},toIgnore,#}]&,thePts]},
With[{theVals=ParallelMap[(aVecFunc @@ #)&,filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]



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
	initVec_?MatrixQ,theZs:{_?MatrixQ..},len_Integer]:=
	Module[{},(*Print["theZFuncs",theZFuncs];*)
   With[{maybe = genLilXkZkFunc[linMod,theZs]},(*	Print["zzts",Join[initVec,theZs[[1]]]];*)
   	maybe@@ Join[Flatten[initVec],Flatten[theZs[[1]]]]]]



worstPathForErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{fMinRes=evalBadPathErrDRREIntegrate[drFunc,noEpsVec,distribSpec,eqnsFunc]},
	With[{badEps=Transpose[{(First/@fMinRes[[2]])/.fMinRes[[2]]}]},
	With[{badPath=iterateDRREIntegrate[drFunc,Join[noEpsVec,badEps],distribSpec,2]},
		Join[badPath,badEps]]]]



makeDREvalInterp[drFunc_Function,
	distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},
	eqnsFunc:(_Function|_CompiledFunction),
	gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},
		numRegimes_:0}]:=
With[{xEpsArgs=Table[Unique["xeArgs"],{Length[toIgnore]+getNumVars[gSpec]}]},
	With[{preInterp=
	ReplacePart[
	Function[xxxxxx,  
  Identity[
     evalPathErrDRREIntegrate[drFunc, Transpose[{#}], distribSpec,eqnsFunc]] & @xxxxxx],{1->xEpsArgs,{2,1}->xEpsArgs}]},
     {preInterp,makeInterpFunc[preInterp,gSpec]}
]]
(*flatten to identity*)
doFuncArg[pathNow_?MatrixQ,epsVals_?MatrixQ,numX_Integer,oSet_Integer]:=
With[{firstArg=Join[Identity[pathNow[[oSet*numX+Range[3*numX]]]],Identity[epsVals]]},
firstArg]


genFVars[numVars_Integer]:=
Module[{},
genFVars[numVars]=
Table[
makeProtectedSymbol["fffSum$"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]



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
	XZFuncs:({_Function,_Integer}),xtGuess_?MatrixQ,drvPairs:{{{aa_Integer,bb_Integer}...},eqnFunc:(_Function|_CompiledFunction)}:{{},{}}]:=
	With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
		With[{theRes=genLilXkZkFunc[linMod,fCon,drvPairs]},
theRes]]


genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	theZs:{_?MatrixQ..}]:=
	With[{fCon=fSumC[phi,FF,psiZ,theZs]},
		With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]

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


(*funxzfunc of xtm1vars,epsvars,zvars and a guess for xt*)
genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncs:{({_Function,_Integer})..},xtGuess_?MatrixQ,drvPairs:{{{aa_Integer,bb_Integer}...},eqnFunc:(_Function|_CompiledFunction)}:{{},{}}]:=
	With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
		With[{theRes=genLilXkZkFunc[linMod,fCon,drvPairs]},
theRes]]


genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	fCons:{_?MatrixQ..},drvPairs:{({}|{{aa_Integer,bb_Integer}...}),eqnFunc:({}|_Function|_CompiledFunction)}:{{},{}}]:=
With[{numXVars=Length[BB],numEpsVars=Length[psiEps[[1]]],numZVars=Length[psiZ[[1]]],
	rtm1Var={Unique["rgm"]},rtVar={Unique["rgm"]},rtp1Var={Unique["rgm"]},notUsedVar={Unique["notUsed"]}},
With[{xtm1Vars=Transpose[{genXtm1Vars[numXVars]}],epsVars=Transpose[{genEpsVars[numEpsVars]}],
zVars=Transpose[{Reverse[Flatten[genZVars[0,numZVars]]]/.name_[t]->name}]},
With[{xtVals=genXtOfXtm1[linMod,xtm1Vars,epsVars,zVars,#]&/@fCons},
With[{xtp1Vals=genXtp1OfXt[linMod,xtVals,#]&/@fCons},
With[{fullVec=Join[rtm1Var,xtm1Vars,rtVar,xtVals,rtp1Var,xtp1Vals,epsVars,notUsedVar]},
With[{(*theDrvs=doImplicitDrv[linMod,fullVec,zVars,xtm1Vars,epsVars,drvPairs]*)},(*Print["theDrvs",theDrvs];*)
ReplacePart[
Function[xxxx,fullVec],{1->Flatten[Join[rtm1Var,xtm1Vars,rtVar,epsVars,zVars]]}]
]]]]]]

makeFullVec[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
fCon_?MatrixQ,regimeNumb_Integer:{}]:=
With[{numXVars=Length[BB],numEpsVars=Length[psiEps[[1]]],numZVars=Length[psiZ[[1]]],
rtm1Var=regimeOrNot[regimeNumb],rtVar=regimeOrNot[regimeNumb],rtp1Var=regimeOrNot[regimeNumb]},
With[{xtm1Vars=Transpose[{genXtm1Vars[numXVars]}],epsVars=Transpose[{genEpsVars[numEpsVars]}],
zVars=Transpose[{Reverse[Flatten[genZVars[0,numZVars]]]/.name_[t]->name}]},
With[{xtVals=genXtOfXtm1[linMod,xtm1Vars,epsVars,zVars,fCon]},
With[{xtp1Vals=genXtp1OfXt[linMod,xtVals,fCon]},
Join[rtm1Var,xtm1Vars,rtVar,xtVals,rtp1Var,xtp1Vals,epsVars]]]]]

regimeOrNot[{}]:={}
regimeOrNot[_Integer]:={Unique["rgm"]}

(*funxzfunc of xtm1vars,epsvars,zvars and a guess for xt*)
genDrvLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	XZFuncs:({_Function,_Integer}),drvXZFuncs:({_Function,_Integer}),xtGuess_?MatrixQ,
	drvPairs:{{{aa_Integer,bb_Integer}...},eqnFunc:(_Function|_CompiledFunction)}:{{},{}}]:=
	With[{fCon=fSum[linMod,XZFuncs,xtGuess],drvFCon=drvFSum[linMod,XZFuncs,drvXZFuncs,xtGuess]},
		With[{theRes=genDrvLilXkZkFunc[linMod,fCon,drvFCon,drvPairs]},
theRes]]


genDrvLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	fCon_?MatrixQ,drvFCon_?MatrixQ,drvPairs:{{{aa_Integer,bb_Integer}...},eqnFunc:(_Function|_CompiledFunction)}:{{},{}}]:=
With[{numXVars=Length[BB],numEpsVars=Length[psiEps[[1]]],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=Transpose[{genXtm1Vars[numXVars]}],epsVars=Transpose[{genEpsVars[numEpsVars]}],
zVars=Transpose[{Reverse[Flatten[genZVars[0,numZVars]]]/.name_[t]->name}]},
With[{xtVals=genXtOfXtm1[linMod,xtm1Vars,epsVars,zVars,fCon]},
With[{xtp1Vals=genXtp1OfXt[linMod,xtVals,fCon]},
With[{fullVec=Join[xtm1Vars,xtVals,xtp1Vals,epsVars]},
With[{theDrvs=doImplicitDrv[linMod,fullVec,zVars,xtm1Vars,epsVars,drvPairs]},Print["theDrvs",theDrvs];
ReplacePart[
Function[xxxx,fullVec],{1->Flatten[Join[xtm1Vars,epsVars,zVars]]}]
]]]]]]


genXtp1OfXt[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xtVals_?MatrixQ,
	fCon_?MatrixQ]:=
With[{xtp1Vals=BB.xtVals+Inverse[IdentityMatrix[Length[xtVals]]-FF] . phi . psiC+fCon},xtp1Vals]


genXtOfXtm1[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xtm1Vars_?MatrixQ,epsVars_?MatrixQ,zVars_?MatrixQ,
	fCon_?MatrixQ]:=
With[{xtVals=BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . epsVars+
phi . psiZ . zVars +FF.fCon},xtVals]

doImplicitDrv[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	fullVec_?MatrixQ,zVars_?MatrixQ,xtm1Vars_?MatrixQ,{{},_}]:={}
	
doImplicitDrv[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	fullVec_?MatrixQ,zVars_?MatrixQ,xtm1Vars_?MatrixQ,epsVars_?MatrixQ,drvPairs:{justPairs:{{aa_Integer,bb_Integer}...},
	eqnFunc:(_Function|_CompiledFunction)}]:=
With[{forDeriv=eqnFunc @@ Flatten[fullVec]},
With[{zdrvs=Transpose[D[forDeriv,#]&/@Flatten[zVars]],
	xdrvs=Transpose[D[forDeriv,#]&/@Flatten[xtm1Vars]]},
With[{drvZMat=-Inverse[zdrvs] . xdrvs},
With[{},
{drvZMat}//Expand]]]]

	
doEarlyPairSymb[theXt_?MatrixQ,theXVars_?VectorQ,aPair:{dyIndx_Integer,dxIndx_Integer}]:=
With[{theXtm1=theXVars[[dxIndx]]},
D[theXt[[dyIndx]],theXtm1]]/;And[0<=dyIndx<=numX,0<=dxIndx<=numX]

doEarlyPairSymb[___]:="doPairSymbConfused"


getXt[vecOrMat:_?MatrixQ,numX_Integer]:=vecOrMat[[Range[numX]]]



genPathCompare[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	xzFunc_Function,{XZFunc_Function,numSteps_Integer},xtm1Val_?MatrixQ,epsVal_?MatrixQ]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=xzFunc @@ Flatten[Join[xtm1Val,epsVal]]},
With[{xzRes=multiStepX[{XZFunc,numSteps+1},numXVars]@@ Flatten[xtVal]},
	{Join[xtm1Val,Join @@xzRes],
compareFormula[linMod,{XZFunc,numSteps},xtm1Val,epsVal,xtVal]}
]]]

compareFormula[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	{XZFunc_Function,numSteps_Integer},xtm1Vars_?MatrixQ,epsVars_?MatrixQ,xtztVal_?MatrixQ]:=
	With[{numX=Length[BB],numZ=Length[psiZ[[1]]]},
		With[{xtVal=xtztVal[[Range[numX]]],ztVal=xtztVal[[numX+Range[numZ]]]},
With[{fCon=fSum[linMod,{XZFunc,numSteps},xtVal]},
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
 
genNSFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc:(_Function|_CompiledFunction),opts:OptionsPattern[]]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX+numEps}],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
funcName[funcArgsNot:{_(*?NumberQ*)..}]:=
Module[{theVars=Join[funcArgsNot]},
eqnsFunc@@(Flatten[xkFunc@@theVars])];
ReplacePart[
Function[xxxx,With[{zVals=zArgs/.NSolve[funcName@Join[funcArgs,zArgs],zArgs,Reals,Sequence @@FilterRules[{opts},Options[NSolve]]][[1]]},
Join[(xkFunc@@Join[funcArgs,zVals])[[numX+Range[numX]]],
Transpose[{zVals}]]]],
1->funcArgs]]

myFixedPoint[firstArg_,secondArg_,thirdArg_]:=
Module[{},
FixedPoint[firstArg,secondArg,thirdArg]]
	
	
$fixedPointLimit=30;
genFPFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:({_Function,_Integer}),eqnsFunc:(_Function|_CompiledFunction)]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}]},
ReplacePart[
Function[xxxx,Sow[
myFixedPoint[With[{
	xzFuncNow=If[Head[eqnsFunc]===CompiledFunction,
genFRFunc[{numX,numEps,numZ},genLilXkZkFunc[linMod,XZFuncs,#[[Range[numX]]]],eqnsFunc],
genNSFunc[{numX,numEps,numZ},genLilXkZkFunc[linMod,XZFuncs,#[[Range[numX]]]],eqnsFunc,Method->"JenkinsTraub"]]
},(*Print["infp:",XZFuncs[[1]]@@funcArgs];*)
xzFuncNow @@funcArgs]&,(XZFuncs[[1]]@@funcArgs)[[Range[numX]]],$fixedPointLimit]]],
1->funcArgs]]]
(* input   [linMod,XZ, xguess,function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)



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
