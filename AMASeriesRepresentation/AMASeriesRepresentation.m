(* Mathematica Package *)

(* Created by the Wolfram Workbench Jul 27, 2015 *)

Print["reading AMASeriesRepresentation`"]

BeginPackage["AMASeriesRepresentation`", {"JLink`","ProtectedSymbols`","mathSmolyak`",
	"DifferentialEquations`InterpolatingFunctionAnatomy`"}]

truncErrorMat::usage="truncErrorMat[{{fmat,_Real,2},{phimat,_Real,2},{kk,_Integer}}]"

genXZFuncRE::usage="genXZFuncRE[{numX_Integer,numEps_Integer,numZ_Integer},aLilXkZkFunc_Function,distribs_List]"

genXZFuncPF::usage="genXZFuncPF[{numX_Integer,numEps_Integer,numZ_Integer},aLilXkZkFunc_Function]"
pathErrs::usage="pathErrs[{numX_Integer,numEps_Integer,numZs_Integer},{lilXZFunc_Function,bigXZFuncs:{_Function..}},eqnsFunc_CompiledFunction,anX_?MatrixQ,anEps_?MatrixQ]"

nestIterPF::usage="nestIterPF[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc_CompiledFunction,numIters_Integer]"


nestIterRE::usage="nestIterRE[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc_CompiledFunction,numIters_Integer]"

genPath::usage="genPath[xzFunc_Function,XZFuncs:{_Function..},xtm1Val_?MatrixQ,epsVal_?MatrixQ]"
genX0Z0Funcs::usage="genX0Z0Funcs[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]"

doIterPF::usage="doIterPF[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc_CompiledFunction]"


doIterRE::usage="doIterRE[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc_CompiledFunction]"
X0Z0::usage="from genX0Z0Funcs[linMod];"
fSum::usage="fSum[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},zPath:{_?MatrixQ..}]"

iterateDRPF::usage="iterateDRPF[drFunc_Function,initVec_?VectorQ,numPers_Integer]"

iterateDRREIntegrate::usage="iterateDRPF[drFunc_Function,initVec_?VectorQ,numPers_Integer]"

evalPathErrDRREIntegrate::usage="evalPathErrDRREIntegrate[drFunc_Function,initVec_?VectorQ,allArgs:{expctSpec:{{_Symbol,_}..},opts_:{}},eqnsFunc_CompiledFunction]"

evalExpctPathErrDRREIntegrate[drFunc_Function,initVec_?VectorQ,allArgs:{expctSpec:{{_Symbol,_}..},opts_:{}},eqnsFunc_CompiledFunction]:=
With[{numEps=Length[expctSpec]},





pathErrsDRREIntegrate::usage="pathErrsDRPF[drFunc_Function,eqnsFunc_CompiledFunction,anX_?MatrixQ,anEps_?MatrixQ,numPers_Integer]"
pathErrsDRPF::usage="pathErrsDRPF[drFunc_Function,eqnsFunc_CompiledFunction,anX_?MatrixQ,anEps_?MatrixQ,numPers_Integer]"
Begin["Private`"]


 
gridPts[rngs:{{_?NumberQ,_?NumberQ,_?NumberQ}..}]:=
With[{funcForPts=(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]] @#) &},
With[{oneDimPts=funcForPts/@rngs},
With[{theOuter=Outer[List,Sequence@@#]&[oneDimPts]},
Flatten[theOuter,Depth[theOuter]-3]]]]


oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
If[iPts==0,{{(xLow+xHigh)2}},
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]]/;iPts>=0


fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
	If[MemberQ[toIgnore,Length[theRes]+1],fillIn[{Append[theRes,1],Drop[toIgnore,1],shortVec}],
		fillIn[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]

fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
fillIn[{theRes,Sort[toIgnore],shortVec}]

Print["makeInterpFunc not generic, tied to RBC"];
makeInterpFunc[aVecFunc_Function,toIgnore:{_Integer...},gSpec:{iOrd_Integer,{_Integer,_?NumberQ,_?NumberQ}..}]:=
With[{interpData=genInterpData[aVecFunc,toIgnore,gSpec],numArgs=Length[gSpec[[2]]]},
	With[{numFuncs=Length[interpData[[1,2]]],funcArgs=Table[Unique["fArgs"],{numArgs}]},
	With[{longFuncArgs=fillIn[{},toIgnore,funcArgs],
		interpFuncList=
	Function[funcIdx,Interpolation[{#[[1]], #[[2, funcIdx, 1]]} & /@ interpData,InterpolationOrder -> iOrd]]/@Range[numFuncs]},
	ReplacePart[
	Function[{ig, kk, th}, Transpose[{Through[interpFuncList[kk, th]]}]],{1->longFuncArgs,{-1,1}->funcArgs}]
	]
]]


 
genInterpData[aVecFunc_Function,toIgnore:{_Integer...},gSpec:{iOrd_Integer,{_Integer,_?NumberQ,_?NumberQ}..}]:=
With[{thePts=gridPts[Drop[gSpec,1]]},
With[{interpData=Map[{#,aVecFunc@@fillIn[{{},toIgnore,#}]}&,thePts]},
interpData]]



Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]


iterateDRRE[drFunc_Function,initVec_?MatrixQ,expctSpec:{{anEpsVar_,aDist_},opts_:{}},numPers_Integer,reps_Integer:1]:=
With[{firVal=drFunc @@ initVec},
With[{allReps=
Table[
NestList[drFunc @@ {#[[1]],#[[3]],
If[NumberQ[aDist],aDist,RandomVariate[aDist]]}&,firVal,numPers-1],{reps}]},
With[{theMean=prepMeansForHApp[Mean[allReps],initVec]},
If[reps==1,theMean,
{theMean,prepStdDevsForHApp[StandardDeviation[allReps]]}]]]]/;
And[reps>0,numPers>0]

(*
iterateDRREIntegrate[drFunc_Function,initVec_?MatrixQ,expctSpec:{{{_Symbol,_}..},opts_:{}},numPers_Integer,reps_Integer:1]:=
With[{firVal=drFunc @@ initVec},
With[{allReps=
Table[
NestList[drFunc @@ {#[[1]],#[[3]],
If[NumberQ[aDist],aDist,RandomVariate[aDist]]}&,firVal,numPers-1],{reps}]},
With[{theMean=prepMeansForHApp[Mean[allReps],initVec]},
If[reps==1,theMean,
{theMean,prepStdDevsForHApp[StandardDeviation[allReps]]}]]]]/;
And[reps>0,numPers>0]

*)

iterateDRREIntegrate[drFunc_Function,initVec_?VectorQ,allArgs:{expctSpec:{{_Symbol,_}..},opts_:{}},numPers_Integer]:=
With[{numEps=Length[expctSpec],firVal=drFunc @@ initVec},
	With[{numX=Length[initVec]-numEps,iterFunc=makeREIterFunc[drFunc,allArgs]},
With[{iterated=
NestList[(Transpose[{iterFunc @@ Flatten[#]}])&,firVal,numPers-1]},
Join[Transpose[{initVec}][[Range[numX]]],Join @@ (Identity[#[[Range[numX]]]]&/@iterated)]]]]/;
And[numPers>0]

makeREIterFunc[drFunc_Function,{expctSpec:{{_Symbol,_}..},opts_:{}}]:=
With[{numX=Length[drFunc[[1]]]-Length[expctSpec]},
With[{xVars=Table[Unique["xV"],{numX}]},
With[{xEpsVars=Join[xVars,First/@expctSpec],
	intArg=MapThread[#1 \[Distributed] #2&,Transpose[expctSpec]],funcName=Unique["fName"]},
funcName[funcArgsNot:{_?NumberQ..},idx_Integer]:=(drFunc@@Flatten[funcArgsNot])[[idx,1]];
	With[{theStuff=
		Function[xxxx,
		NExpectation[funcName[Flatten[valSubbed],#],theArg]&/@Range[numX]]
		},(*{theStuff,*)
	ReplacePart[
theStuff,
	{1->xVars,{2,1,1,2}->intArg,{2,1,1,1,1,1}->xEpsVars}](*	}*)
	]]]]

   
evalPathErrDRREIntegrate[drFunc_Function,initVec_?VectorQ,allArgs:{expctSpec:{{_Symbol,_}..},opts_:{}},eqnsFunc_CompiledFunction]:=
With[{numEps=Length[expctSpec]},
With[{pathNow=iterateDRREIntegrate[drFunc,initVec,allArgs,2],numX=Length[initVec]-numEps,
epsArgs=Table[Unique["eArgs"],{numEps}]},
With[{firstArg=
doFuncArg[pathNow,epsArgs,numX,0]},
With[{first=NExpectation[eqnsFunc@@firstArg,
Thread[shockVars \[Distributed] distribs]]},
first
]]]]

evalExpctPathErrDRREIntegrate[
drFunc_Function,initVec_?VectorQ,allArgs:{expctSpec:{{_Symbol,_}..},opts_:{}},eqnsFunc_CompiledFunction]:=
With[{numEps=Length[expctSpec]},
With[{pathNow=iterateDRREIntegrate[drFunc,initVec,allArgs,1],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Flatten[Reverse[initVec[[-Range[numEps]]]]],numX,0]},
With[{first=eqnsFunc@@firstArg},
first
]]]]


 
pathErrsDRREIntegrate[drFunc_Function,initVec_?VectorQ,allArgs:{expctSpec:{{_Symbol,_}..},opts_:{}},eqnsFunc_CompiledFunction,numPers_Integer]:=
With[{numEps=Length[expctSpec]},
With[{pathNow=iterateDRREIntegrate[drFunc,initVec,allArgs,numPers],numX=Length[initVec]-numEps},
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

   
 
pathErrsDRPF[drFunc_Function,initVec_?VectorQ,numEps_Integer,eqnsFunc_CompiledFunction,numPers_Integer]:=
With[{pathNow=iterateDRPF[drFunc,initVec,numEps,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Flatten[Reverse[initVec[[-Range[numEps]]]]],numX,0],
	restArgs=(doFuncArg[pathNow,Table[0,{numEps}],numX,#-2]&/@Range[3,numPers])},
With[{first=eqnsFunc@@firstArg},
	With[{theRest=(eqnsFunc@@#)&/@restArgs},
		Prepend[theRest,first]
]]]]/;
And[numPers>1]

doFuncArg[pathNow_?MatrixQ,epsVals_?VectorQ,numX_Integer,oSet_Integer]:=
With[{firstArg=Join[Flatten[pathNow[[oSet*3+Range[3*numX]]]],Flatten[epsVals]]},
firstArg]
  (*
pathErrsDRPF[drFunc_Function,eqnsFunc_CompiledFunction,
anX_?MatrixQ,anEps_?MatrixQ,numPers_Integer]:=
With[{numX=Length[anX],numEps=Length[anEps]},
With[{aPath=iterateDRPF[drFunc,Flatten[Join[anX,anEps]],numEps,numPers+1]},
Map[(eqnsFunc @@ Append[
Flatten[aPath[[numX*(#-1)+Range[3*numX]]]],0])&,
Range[1,numPers]]]]

*)

pathErrs[{numX_Integer,numEps_Integer,numZs_Integer},
{lilXZFunc_Function,bigXZFuncs:{_Function..}},eqnsFunc_CompiledFunction,
anX_?MatrixQ,anEps_?MatrixQ]:=
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
If[kk==0,Inverse[IdentityMatrix[dim] - fmat],
Inverse[IdentityMatrix[dim] - fmat] . MatrixPower[fmat,kk]]]]


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

genX0Z0Funcs[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars]},
With[{compArgs=xtm1Vars},
Function @@ {compArgs,Join[BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]}]]]


(*func of xtm1vars,epsvars,zvars and a guess for xt*)
genLilXkZkFunc[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
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




(*sum for tp1 mult by FF for t*)
fSumOld[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{
fPows=NestList[FF.#&,IdentityMatrix[numZVars],Length[XZFuncs]-1]},
With[{xzRes=Drop[FoldList[#2@@(Flatten[#1][[Range[numXVars]]])&,
xtGuess,XZFuncs],1]},Plus @@
MapThread[Dot[#1,phi.psiZ.Drop[#2,numXVars]]&,{fPows , xzRes}]]]]

fSum[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB]},
With[{xzRes=#[[numXVars+Range[numXVars]]]&/@(Drop[FoldList[#2@@(Flatten[#1][[Range[numXVars]]])&,
xtGuess,XZFuncs],1])},
fSumC[phi,FF,psiZ,xzRes]]]


fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},
With[{numZVars=Length[psiZ[[1]]]},
With[{fPows=Drop[NestList[FF.#&,IdentityMatrix[numZVars],Length[zPath]],-1]},
Plus @@
MapThread[Dot[#1,phi.psiZ.#2]&,{fPows , zPath}]]]]




genPath[xzFunc_Function,
XZFuncs:{_Function..},xtm1Val_?MatrixQ,epsVal_?MatrixQ]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=xzFunc @@ Flatten[Join[xtm1Val,epsVal]]},
With[{xzRes=FoldList[(#2@@(Flatten[#1][[Range[numXVars]]]))[[Range[numXVars]]]&,
xtVal[[Range[numXVars]]],XZFuncs]},Join[xtm1Val,Join @@xzRes]]]]


genPathCompare[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	xzFunc_Function,XZFuncs:{_Function..},xtm1Val_?MatrixQ,epsVal_?MatrixQ]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=xzFunc @@ Flatten[Join[xtm1Val,epsVal]]},
With[{xzRes=FoldList[(#2@@(Flatten[#1][[Range[numXVars]]]))[[Range[numXVars]]]&,
xtVal[[Range[numXVars]]],XZFuncs]},{Join[xtm1Val,Join @@xzRes],
compareFormula[linMod,XZFuncs,xtm1Val,epsVal,xtVal]}
]]]

compareFormula[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
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

(*eqnsfuncs func of xtm1,xt,xtp1,eps  returns discrep*)
(*xkfunc func of xtm1, eps zs returns xtm1,xt,xtp1,eps as matrices*)
Print["exactCalcsRBC.mth:should compile"];

(*returns function of xtm1 eps that gives xt and z*)
 
genFRFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc_CompiledFunction]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX+numEps}],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
With[{zArgsInit={#,0}&/@zArgs,funcName=Unique["fName"]},
funcName[funcArgsNot:{_?NumberQ..}]:=
Module[{theVars=Join[funcArgsNot]},(*Print[theVars,Flatten[xkFunc@@theVars]];*)
eqnsFunc@@(Flatten[xkFunc@@theVars])];
ReplacePart[
Function[xxxx,With[{zVals=zArgs/.FindRoot[funcName@Join[funcArgs,zArgs],zArgsInit]},
Join[(xkFunc@@Join[funcArgs,zVals])[[numX+Range[numX]]],
Transpose[{zVals}]]]],
1->funcArgs]]]



 
$fixedPointLimit=30;
genFPFunc[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:{_Function..},xtGuess_?MatrixQ,eqnsFunc_CompiledFunction]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}]},
ReplacePart[
Function[xxxx,Sow[
FixedPoint[With[{xzFuncNow=
genFRFunc[{numX,numEps,numZ},genLilXkZkFunc[linMod,XZFuncs,#[[Range[numX]]]],
eqnsFunc]},xzFuncNow @@funcArgs]&,xtGuess,$fixedPointLimit]]],
1->funcArgs]]]


$fixedPointLimit=30;
genFPFuncAgain[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:{_Function..},eqnsFunc_CompiledFunction]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}]},
ReplacePart[
Function[xxxx,
FixedPoint[With[{xzFuncNow=
genFRFunc[{numX,numEps,numZ},genLilXkZkFunc[linMod,XZFuncs,#[[Range[numX]]]],
eqnsFunc]},xzFuncNow @@funcArgs]&,xtGuess,$fixedPointLimit]],
1->funcArgs]]]


doIterPF[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc_CompiledFunction]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=genFPFunc[linMod,XZFuncsNow,xtGuess,eqnsFunc]},
{theFuncs,Prepend[XZFuncsNow,genXZFuncPF[{numX,numEps,numZ},theFuncs]]}]]


doIterRE[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc_CompiledFunction,distribs_List]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=genFPFunc[linMod,XZFuncsNow,xtGuess,eqnsFunc]},
{theFuncs,Prepend[XZFuncsNow,genXZFuncRE[{numX,numEps,numZ},theFuncs,distribs]]}]]

nestIterPF[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc_CompiledFunction,numIters_Integer]:=
NestList[doIterPF[linMod,#[[2]],xtGuess,
eqnsFunc]&,{ig,XZFuncsNow},numIters]


nestIterRE[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncsNow:{_Function..},
xtGuess_?MatrixQ,eqnsFunc_CompiledFunction,distribs_List,numIters_Integer]:=
NestList[doIterRE[linMod,#[[2]],xtGuess,
eqnsFunc,distribs]&,{ig,XZFuncsNow},numIters]

Print["genXZFuncPFInterp: tied to RBC Model not generic"]'

Global`numPts = 20; Global`aGSpec = 
 MapAt[N, {0, {Global`numPts, Global`kLow, Global`kHigh}, {Global`numPts, Global`thLow, Global`thHigh}, {Global`numPts,
     Global`sigLow, Global`sigHigh}}, {{2, 2}, {2, 3}, {3, 2}, {3, 3}, {4, 2}, {4, 
    3}}];
    
genXZFuncPFInterp[{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,aGSpec:{_Integer,{{_Integer,_?NumberQ,_?NumberQ}..}}]:=
With[{theFuncNow=genXZFuncPF[{numX,numEps,numZ},aLilXkZkFunc]},
makeInterpFunc[theFuncNow,{1},Drop[aGSpec,-1]]]

    
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


genXZFuncRE[{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,distribs_List]:=
With[{shockVars=Table[Unique["shkVars"],{Length[distribs]}],
	funcArgs=Table[Unique["theFRFuncArgs"],{numX}],funcName=Unique["fName"]},
funcName[fNameArgs:{_?NumberQ..},idx_Integer]:=Module[{},
(*	Print["fn:",{fNameArgs,idx,(aLilXkZkFunc@@ fNameArgs)}];*)
(aLilXkZkFunc@@ fNameArgs)[[idx,1]]];
eqnsFunc@@(Flatten[xkFunc@@Join[funcArgs,zArgs]]);
ReplacePart[
Function[xxxx,
	NExpectation[
	(funcName[Join[funcArgs,shockVars],#]),Thread[shockVars \[Distributed] distribs]]&/@Range[numX+numZ]
	],
1->funcArgs]]




End[]
EndPackage[]

Print["done reading AMASeriesRepresentation`"]
