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

pathErrsDRPF::usage="pathErrsDRPF[drFunc_Function,eqnsFunc_CompiledFunction,anX_?MatrixQ,anEps_?MatrixQ,numPers_Integer]"
Begin["Private`"]

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

iterateDRPF[drFunc_Function,initVec_?VectorQ,numEps_Integer,numPers_Integer]:=
With[{firVal=drFunc @@ initVec,numX=Length[initVec]-numEps},
With[{iterated=
NestList[(drFunc @@ Flatten[#])&,firVal,numPers-1]},
Join[Transpose[{initVec}][[Range[numX]]],(*firVal[[Range[numX]]],*)Join @@ (#[[Range[numX]]]&/@iterated)]]]/;
And[numPers>0]

  

pathErrsDRPF[drFunc_Function,eqnsFunc_CompiledFunction,
anX_?MatrixQ,anEps_?MatrixQ,numPers_Integer]:=
With[{numX=Length[anX],numEps=Length[anEps]},
With[{aPath=iterateDRPF[drFunc,Flatten[Join[anX,anEps]],numEps,numPers+1]},
Map[(eqnsFunc @@ Append[
Flatten[aPath[[numX*(#-1)+Range[3*numX]]]],0])&,
Range[1,numPers]]]]



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

(*
(*func of xtm1vars,epsvars,zvars and a guess for xt*)
genLilXkZkFuncComp[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],
numEpsVars=Length[psiEps[[1]]],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars],
epsVars=genEpsVars[numEpsVars],
zVars=Reverse[Flatten[genZVars[0,numZVars]]]},
With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
With[{compArgs={#,_Real}&/@Join[xtm1Vars,epsVars,zVars]},
Compile @@ {compArgs,
With[{xtVals=BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . Transpose[{epsVars}]+
phi . psiZ . Transpose[{zVars}] +FF.fCon},
Join[Transpose[{xtm1Vars}],xtVals,
BB.xtVals+Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC+fCon,
Transpose[{epsVars}]]]}]]]]

*)
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

(*
(*need to move compile out of the loop
func of xtm1vars,epsvars,zvars and a guess for xt*)
genLilXkZkFuncNG[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..}]:=
Module[{},
With[{fSummer=Unique["fSumFName"],
	numXVars=Length[BB],
numEpsVars=Length[psiEps[[1]]],numZVars=Length[psiZ[[1]]]},
fSummer[xtGuess_?MatrixQ]:=fSum[linMod,XZFuncs,xtGuess];
With[{fSumName=fSum[linMod,XZFuncs,#]&,
	xtm1Vars=genXtm1Vars[numXVars],
xGVars=Table[Unique["xGuess"],{numXVars}],
epsVars=genEpsVars[numEpsVars],
zVars=Reverse[Flatten[genZVars[0,numZVars]/.{xxx_[t]->xxx}]],
fConNG=Unique["fCon"]},
With[{betterArgs=Flatten[Join[xtm1Vars,epsVars,zVars,xGVars,{fConNG}]]},
With[{compArgs=Append[{#,_Real}&/@(Drop[betterArgs,-1]),{fConNG,_Real,2}]},Print["about to compile",compArgs];
	With[{theGuts={
compArgs,
With[{xtVals=0*BB.Transpose[{xtm1Vars}]+
0*Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + 0*phi . psiEps . Transpose[{epsVars}]+
0*phi . psiZ . Transpose[{zVars}] +FF.fConNG},Print["incomped",{xtm1Vars,epsVars,zVars,xGVars,fConNG}];
Join[Transpose[{xtm1Vars}],xtVals,
BB.xtVals+Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC+fConNG,
Transpose[{epsVars}]]]
}},Print[theGuts//InputForm];
	With[{nonFPartComp=Compile @@ theGuts},
ReplacePart[
Function[xxxx,nonFPartComp @@ Append[zzzz,yyyy @@{Transpose[{xGVars}]}]],
{1->Drop[betterArgs,-1]}
]/.{zzzz->Drop[betterArgs,-1],yyyy->fSumName}
]]]]]]]
*)
notYetFSum[_?MatrixQ]:=Print["notYetFSum:should never evaluate"]
(*	
genLilXkZkFuncNGAgain[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..}]:=
Module[{},
With[{numXVars=Length[BB],
numEpsVars=Length[psiEps[[1]]],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars],
xGVars=Table[Unique["xGuess"],{numXVars}],
epsVars=genEpsVars[numEpsVars],
zVars=Reverse[Flatten[genZVars[0,numZVars]]]},
With[{compArgs={#,_Real}&/@Join[xtm1Vars,epsVars,zVars,xGVars]},
ReplacePart[
Compile[
xxxxx,
With[{fConNG=notfSum[linMod,XZFuncs,Transpose[{xGVars}]]},
With[{xtVals=BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . Transpose[{epsVars}]+
phi . psiZ . Transpose[{zVars}] +FF.fConNG},
Join[Transpose[{xtm1Vars}],xtVals,
BB.xtVals+Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC+fConNG,
Transpose[{epsVars}]]]
]
],
1->compArgs
]]]]]
*)

(*sum for tp1 mult by FF for t*)
notfSum[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
Module[{},
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{
fPows=NestList[FF.#&,IdentityMatrix[numZVars],Length[XZFuncs]-1]},Print[{xtGuess,XZFuncs}//InputForm];
With[{xzRes=5(*Drop[FoldList[#2@@(Flatten[#1][[Range[numXVars]]])&,
xtGuess,XZFuncs],1]*)},Print["notFSum"];5(*Plus @@
MapThread[Dot[#1,phi.psiZ.Drop[#2,numXVars]]&,{fPows , xzRes}]
*)]]]]


(*sum for tp1 mult by FF for t*)
fSumOld[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{
fPows=NestList[FF.#&,IdentityMatrix[numZVars],Length[XZFuncs]-1]},
With[{xzRes=Drop[FoldList[#2@@(Flatten[#1][[Range[numXVars]]])&,
xtGuess,XZFuncs],1]},Plus @@
MapThread[Dot[#1,phi.psiZ.Drop[#2,numXVars]]&,{fPows , xzRes}]]]]

(*
fSum[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},zPath:{_?MatrixQ..}]:=
With[{numZVars=Length[psiZ[[1]]]},
With[{fPows=NestList[FF.#&,IdentityMatrix[numZVars],Length[zPath]-1]},
Plus @@
MapThread[Dot[#1,phi.psiZ.#2]&,{fPows , zPath}]]]
*)
fSum[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{fPows=NestList[FF.#&,IdentityMatrix[numZVars],Length[XZFuncs]-1]},
With[{xzRes=#[[numXVars+Range[numXVars]]]&/@(Drop[FoldList[#2@@(Flatten[#1][[Range[numXVars]]])&,
xtGuess,XZFuncs],1])},fSumC[phi,FF,psiZ,xzRes]]]]


fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},
With[{numZVars=Length[psiZ[[1]]]},
With[{fPows=NestList[FF.#&,IdentityMatrix[numZVars],Length[zPath]-1]},
Plus @@
MapThread[Dot[#1,phi.psiZ.#2]&,{fPows , zPath}]]]]




genPath[xzFunc_Function,
XZFuncs:{_Function..},xtm1Val_?MatrixQ,epsVal_?MatrixQ]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=xzFunc @@ Flatten[Join[xtm1Val,epsVal]]},
With[{xzRes=FoldList[(#2@@(Flatten[#1][[Range[numXVars]]]))[[Range[numXVars]]]&,
xtVal[[Range[numXVars]]],XZFuncs]},Join[xtm1Val,Join @@xzRes]]]]


genPath[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
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
	With[{xtVals=BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . Transpose[{epsVars}]+
phi . psiZ . ztVal +FF.fCon},
Join[Transpose[{xtm1Vars}],xtVals,
BB.xtVals+Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC+fCon,
Transpose[{epsVars}]]]]]]

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
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}](*,
theNG=genLilXkZkFuncNG[linMod,XZFuncs]*)},(*Print["theNG=",theNG//InputForm];*)
ReplacePart[
Function[xxxx,
FixedPoint[With[{xzFuncNow=
genFRFunc[{numX,numEps,numZ},genLilXkZkFunc[linMod,XZFuncs,#[[Range[numX]]]],
eqnsFunc]},xzFuncNow @@funcArgs]&,xtGuess,$fixedPointLimit]],
1->funcArgs]]]


$fixedPointLimit=30;
genFPFuncAgain[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:{_Function..},eqnsFunc_CompiledFunction]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}](*,
theNG=genLilXkZkFuncNG[linMod,XZFuncs]*)},(*Print["theNG=",theNG//InputForm];*)
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



genXZFuncPF[{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX}],
theZeroes=Table[0,{numEps}]},
ReplacePart[
Function[xxxx,aLilXkZkFunc@@Join[funcArgs,theZeroes]],
1->funcArgs]]


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
