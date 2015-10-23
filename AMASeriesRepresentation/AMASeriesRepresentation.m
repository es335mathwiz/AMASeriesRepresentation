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
Begin["Private`"]

Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]




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

(*sum for tp1 mult by FF for t*)
fSum[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},XZFuncs:{_Function..},xtGuess_?MatrixQ]:=
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
xkFunc_CompiledFunction,eqnsFunc_CompiledFunction]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX+numEps}],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
With[{zArgsInit={#,0}&/@zArgs,funcName=Unique["fName"]},
funcName[funcArgs:{_?NumberQ..},zArgs:{_?NumberQ..}]:=
eqnsFunc@@(Flatten[xkFunc@@Join[funcArgs,zArgs]]);
ReplacePart[
Function[xxxx,With[{zVals=zArgs/.FindRoot[funcName[funcArgs,zArgs],zArgsInit]},
Join[(xkFunc@@Join[funcArgs,zVals])[[numX+Range[numX]]],
Transpose[{zVals}]]]],
1->funcArgs]]]
$fixedPointLimit=30;
genFPFunc[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:{_Function..},xtGuess_?MatrixQ,eqnsFunc_CompiledFunction]:=
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

(*


doExpect[vecFuncs:{_InterpolatingFunction..},distribs_List,xtm1_?VectorQ]:=
With[{shockVars=Table[Unique["shkVars"],{Length[distribs]}]},
NExpectation[Through[vecFuncs@@#&[Join[xtm1,shockVars]]],Thread[shockVars \[Distributed] distribs]]]

(*compute the first set of z functions*)
computeNonFPart[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=(*
computeNonFPart[BB,phi,psiEps,psiC]=*)
(BB.Transpose[{genXtm1Vars[Length[BB]]}] + phi.psiEps.Transpose[{genEpsVars[Length[psiEps[[1]]]]}]+
Inverse[IdentityMatrix[Length[FF]]-FF] . phi . psiC)

computeNonFPart[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xt_?MatrixQ]:=(*
computeNonFPart[BB,phi,psiEps,psiC]=*)
(BB.xt +Inverse[IdentityMatrix[Length[FF]]-FF] . phi . psiC)



computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=(*
computeNextXt[linMod]=*)
computeNonFPart[linMod]+phi.psiZ .genZVars[Length[psiZ[[1]]]]


computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xt_?MatrixQ]:=(*
computeNextXt[linMod,xt]=*)
computeNonFPart[linMod,xt](*+phi.psiZ .genZVars[Length[psiZ[[1]]]]*)

computeNextXtp1[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=(*
computeNextXtp1[linMod]=*)
With[{xt=computeNextXt[linMod]},
	computeNextXt[linMod,xt]]



Print["should memoize subXtXtp1"]
subXtXtp1[aFunc_Function,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
	With[{xt=computeNextXt[linMod],
		xtp1=computeNextXtp1[linMod]},
		aFunc[Transpose[{genXtm1Vars[Length[BB]]}],xt,xtp1,Transpose[{genEpsVars[Length[psiEps[[1]]]]}]]]



makeConstraintFindRootFunc[hmFunc_Function,
	linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	zzGuesser:{_InterpolatingFunction...}]:=(*
makeConstraintFindRootFunc[hmFunc,linMod,zzGuesser]=*)
With[{subbedEqns=Thread[(subXtXtp1[hmFunc,linMod]/.zName_[t]->zName(*//N//Expand//Simplify*))==0],
	forZSubs=Flatten[Join[computeNextXt[linMod],computeNextXtp1[linMod]]]/.zName_[t]->zName,
	flatXtm1Eps=Flatten[Join[genXtm1Vars[Length[BB]],genEpsVars[Length[psiEps[[1]]]]]],
	xxTargets=Flatten[Join[genXtVars[Length[BB]],genXtp1Vars[Length[BB]]]]},
With[{zzGuess=If[zzGuesser=={},Table[(*Abs[Random[]]*).14*0,{Length[psiZ[[1]]]}],Through[zzGuesser[flatXtm1Eps]]]},
	With[{findRootArg=Transpose[{Flatten[genZVars[Length[psiZ[[1]]]]]/.zName_[t]->zName,zzGuess}]},
ReplacePart[Function[theArgs,
	With[{zSubs=
FindRoot[subbedEqns,
		findRootArg]//Chop},
		Join[Thread[xxTargets->(forZSubs/.zSubs)],zSubs]]
		],1->flatXtm1Eps]]]]





(*compute the next set of z functions, setup x guess for fixed point computation*)

computeFPower[FF_?MatrixQ,kk_Integer]:=(
computeFPower[FF,kk]=FF .computeFPower[FF,kk-1])/;kk>=1
computeFPower[FF_?MatrixQ,0]:=IdentityMatrix[Length[FF]]

computeFPartK[FF_?MatrixQ,phi_?MatrixQ,psiZ_?MatrixQ,kk_Integer]:=
computeFPartK[FF,phi,psiZ,kk,ZZk]=computeFPower[FF,kk].phi.psiZ

computeFPart[FF_?MatrixQ,phi_?MatrixQ,psiEps_?MatrixQ,psiZ_?MatrixQ,
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
With[{kk=Length[ZZks]/Length[psiZ[[1]]],zzkVecs=applyZFuncs[ZZks,doIgnoreParts[xxGuess,toIgnore]]},
	With[{allZPows= ArrayFlatten[{Table[computeFPartK[FF,phi,psiZ,ii],{ii,kk}]}]},allZPows . zzkVecs]]


computeFPart[FF_?MatrixQ,phi_?MatrixQ,psiEps_?MatrixQ,psiZ_?MatrixQ,
	{},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=Table[0,{Length[phi]}]


	
	
applyZFuncs[theFuncs:{_InterpolatingFunction..},xxGuessEps_?MatrixQ]:=
Transpose[{Through[(theFuncs @@ #)&[Flatten[xxGuessEps]]]}]


computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=(*
computeNextXt[linMod,ZZks,xxGuess,toIgnore]=*)
computeNonFPart[linMod]+
computeFPart[FF,phi,psiEps,psiZ,ZZks,xxGuess,toIgnore]+phi.psiZ.genZVars[Length[psiZ[[1]]]]

computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xt_?MatrixQ,
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=(*
computeNextXt[linMod,xt,ZZks,xxGuess,toIgnore]=*)
computeNonFPart[linMod,xt]+
computeFPart[FF,phi,psiEps,psiZ,ZZks,xxGuess,toIgnore]+phi.psiZ.genZVars[Length[psiZ[[1]]]]



computeNextXtp1[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=(*
computeNextXtp1[linMod,ZZks,xxGuess,toIgnore]=*)
With[{xt=computeNextXt[linMod,ZZks,xxGuess,toIgnore]},
With[{zzkVecs=applyZFuncs[ZZks[[Range[Length[psiZ[[1]]]]]],doIgnoreParts[xxGuess,toIgnore]]},
computeNonFPart[linMod,xt]+
computeFPart[FF,phi,psiEps,psiZ,Drop[ZZks,Length[psiZ[[1]]]],xxGuess,toIgnore]+phi.zzkVecs]]


doIgnoreParts[xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
Delete[xxGuess,{#}&/@toIgnore]

Print["should memoize subXtXtp1"]
subXtXtp1[aFunc_Function,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
	With[{xt=computeNextXt[linMod,ZZks,xxGuess,toIgnore],
		xtp1=computeNextXtp1[linMod,ZZks,xxGuess,toIgnore]},
		aFunc[Transpose[{genXtm1Vars[Length[BB]]}],xt,xtp1,Transpose[{genEpsVars[Length[psiEps[[1]]]]}]]]




makeConstraintFindRootFunc[hmFunc_Function,
	linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	ZZks:{_InterpolatingFunction..},
	zzGuesser:{_InterpolatingFunction...},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
(*makeConstraintFindRootFunc[hmFunc,linMod,ZZks,zzGuesser,xxGuess,toIgnore]=*)
With[{subbedEqns=Thread[(subXtXtp1[hmFunc,linMod,ZZks,xxGuess,toIgnore]/.zName_[t]->zName(*/N//Expand//Simplify*))==0],
	forZSubs=Flatten[Join[computeNextXt[linMod,ZZks,xxGuess,toIgnore],computeNextXtp1[linMod,ZZks,xxGuess,toIgnore]]]/.zName_[t]->zName,
	flatXtm1Eps=Flatten[Join[genXtm1Vars[Length[BB]],genEpsVars[Length[psiEps[[1]]]]]],
	xxTargets=Flatten[Join[genXtVars[Length[BB]],genXtp1Vars[Length[BB]]]]},
With[{zzGuess=If[zzGuesser=={},Table[0,{Length[psiZ[[1]]]}],Through[zzGuesser[flatXtm1Eps]]]},
	With[{findRootArg=Transpose[{Flatten[genZVars[Length[psiZ[[1]]]]]/.zName_[t]->zName,zzGuess}]},Sow[{subbedEqns,findRootArg}];
ReplacePart[Function[theArgs,
	With[{zSubs=
FindRoot[subbedEqns,
		findRootArg]},
		Join[Thread[xxTargets->(forZSubs/.zSubs)],zSubs]]
		],1->flatXtm1Eps]]]]


FPTConst=10;
makeConstraintFixedPointFunc[hmFunc_Function,
	linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	ZZks:{_InterpolatingFunction..},zzGuesser:{_InterpolatingFunction...},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
(*makeConstraintFixedPointFunc[hmFunc,
	linMod,
	ZZks,zzGuesser,xxGuess,toIgnore]=*)
With[{numVars=Length[BB],numShocks=Length[psiEps[[1]]]},
With[{xeVars=Table[Unique["xeVars"],{numVars+numShocks}],
frFuncNow=
Function[xg,(*Print["x"(*"from makeConstraintFindRoot spawn:xg=",xg//InputForm*)];*)
makeConstraintFindRootFunc[hmFunc,linMod,ZZks,zzGuesser,xg,toIgnore]]},
ReplacePart[Function[theArgs,(*Print["xeVars=",something];*)
With[{theFP=TimeConstrained[
FixedPoint[
Transpose[{Last/@(frFuncNow[#]@@xeVars)}][[Range[numVars]]]&,xxGuess,30,SameTest->mySameTest],FPTConst,Sow[theArgs,"badArgs"]]},
frFuncNow[theFP]@@xeVars]],
{1->xeVars,{2,1,2}->xeVars}]
]]

mySameTest[xx_?MatrixQ,yy_?MatrixQ]:=(Norm[xx-yy]<=10^-6)


genZVars[numConstr_Integer]:=
Reverse[Flatten[genZVars[0,numConstr]]](*
Module[{},
genZVars[numConstr]=
Table[
makeProtectedSymbol["zzzVar$"<>ToString[ii]],{ii,numConstr}]]*)/;And[numConstr>=0]


genXtVars[numVars_Integer]:=
Module[{},
genXtVars[numVars]=
Table[
makeProtectedSymbol["xxxtVar$"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]


genXtm1Vars[numVars_Integer]:=
Module[{},
genXtm1Vars[numVars]=
Table[
makeProtectedSymbol["xxxtm1Var$"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]


genXtp1Vars[numVars_Integer]:=
Module[{},
genXtp1Vars[numVars]=
Table[
makeProtectedSymbol["xxxtp1Var$"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]


genXGuessVars[numVars_Integer]:=
Module[{},
genXGuessVars[numVars]=
Table[
makeProtectedSymbol["xxxGuessVar$"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]


genEpsVars[numShocks_Integer]:=
Module[{},
genEpsVars[numShocks]=
Table[
makeProtectedSymbol["epsVar$"<>ToString[ii]],{ii,numShocks}]]/;And[numShocks>=0]


gridPts[rngs:{{_?NumberQ,_?NumberQ,_?NumberQ}..}]:=
With[{funcForPts=(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]] @#) &},
With[{oneDimPts=funcForPts/@rngs},
With[{theOuter=Outer[List,Sequence@@#]&[oneDimPts]},
Flatten[theOuter,Depth[theOuter]-3]]]]


oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
If[iPts==0,{{(xLow+xHigh)2}},
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]]/;iPts>=0

subsFuncToVecFunc[aSubsFunc_Function]:=Function[xx,With[{theRes=aSubsFunc @@ xx},Last/@theRes]]

fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
	If[MemberQ[toIgnore,Length[theRes]+1],fillIn[{Append[theRes,0],Drop[toIgnore,1],shortVec}],
		fillIn[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]

fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
fillIn[{theRes,Sort[toIgnore],shortVec}]

makeInterpFunc[aVecFunc_Function,toIgnore:{_Integer...},gSpec:{iOrd_Integer,{_Integer,_?NumberQ,_?NumberQ}..}]:=
With[{thePts=gridPts[Drop[gSpec,1]]},
With[{interpData=Map[{#,aVecFunc@fillIn[{{},toIgnore,#}]}&,thePts]},Function[{kk,theOrd},
Interpolation[{#[[1]],#[[2,kk]]}&/@interpData,InterpolationOrder->theOrd]]@@#&/@
Transpose[{Range[Length[interpData[[1,2]]]],Table[iOrd,{Length[interpData[[1,2]]]}]}]]]


prepNextZZksPForRE[iterStateFuncs:{_InterpolatingFunction..},
	newPForREFunc:{_InterpolatingFunction..},previousPForREFuncs:{_InterpolatingFunction...},numShocks_Integer]:=
Join[newPForREFunc,ageZFuncsPF[iterStateFuncs,previousPForREFuncs,numShocks]]

makeInterpFuncPF[aVecFunc_Function,
	toIgnore:{_Integer...},gSpec:{iOrd_Integer,{_Integer,_?NumberQ,_?NumberQ}..},numShocks_Integer]:=
With[{interpFuncs=makeInterpFunc[aVecFunc,toIgnore,gSpec]},
	With[{droppedShocks=Drop[gSpec,-numShocks]},
	With[{newGSpecPts=Drop[droppedShocks,1]},
		With[{thePts=gridPts[newGSpecPts]},
With[{interpData=Map[{#,doPF[interpFuncs,numShocks,#]}&,thePts]},Function[{kk,theOrd},
Interpolation[{#[[1]],#[[2,kk]]}&/@interpData,InterpolationOrder->theOrd]]@@#&/@
Transpose[{Range[Length[interpData[[1,2]]]],Table[iOrd,{Length[interpData[[1,2]]]}]}]]]]]]


doPF[vecFuncs:{_InterpolatingFunction..},numShocks_Integer,xtm1_?VectorQ]:=
With[{shockVals=Table[0,{numShocks}]},
Through[vecFuncs@@#&[Join[xtm1,shockVals]]]]

makeInterpFuncRE[aVecFunc_Function,
	toIgnore:{_Integer...},gSpec:{iOrd_Integer,{_Integer,_?NumberQ,_?NumberQ}..},distribs_List]:=
With[{interpFuncs=makeInterpFunc[aVecFunc,toIgnore,gSpec]},
	With[{droppedShocks=Drop[gSpec,-Length[distribs]]},
	With[{newGSpecPts=Drop[droppedShocks,1]},
		With[{thePts=gridPts[newGSpecPts]},
With[{interpData=Map[{#,doExpect[interpFuncs,distribs,#]}&,thePts]},Function[{kk,theOrd},
Interpolation[{#[[1]],#[[2,kk]]}&/@interpData,InterpolationOrder->theOrd]]@@#&/@
Transpose[{Range[Length[interpData[[1,2]]]],Table[iOrd,{Length[interpData[[1,2]]]}]}]]]]]]

doExpect[vecFuncs:{_InterpolatingFunction..},distribs_List,xtm1_?VectorQ]:=
With[{shockVars=Table[Unique["shkVars"],{Length[distribs]}]},
NExpectation[Through[vecFuncs@@#&[Join[xtm1,shockVars]]],Thread[shockVars \[Distributed] distribs]]]

getGridPtsLessShocks[aFunc_InterpolatingFunction,numShocks_Integer]:=
With[{fullGrid=InterpolatingFunctionGrid[aFunc]},
	With[{iPts=Union[Drop[#,-numShocks]&/@Level[fullGrid,{Depth[fullGrid]-2}]]},iPts]]


ageOneZFuncPF[iterStateFuncs:{_InterpolatingFunction..},zFunc_InterpolatingFunction,numShocks_Integer]:=
With[{iPts=getGridPtsLessShocks[iterStateFuncs[[1]],numShocks],
	iOrd=InterpolatingFunctionInterpolationOrder[iterStateFuncs[[1]]][[1]]},
	With[{funcData=Map[{#,zFunc@@#}&,iPts]},
		Interpolation[Flatten[funcData,Depth[funcData]-4],InterpolationOrder->iOrd]]]/;(Length[Union[InterpolatingFunctionInterpolationOrder[iterStateFuncs[[1]]]]]==1)

ageZFuncsPF[iterStateFuncs:{_InterpolatingFunction..},zFuncs:{_InterpolatingFunction...},numShocks_Integer]:=
ageZFuncsPF[iterStateFuncs,zFuncs,numShocks]=Module[{},ageOneZFuncPF[iterStateFuncs,#,numShocks]& /@ zFuncs]

	
genPath[xtm1_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,
psic_?MatrixQ,psiz_?MatrixQ,
numNonZeroZs_Integer,zSubs_List,epsSubs_List]:=
genPath[xtm1,
bmat,phimat,fmat,psieps,psic,psiz,
numNonZeroZs,zSubs,epsSubs]=
 With[{numCon=Length[psiz[[1]]],epsVals=genEpsVars[Length[psieps[[1]]]]/.epsSubs},
      With[{rawFParts=Reverse[(doFPart[phimat,fmat,psiz,#,numCon,0] &/@Range[0,numNonZeroZs-1])]/.zSubs},
With[{bgn=(nonFPart[xtm1,
epsVals,bmat,phimat,fmat,psieps,psic]+rawFParts[[1]])},
Join[xtm1,Join @@ FoldList[(nonFPart[#1,{{0}},bmat,phimat,fmat,psieps,psic]+#2)&,bgn,Drop[rawFParts,1]]]]]]

doFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer]:=
doFPart[phimat,fmat,psiz,horizon,numCon,0]


doFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer,offset_Integer]:=
With[{zMats=genZVars[horizon,numCon,offset]},
Plus @@ MapIndexed[ MatrixPower[fmat,(#2[[1]]-1)] . phimat. psiz . #1&,
Reverse[zMats]]]


doFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer,offset_Integer,zSubs_List]:=
With[{zMats=genZVars[horizon,numCon,offset]/.zSubs},
Plus @@ MapIndexed[ MatrixPower[fmat,(#2[[1]]-1)] . phimat. psiz . #1&,
Reverse[zMats]]]


doFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer,zSubs_List]:=doFPart[phimat,fmat,psiz,horizon,numCon,0,zSubs]


nonFPart=Compile[{{xtm1,_p,2},{epsilon,_Real,2},
{bmat,_Real,2},{phimat,_Real,2},{fmat,_Real,2},{psimat,_Real,2},{psic,_Real,2}},
bmat . xtm1 + phimat . psimat . epsilon + 
Inverse[IdentityMatrix[Length[xtm1]]-fmat] . phimat . psic]

genZVars[horizons_Integer,numConstr_Integer]:=
genZVars[horizons,numConstr,0]
	
genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Module[{},
genZVars[horizons,numConstr,offset]=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]]/;offset<=0

genEpsVars[numShocks_Integer]:=
Table[
{makeProtectedSymbol["eps$"<>ToString[ii]]},{ii,numShocks}]



(*numTerms version used outside of this function to check solution*)
computeFPart[FF_?MatrixQ,phi_?MatrixQ,psiEps_?MatrixQ,psiZ_?MatrixQ,numTerms_Integer]:=
With[{theZs=Join @@ Reverse[Drop[Reverse/@genZVars[numTerms,Length[psiZ[[1]]]],1]]},
	With[{allZPows= ArrayFlatten[{Table[computeFPartK[FF,phi,psiZ,ii],{ii,numTerms}]}]},allZPows.theZs]]


computeXtPath[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	numZTerms_Integer,addZeroZTerms_Integer]:=
		With[{zeroEps=0*Transpose[{Private`genEpsVars[Length[psiEps[[1]]]]}]},
			With[{trips=
				 NestList[{doStep[linMod,#[[1]],#[[2]],#[[3]]],zeroEps,Drop[#[[3]],1]}&,
				 	{Transpose[{Private`genXtm1Vars[Length[BB]]}],
				 Transpose[{Private`genEpsVars[Length[psiEps[[1]]]]}], 
				 Reverse /@ genZVars[numZTerms-1, Length[psiZ[[1]]],0]/.zName_[t]->zName},numZTerms]},
				 With[{pathNow=Join @@ First/@ trips,theEnd=BB .trips[[-1,1]]+Inverse[IdentityMatrix[Length[FF]]-FF] . phi . psiC},
				 	With[{extraEnd=Join @@ NestList[BB.#+Inverse[IdentityMatrix[Length[FF]]-FF] . phi . psiC&,theEnd,addZeroZTerms]},
				 	Join[pathNow,extraEnd]
				 ]]]]
		
doStep[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
	xxNow_?MatrixQ,epsNow_?MatrixQ,zsNow:{_?MatrixQ...}]:=		
	BB . xxNow + phi .psiEps. epsNow+sumZs[FF,phi,psiZ,zsNow]+
	Inverse[IdentityMatrix[Length[FF]]-FF] . phi . psiC
	
sumZs[FF_?MatrixQ,phi_?MatrixQ,psiZ_?MatrixQ,zsNow:{_?MatrixQ..}]:=
With[{allFPows=ArrayFlatten[{Table[computeFPartK[FF,phi,psiZ,ii],{ii,0,Length[zsNow]-1}]}]},
	allFPows.Transpose[{Flatten[zsNow]}]]
	
sumZs[FF_?MatrixQ,phi_?MatrixQ,psiZ_?MatrixQ,{}]:=0


computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},numTerms_Integer]:=(*
computeNextXt[linMod,ZZks,xxGuess,toIgnore]=*)
computeNonFPart[linMod]+
computeFPart[FF,phi,psiEps,psiZ,numTerms]+phi.genZVars[Length[psiZ[[1]]]]

computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},numTerms_Integer,xt_?MatrixQ]:=(*
computeNextXt[linMod,xt,ZZks,xxGuess,toIgnore]=*)
computeNonFPart[linMod,xt]+
computeFPart[FF,phi,psiEps,psiZ,numTerms]+phi.genZVars[Length[psiZ[[1]]]]



computeNextXtp1[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},numTerms_Integer]:=(*
computeNextXtp1[linMod,ZZks,xxGuess,toIgnore]=*)
With[{xt=computeNextXt[linMod,numTerms]},
With[{zzkVecs=Reverse[Last[genZVars[numTerms,Length[psiZ[[1]]]]]]},
computeNonFPart[linMod,xt]+
computeFPart[FF,phi,psiEps,psiZ,numTerms]+phi.psiZ.zzkVecs]]



*)
(*
genZVars::usage="genZVars[horizons_Integer,numConstr_Integer,offset_Integer]"
genPath::usage="genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,numNonZeroZs_Integer,zSubs_List,epsSubs_List]"

Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]
*)
End[]
EndPackage[]

Print["done reading AMASeriesRepresentation`"]
