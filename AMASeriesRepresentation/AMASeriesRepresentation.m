(* Mathematica Package *)

(* Created by the Wolfram Workbench Jul 27, 2015 *)

Print["reading AMASeriesRepresentation`"]

BeginPackage["AMASeriesRepresentation`", {"JLink`","ProtectedSymbols`","mathSmolyak`",
	"DifferentialEquations`InterpolatingFunctionAnatomy`"}]

genZVars::usage="genZVars[horizons_Integer,numConstr_Integer,offset_Integer]"
genPath::usage="genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,numNonZeroZs_Integer,zSubs_List,epsSubs_List]"

Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]

Begin["Private`"]

(*compute the first set of z functions*)
computeNonFPart[BB_?MatrixQ,phi_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ]:=(*
computeNonFPart[BB,phi,psiEps,psiC]=*)
(BB.Transpose[{genXtm1Vars[Length[BB]]}] + phi.psiEps.Transpose[{genEpsVars[Length[psiEps[[1]]]]}]+
Inverse[IdentityMatrix[Length[fmat]]-fmat] . phimat . psic)

computeNonFPart[BB_?MatrixQ,phi_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,xt_?MatrixQ]:=(*
computeNonFPart[BB,phi,psiEps,psiC]=*)
(BB.xt +Inverse[IdentityMatrix[Length[fmat]]-fmat] . phimat . psic)



computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ}]:=(*
computeNextXt[linMod]=*)
computeNonFPart[BB,phi,psiEps,psiC]+phi.psiZ .genZVars[Length[psiZ[[1]]]]




computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},xt_?MatrixQ]:=(*
computeNextXt[linMod,xt]=*)
computeNonFPart[BB,phi,psiEps,psiC,xt]+phi.psiZ .genZVars[Length[psiZ[[1]]]]

computeNextXtp1[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ}]:=(*
computeNextXtp1[linMod]=*)
With[{xt=computeNextXt[linMod]},
	computeNextXt[linMod,xt]]



Print["should memoize subXtXtp1"]
subXtXtp1[aFunc_Function,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ}]:=
	With[{xt=computeNextXt[linMod],
		xtp1=computeNextXtp1[linMod]},
		aFunc[Transpose[{genXtm1Vars[Length[BB]]}],xt,xtp1,Transpose[{genEpsVars[Length[psiEps[[1]]]]}]]]



makeConstraintFindRootFunc[hmFunc_Function,
	linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},
	zzGuesser:{_InterpolatingFunction...}]:=
makeConstraintFindRootFunc[hmFunc,linMod,zzGuesser]=
With[{subbedEqns=Thread[(subXtXtp1[hmFunc,linMod]//N//Expand//Simplify)==0],
	forZSubs=Flatten[Join[computeNextXt[linMod],computeNextXtp1[linMod]]],
	flatXtm1Eps=Flatten[Join[genXtm1Vars[Length[BB]],genEpsVars[Length[psiEps[[1]]]]]],
	xxTargets=Flatten[Join[genXtVars[Length[BB]],genXtp1Vars[Length[BB]]]]},
With[{zzGuess=If[zzGuesser=={},Table[0,{Length[psiZ[[1]]]}],Through[zzGuesser[flatXtm1Eps]]]},
	With[{findRootArg=Transpose[{Flatten[genZVars[Length[psiZ[[1]]]]],zzGuess}]},
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


computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=(*
computeNextXt[linMod,ZZks,xxGuess,toIgnore]=*)
computeNonFPart[BB,phi,psiEps,psiC]+
computeFPart[FF,phi,psiEps,psiZ,ZZks,xxGuess,toIgnore]+phi.genZVars[Length[psiZ[[1]]]]
(*
computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},xt_?MatrixQ,
	{},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=(*
computeNextXt[linMod,xt,{},xxGuess,toIgnore]=*)
	computeNextXt[linMod,xt](*/.Join[zapEps,zapZs]*)

*)
computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},xt_?MatrixQ,
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=(*
computeNextXt[linMod,xt,ZZks,xxGuess,toIgnore]=*)
computeNonFPart[BB,phi,psiEps,psiC,xt]+
computeFPart[FF,phi,psiEps,psiZ,ZZks,xxGuess,toIgnore]+phi.genZVars[Length[psiZ[[1]]]]



computeNextXtp1[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=(*
computeNextXtp1[linMod,ZZks,xxGuess,toIgnore]=*)
With[{xt=computeNextXt[linMod,ZZks,xxGuess,toIgnore]},
With[{zzkVecs=applyZFuncs[ZZks[[Range[Length[psiZ[[1]]]]]],doIgnoreParts[xxGuess,toIgnore]]},
computeNonFPart[BB,phi,psiEps,psiC,xt]+
computeFPart[FF,phi,psiEps,psiZ,Drop[ZZks,Length[psiZ[[1]]]],xxGuess,toIgnore]+phi.zzkVecs]]

doIgnoreParts[xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
Delete[xxGuess,{#}&/@toIgnore]

Print["should memoize subXtXtp1"]
subXtXtp1[aFunc_Function,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},
	ZZks:{_InterpolatingFunction..},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
	With[{xt=computeNextXt[linMod,ZZks,xxGuess,toIgnore],
		xtp1=computeNextXtp1[linMod,ZZks,xxGuess,toIgnore]},
		aFunc[Transpose[{genXtm1Vars[Length[BB]]}],xt,xtp1,Transpose[{genEpsVars[Length[psiEps[[1]]]]}]]]




makeConstraintFindRootFunc[hmFunc_Function,
	linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},
	ZZks:{_InterpolatingFunction..},
	zzGuesser:{_InterpolatingFunction...},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
(*makeConstraintFindRootFunc[hmFunc,linMod,ZZks,zzGuesser,xxGuess,toIgnore]=*)
With[{subbedEqns=Thread[(subXtXtp1[hmFunc,linMod,ZZks,xxGuess,toIgnore]//N//Expand//Simplify)==0],
	forZSubs=Flatten[Join[computeNextXt[linMod,ZZks,xxGuess,toIgnore],computeNextXtp1[linMod,ZZks,xxGuess,toIgnore]]],
	flatXtm1Eps=Flatten[Join[genXtm1Vars[Length[BB]],genEpsVars[Length[psiEps[[1]]]]]],
	xxTargets=Flatten[Join[genXtVars[Length[BB]],genXtp1Vars[Length[BB]]]]},
With[{zzGuess=If[zzGuesser=={},Table[0,{Length[psiZ[[1]]]}],Through[zzGuesser[flatXtm1Eps]]]},
	With[{findRootArg=Transpose[{Flatten[genZVars[Length[psiZ[[1]]]]],zzGuess}]},Sow[{subbedEqns,findRootArg}];
ReplacePart[Function[theArgs,
	With[{zSubs=
FindRoot[subbedEqns,
		findRootArg]},
		Join[Thread[xxTargets->(forZSubs/.zSubs)],zSubs]]
		],1->flatXtm1Eps]]]]

(*

makeConstraintFindRootFunc[hmFunc_Function,
	linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},
	ZZks:{_InterpolatingFunction..},zzGuesser:{_InterpolatingFunction...},xxGuess_?MatrixQ,toIgnore:{_Integer...}]:=
(*makeConstraintFindRootFunc[hmFunc,
	linMod,
	ZZks,zzGuesser,xxGuess]=*)
With[{flatXtm1Eps=Flatten[Join[genXtm1Vars[Length[BB]],genEpsVars[Length[psiEps[[1]]]]]],
	xxTargets=Flatten[Join[genXtVars[Length[BB]],genXtp1Vars[Length[psiEps[[1]]]]]]},
With[{zzGuess=If[zzGuesser=={},Table[0,{Length[psiZ[[1]]]}],Through[zzGuesser[flatXtm1Eps]]]},
	With[{findRootArg=Transpose[{Flatten[genZVars[Length[psiZ[[1]]]]],zzGuess}]},
ReplacePart[Function[theArgs,
	With[{forZSubs=Flatten[Join[computeNextXt[linMod,ZZks,xxGuess,toIgnore],
		computeNextXtp1[linMod,ZZks,xxGuess,toIgnore]]],
		zSubs=
FindRoot[Thread[
	(subXtXtp1[hmFunc,linMod,
		ZZks,xxGuess,toIgnore]//N//Expand//Simplify)==0],
		findRootArg]},
		Join[Thread[xxTargets->(forZSubs/.zSubs)],zSubs]]
		],1->flatXtm1Eps]]]]

*)

makeConstraintFixedPointFunc[hmFunc_Function,
	linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},
	ZZks:{_InterpolatingFunction..},xxNext_?MatrixQ,xxNextp1_?MatrixQ,zzGuesser:{_InterpolatingFunction...},xxGuess_?MatrixQ,toIgnore:{_Integer}]:=
makeConstraintFixedPointFunc[hmFunc,
	linMod,
	ZZks,xxNext,xxNextp1,zzGuesser,xxGuess,toIgnore]=
With[{numVars=Length[Length[BB]],numShocks=Length[psiEps[[1]]]},
With[{xeVars=Table[Unique["xeVars"],{numVars+numShocks}],xxTargets=Flatten[Join[xxNext,xxNextp1]],
	frFuncNow=Function[xg,makeConstraintFindRootFunc[hmFunc,linMod,ZZks,xxNext,xxNextp1,zzGuesser,xg]]},
	Print["mcfp:",{flatXtm1Eps,xxTargets}];
ReplacePart[Function[theArgs,FixedPoint[Transpose[{Last/@(frFuncNow[#]@@xeVars)}][[Range[numVars]]]&,xxGuess]],1->xeVars]]]


genZVars[numConstr_Integer]:=
Module[{},
genZVars[numConstr]=
Table[
makeProtectedSymbol["zzzVar$"<>ToString[ii]],{ii,numConstr}]]/;And[numConstr>=0]


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


prepNextZZksPF[iterStateFuncs:{_InterpolatingFunction..},
	newPForREFunc:{_InterpolatingFunction..},previousPForREFuncs:{_InterpolatingFunction...},numShocks_Integer]:=
Join[newPForREFunc,ageZFuncsPF[iterStateFuncs,previousPForREFuncs,numShocks]]
(*
makeInterpFuncPF[aVecFunc_Function,
	toIgnore:{_Integer...},gSpec:{iOrd_Integer,{_Integer,_?NumberQ,_?NumberQ}..},numShocks_Integer]:=
	With[{droppedShocks=Drop[gSpec,-numShocks],theZeroes=Table[0,numShocks]},
	With[{newGSpecPts=doIgnoreParts[droppedShocks,toIgnore]},
		With[{thePts=gridPts[newGSpecPts]},
With[{interpData=Map[{#,aVecFunc@Join[fillIn[{{},toIgnore,#}],theZeroes]}&,thePts]},Function[{kk,theOrd},
Interpolation[{#[[1]],#[[2,kk]]}&/@interpData,InterpolationOrder->theOrd]]@@#&/@
Transpose[{Range[Length[interpData[[1,2]]]],Table[iOrd,{Length[interpData[[1,2]]]}]}]]]]]
*)

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
(*
*)

(*
http://mathematica.stackexchange.com/questions/1803/how-to-compile-effectively
computeNonFPart=Compile[{{BB,_Real,2},{phi,_Real,2},{psiEps,_Real,2},{xtm1,_Real,2},{epst,_Real,2}},
	BB . xtm1 + phi.psiEps . epst]
	
computeFPartk=Compile[{{FF,_Real,2},{phi,_Real,2},{kk,_Integer},{ZZk,_Real,2}},
	computeFPower[FF,kk] . phi . ZZk]
	

makePFFuncs[czFuncs_Function,numModVars_Integer,numShocks_Integer]:=
With[{modArgs=Table[Unique["xvar"],{numModVars}]},
Function[modVars,czFuncs @@ Join[modVars,Table[0,{numShocks}]]]]


	
*)	
	
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
horizon_Integer,numCon_Integer,zSubs_List]:=
With[{zMats=genZVars[horizon,numCon,offset]/.zSubs},
Plus @@ MapIndexed[ MatrixPower[fmat,(#2[[1]]-1)] . phimat. psiz . #1&,
Reverse[zMats]]]

nonFPart=Compile[{{xtm1,_Real,2},{epsilon,_Real,2},
{bmat,_Real,2},{phimat,_Real,2},{fmat,_Real,2},{psimat,_Real,2},{psic,_Real,2}},
bmat . xtm1 + phimat . psimat . epsilon + 
Inverse[IdentityMatrix[Length[xtm1]]-fmat] . phimat . psic]
	
genZVars[horizons_Integer,numConstr_Integer]:=
genZVars[horizons,numConstr,0]
(*
genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]/;offset<=0
*)

genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Module[{},
genZVars[horizons,numConstr,offset]=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]]/;offset<=0

genEpsVars[numShocks_Integer]:=
Table[
{makeProtectedSymbol["eps$"<>ToString[ii]]},{ii,numShocks}]



End[]
EndPackage[]

(*
(* Exported symbols added here with SymbolName::usage *) 
genFinalRE::usage=
"genFinalPF[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},"<>
"compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,"<>
	"{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},"<>
"iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},"<>
"initFuncs_List,expctSpec:{{anEpsVar_,aDist_},opts_:{}},iters_Integer]"
genFinalPF::usage=
"genFinalPF[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},"<>
"compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,"<>
	"{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},"<>
"iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},"<>
"initFuncs_List,iters_Integer]"
genZVars::usage="genZVars[horizons_Integer,numConstr_Integer,offset_Integer]"
genEpsVars::usage="genEpsVars[numShocks_Integer]"
genPath::usage="genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,
numNonZeroZs_Integer,padZeroZs_Integer]"
genZVars::usage="genZVars[horizons_Integer,numConstr_Integer,offset_Integer]"







Begin["Private`"]
(* Implementation of the package *)


Print["need to split eps from other state vars"]
fpForInitStateFunc[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},
	compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,
		{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
	xtm1Val:{_?NumberQ..},epsVal:{_?NumberQ..},
zFuncs_List,(pos_List)|(pos_Integer)]:=
With[{beenDone=fpForInitStateFunc[modSpecific,xtm1Val,epsVal,zFuncs]},
beenDone[[pos]]]

Print["code assumes exactly one shock"]
Print["should eliminate use of Random[] to test argument"]
fpForInitStateFunc[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},xtm1Val:{_?NumberQ..},epsVal:{_?NumberQ..},
zFuncs_List]:=
Module[{},
fpForInitStateFunc[modSpecific,xtm1Val,epsVal,zFuncs]=
With[{zArgs=Table[Unique["xNow"],{iterStateDim}],
initGuess=makeInitGuess[modSpecific,xtm1Val,epsVal,zFuncs]},
With[{theSys=makeSysFunction[modSpecific,xtm1Val,epsVal,zFuncs,zArgs]},
With[{fpTarget=makeFPTarget[modSpecific,zFuncs,zArgs]},
fpSolver[fpTarget,theSys,initGuess]
]]]]/;
With[{anArg=Table[(Random[])^2+.1,{iterStateDim}]},(*Print["fpForInitStateFunc:",{anArg,If[zFuncs==={},{},(Through[((zFuncs[[-1]]) @@#)&[anArg]])],iterStateDim}];*)
Or[zFuncs==={},
NumberQ[Plus @@ (Through[((zFuncs[[-1]]) @@#)&[anArg]])]]]


makeFPTarget[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},zFuncs_List,zArgs_List]:=
With[{theZs=makeTheZs[modSpecific,zFuncs]},
Join[zArgs,theZs]]

makeTheZs[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},zFuncs_List]:=
Flatten[genZVars[
compPathLen[modSpecific,zFuncs]-1,compNumCon[modSpecific]]]

makeValSubs[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},xtm1Val:{_?NumberQ..},epsVal:{_?NumberQ..}]:=
With[{lhRule=(First/@(xtm1[[stateSel]]))},
Append[Thread[lhRule->(xtm1Val[[Range[Length[lhRule]]]])],
eps->epsVal[[1]]]]


makeInitGuess[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},xtm1Val:{_?NumberQ..},epsVal:{_?NumberQ..},
zFuncs_List]:=
With[{epsPart=phimat . psieps . Transpose[{epsVal}]},
(*Print["epsPart=",{epsPart,epsPart[[stateSel]],epsVal}];*)
initGuess=Flatten[If[Length[zFuncs]==0,
Through[noZFuncsGuess@@#&[Join[xtm1Val,epsVal]]],
With[{fromZs=Through[(zFuncs[[Range[iterStateDim]]]@@#&)[xtm1Val]]},
With[{},fromZs+epsPart[[stateSel]]]]]]]


mySameQ[xx_,yy_]:=And[Length[xx]===Length[yy],Norm[xx-yy]<=10^(-10)]

genCompSlackSysFunc[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
pathLen_Integer]:=
With[{aPath=genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,
pathLen,nlead],
theZs=Flatten[genZVars[pathLen-1,Length[compCon]]]},
With[{compConVal=Through[compCon[aPath,theZs]],
rhsEqns=First/@(Drop[aPath,Length[xtm1]][[stateSel]])},
{compConVal,rhsEqns}]]/;
And[pathLen>0]


makeInitStateTryEqnsSubbed[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},xtm1Val:{_?NumberQ..},epsVal:{_?NumberQ..},
zFuncs_List,zArgs_List]:=
With[{valSubs=makeValSubs[modSpecific,xtm1Val,epsVal],
pathLen=compPathLen[modSpecific,zFuncs]},
With[{csrhs=genCompSlackSysFunc[modSpecific,pathLen]/.valSubs},
With[{initStateSubbed=And @@ (csrhs[[1]]),
tryEqnsSubbed=And @@Thread[zArgs==(csrhs[[2]])]},
And[initStateSubbed,tryEqnsSubbed]]]]

makeSysFunction[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},xtm1Val:{_?NumberQ..},epsVal:{_?NumberQ..},
zFuncs_List,zArgs_List]:=
With[{pathLen=compPathLen[modSpecific,zFuncs],
zLeft=compZLeft[modSpecific,zFuncs]},
With[{xTryVars=Table[Unique["xTry"],{Length[zArgs]}]},
With[{theZFuncsApps=makeTheZFuncsApps[modSpecific,zFuncs,zArgs,xTryVars]},
With[{theZEqns=And @@ (Thread[zLeft==theZFuncsApps])},
With[{theGuts=makeSysFunctionGuts[modSpecific,xtm1Val,epsVal,zFuncs,zArgs,xTryVars]},
With[{theFunc=Function @@{xTryVars,theGuts}},
theFunc]]]]]]

makeTheZFuncsApps[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
zFuncs_List,zArgs_List,xTryVars_List]:=
If[compPathLen[modSpecific,zFuncs]===1,{},
Through[(Drop[zFuncs,Length[zArgs]])@@ #&[xTryVars]]]


makeSysFunctionGuts[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},
stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},xtm1Val:{_?NumberQ..},epsVal:{_?NumberQ..},
zFuncs_List,zArgs_List,xTryVars_List]:=
With[{initStateSubbedtryEqnsSubbed=
makeInitStateTryEqnsSubbed[modSpecific,xtm1Val,epsVal,zFuncs,zArgs]},
With[{theZEqns=And @@ (Thread[(zLeft=compZLeft[modSpecific,zFuncs])==(makeTheZFuncsApps[modSpecific,zFuncs,zArgs,xTryVars])])},
And[initStateSubbedtryEqnsSubbed,theZEqns]]]


compPathLen[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},
stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},zFuncs_List]:=
With[{numCon=compNumCon[modSpecific]},
If[zFuncs==={},1,((Length[zFuncs]-iterStateDim)/numCon)+1]]

compNumCon[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},
stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_}]:=Length[compCon] 


compZLeft[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},
stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},zFuncs_List]:=
If[compPathLen[modSpecific,zFuncs]==1,{},
With[{theZs=makeTheZs[modSpecific,zFuncs]},
(Drop[theZs,-compNumCon[modSpecific]])]]

genFinalPF[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},
initFuncs_List,iters_Integer]:=
genFinalWorker[modSpecific(*modSpecific*),
forIOrdNPtsPF,iOrd,gSpec,
initFuncs,{{ignore,ig}},iters]


genFinalRE[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},
initFuncs_List,expctSpec:{{anEpsVar_,aDist_},opts_:{}},iters_Integer]:=
genFinalWorker[modSpecific,
forIOrdNPtsRE,iOrd,gSpec,
initFuncs,expctSpec,iters]
(*put std dev = 0 in ratex*)

genFinalWorker[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
forIOrdNPtsFunc_,
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},initFuncs_List,
expctSpec:{{anEpsVar:(_|ignore),aDist_},opts_:{}},iters_Integer:1]:=
With[{zFuncs=forIOrdNPtsFunc[modSpecific,
iOrd,gSpec,initFuncs,expctSpec,iters],
xWorker=Table[Unique["finalWorker"],{Length[gSpec]}]},
With[{preInterpFunc=
Function @@ {xWorker,fpForInitStateFunc[modSpecific,
xWorker[[Range[iterStateDim]]],xWorker[[{iterStateDim+1}]],zFuncs[[-1]]]}},(*Print["genFinalWorker:",preInterpFunc//InputForm];*)
With[{numVals=Length[preInterpFunc @@ midGrid[gSpec]]},
With[{interpFuncFinal=
makeInterpFuncFinal[preInterpFunc,xtm1,Range[numVals],
iOrd,gSpec]},
{{iOrd,gSpec},{},zFuncs,interpFuncFinal}]]]]

iterPF[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
iOrder_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},zFuncsNow_List]:=
With[{xWorker=Table[Unique["finalWorker"],{iterStateDim}]},
With[
{fpSolnFunc=Function @@ {xWorker,fpForInitStateFunc[modSpecific,
xWorker,{0},zFuncsNow]}},
makeInterpFuncPF[modSpecific,fpSolnFunc,iOrder,gSpec]]]/;
And[iOrder>=0,Min[First/@gSpec]>=iOrder]




Print["forIOrdNPts still has mod specific"]
forIOrdNPtsPF[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},start_List,ignore_,maxLen_Integer]:=
NestList[(Print["applying iterPF>"];
Identity[iterPF[modSpecific,iOrd,gSpec[[Range[iterStateDim]]],#]])&,start,maxLen];


iterRE[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
iOrder_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},
zFuncsNow_List,expctSpec:{{anEpsVar_,aDist_},opts_:{}}]:=
With[{agedZs=ageZFuncs[modSpecific,zFuncsNow],
newInterps=makeInterpFuncRE[modSpecific,iOrder,gSpec,zFuncsNow,expctSpec]},
Join[newInterps[[1]],agedZs,newInterps[[2]]]]/;
And[iOrder>=0,Min[First/@gSpec]>=iOrder]


noShocksGSpec[gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}]:=
Drop[gSpec,-1]


makeInterpFuncRE[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,
{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},iOrder_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},zFuncsNow_List,expctSpec:{{anEpsVar_,aDist_},opts_:{}}]:=
With[{reFunc=makeREFunc[modSpecific,zFuncsNow,expctSpec],
theStateInterps=Range[iterStateDim],
theNewZs=-Reverse[Range[Length[compCon]]]},
{doScalarIntegInterp[
modSpecific,#,reFunc,iOrder,gSpec,expctSpec]&/@theStateInterps,
doScalarIntegInterp[
modSpecific,#,reFunc,iOrder,gSpec,expctSpec]&/@(-Reverse[theNewZs])}
]



myExpect[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
aFuncNow:fpForInitStateFunc[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,
{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
theXVals:{_?NumberQ..},{epsVal_},
zFuncs_List,pos_Integer],aVar_,expctSpec:{{anEpsVar_,aDist_},opts_:{}}]:=
Module[{},(*Print["myExpect:",{aFunc,aVar,aFuncNow,stdev}//InputForm];*)
If[NumberQ[aDist],(*Print["aFunc subbed:",{aFunc/.aVar->0,aFuncNow/.aVar->0}];*)aFuncNow/.aVar->aDist,
With[{stdev=aDist[[2]]},
With[{theIntBody=({aFuncNow,anEpsVar \[Distributed] aDist,Sequence @@ opts(*,
AccuracyGoal -> 2, Compiled -> Automatic,
  PrecisionGoal -> 2, WorkingPrecision -> 2*)})},
(*Print["myExpect:intBody=",theIntBody//InputForm];*)
NExpectation @@ theIntBody]]]]



doScalarIntegInterp[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,
{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
aPos_Integer,anREFunc_Function,iOrd_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},expctSpec:{{anEpsVar_,aDist_},opts_:{}}]:=
With[{xxVars=Table[Unique["xInterpRE"],{iterStateDim}],
thePts=gridPts[noShocksGSpec[gSpec]],
thisFunc=anREFunc[aPos]},
With[{forInterpFunc=Function @@ 
{xxVars,myExpect[modSpecific,thisFunc@@xxVars,anEpsVar,expctSpec]}},
evalAtInterpPts[forInterpFunc,thePts,iOrd]]]

evalAtInterpPts[aFunc_Function,thePts_List,iOrd_Integer]:=
Interpolation[{#,aFunc @@ #} & /@ thePts,InterpolationOrder->iOrd]



makeREFunc[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,
xtm1_?MatrixQ,noZFuncsGuess_,
{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},zFuncsNow_List,expctSpec:{{anEpsVar_,aDist_},opts_:{}}]:=
With[{xxVars=Table[Unique["xInterpRE"],{iterStateDim}]},
Function @@ {pos,
Function @@ {xxVars,
fpForInitStateFunc[modSpecific,xxVars,{anEpsVar},zFuncsNow,pos]}}]



ageOneZFunc[listOfFuncs_List,zFunc_]:=
With[{xArgs=Table[Unique["ageZVar"],{Length[listOfFuncs]}]},
Function @@{xArgs,
zFunc @@ Through[(listOfFuncs @@ # )&[ xArgs]]}]

ageZFuncs[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},{}]:={}


ageZFuncs[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},zFuncs_List]:=
With[{listOfFuncs=zFuncs[[Range[iterStateDim]]]},
ageOneZFunc[listOfFuncs,#]&/@Drop[zFuncs,iterStateDim]]



Options[genFinalRE]=Options[forIOrdNPtsRE]=Options[iterRE]=Options[makeInterpFuncRE]=Options[doScalarIntegInterp]=Options[myExpect]=Options[NExpectation]

forIOrdNPtsRE[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},start_List,expctSpec:{{anEpsVar_,aDist_},opts_:{}},maxLen_Integer]:=
NestList[(Print["applying iterRE>"];
Identity[iterRE[modSpecific,iOrd,gSpec,#,expctSpec]])&,start,maxLen];

midGrid[gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}]:=
Mean[Drop[#,1]]&/@ gSpec//N

makeInitGuess[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},xtm1Val:{_?NumberQ..},epsVal:{_?NumberQ..},
zFuncs_List]:=
With[{epsPart=phimat . psieps . Transpose[{epsVal}]},
(*Print["epsPart=",{epsPart,epsPart[[stateSel]],epsVal}];*)
initGuess=Flatten[If[Length[zFuncs]==0,
Through[noZFuncsGuess@@#&[Join[xtm1Val,epsVal]]],
With[{fromZs=Through[(zFuncs[[Range[iterStateDim]]]@@#&)[xtm1Val]]},
With[{},fromZs+0*epsPart[[stateSel]]]]]]]


makeInterpFuncFinal[theFunc_Function,xtm1_?MatrixQ,
pos_List,iOrder_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}
]:=Module[{thePts=
gridPts[gSpec]},
With[{whl={#,theFunc @@ #}& /@
thePts},
doScalarInterp[whl,#,iOrder]&/@pos]]/;
With[{anArg=midGrid[gSpec]},
With[{theRes=theFunc@@ anArg},Print["iPtsFinal:theRes=",theRes//InputForm];
NumberQ[Plus @@ theRes[[pos]]]]]

makeInterpFuncPF[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
theFunc_Function,pos_List,iOrder_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}
]:=Module[{thePts=gridPts[gSpec[[Range[iterStateDim]]]],
xVars=Table[Unique["xForPF"],{iterStateDim}]},
With[{pfFunc=Function @@ {xVars,theFunc @@ xVars}},
With[{whl={#,pfFunc @@ #}& /@
thePts},
doScalarInterp[whl,#,iOrder]&/@pos]]]/;
With[{anArg=midGrid[gSpec[[Range[iterStateDim]]]]},
With[{theRes=theFunc@@anArg},Print["iPtsPF:theRes=",{anArg(*,theFunc//InputForm,theRes//InputForm*)}];
NumberQ[Plus @@ theRes[[pos]]]]]




oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]




gridPts[rngs:{{_?NumberQ,_?NumberQ,_?NumberQ}..}]:=
With[{funcForPts=(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]] @#) &},
With[{oneDimPts=funcForPts/@rngs},
With[{theOuter=Outer[List,Sequence@@#]&[oneDimPts]},
Flatten[theOuter,Depth[theOuter]-3]]]]




doScalarIntegration[whlList:{{{_?NumberQ..},_}..},pos_Integer,iOrder_Integer]:=
Module[{},(*Print["doScalarIntegration:",whlList//InputForm];*)
With[{prtList={#[[1]],(#[[2]]/.thePos->pos)}&/@whlList},
Interpolation[prtList,InterpolationOrder->iOrder]]]




doScalarInterp[whlList:{{{_?NumberQ..},{_?NumberQ..}}..},pos_Integer,iOrder_Integer]:=
With[{prtList={#[[1]],#[[2,pos]]}&/@whlList},
Interpolation[prtList,InterpolationOrder->iOrder]]




makeInterpFuncPF[
modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},
theFunc_Function,iOrder_Integer,
gSpec:{{_Integer,_?NumberQ,_?NumberQ}..}]:=
With[{anArg=midGrid[gSpec[[Range[iterStateDim]]]]},
With[{pos=Range[Length[theFunc @@ anArg]]},(*Print["make pos=",
{pos,anArg,theFunc//InputForm,theFunc@@ anArg}];*)
makeInterpFuncPF[modSpecific,theFunc,pos,iOrder,gSpec]]]







genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,
	psic_?MatrixQ,psiz_?MatrixQ,numNonZeroZs_Integer,padZeroZs_Integer]:=
genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,
numNonZeroZs,padZeroZs]=
With[{startPath=
genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,numNonZeroZs]},
With[{tailPath=NestList[((nonFPart[#,
{{0}},bmat,phimat,fmat,psieps,psic]))&,
startPath[[-Reverse[Range[Length[bmat]]]]],padZeroZs]},
Join[startPath,Join@@Drop[tailPath,1]]]]

Print["genPath assumes only one shock"]
genPath[xtm1_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,
psic_?MatrixQ,psiz_?MatrixQ,
numNonZeroZs_Integer]:=
genPath[xtm1,
bmat,phimat,fmat,psieps,psic,psiz,
numNonZeroZs]=
With[{numCon=Length[psiz[[1]]]},
With[{rawFParts=Reverse[(doFPart[phimat,fmat,psiz,#,numCon,0] &/@Range[0,numNonZeroZs-1])]},
With[{bgn=(nonFPart[xtm1,
{{ProtectedSymbols`eps}},bmat,phimat,fmat,psieps,psic]+rawFParts[[1]])},
Join[xtm1,Join @@ FoldList[(nonFPart[#1,{{0}},bmat,phimat,fmat,psieps,psic]+#2)&,bgn,Drop[rawFParts,1]]]]]]


Print["genPath assumes only one shock"]
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


nonFPart=Compile[{{xtm1,_Real,2},{epsilon,_Real,2},
{bmat,_Real,2},{phimat,_Real,2},{fmat,_Real,2},{psimat,_Real,2},{psic,_Real,2}},
bmat . xtm1 + phimat . psimat . epsilon + 
Inverse[IdentityMatrix[Length[xtm1]]-fmat] . phimat . psic]
	
	

genZVars[horizons_Integer,numConstr_Integer]:=
genZVars[horizons,numConstr,0]
(*
genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]/;offset<=0
*)

genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Module[{},
genZVars[horizons,numConstr,offset]=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]]/;offset<=0

genEpsVars[numShocks_Integer]:=
Table[
{makeProtectedSymbol["eps$"<>ToString[ii]]},{ii,numShocks}]



End[]

EndPackage[]
*)
Print["done reading AMASeriesRepresentation`"]
