(* Mathematica Package *)

(* Created by the Wolfram Workbench Jul 27, 2015 *)

Print["reading AMASeriesRepresentation`"]

BeginPackage["AMASeriesRepresentation`", {"JLink`","ProtectedSymbols`"}]

Begin["Private`"]


makePFFuncs[czFuncs_Function,numModVars_Integer,numShocks_Integer]:=
With[{modArgs=Table[Unique["xvar"],{numModVars}]},
Function[modVars,czFuncs @@ Join[modVars,Table[0,{numShocks}]]]]



makeConstraintFindRootFunc[hmFunc_Function,
	linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},xtm1_?MatrixQ,epst_?MatrixQ,
	ZZks:{_Function...},xxNext_?MatrixQ,xxNextp1_?MatrixQ,zzNext_?MatrixQ,zzGuesser:{_Function...},xxGuess_?MatrixQ]:=
With[{flatXtm1Eps=Flatten[Join[xtm1,epst]],xxTargets=Flatten[Join[xxNext,xxNextp1]]},
With[{zzGuess=If[zzGuesser=={},Table[0,{Length[zzNext]}],Through[zzGuesser[flatXtm1Eps]]]},
	With[{findRootArg=Transpose[{Flatten[zzNext],zzGuess}]},
ReplacePart[Function[theArgs,
	With[{forZSubs=Flatten[Join[computeNextXt[linMod,xtm1,epst,ZZks,zzNext,xxGuess],
		computeNextXtp1[linMod,xtm1,epst,ZZks,zzNext,xxGuess]]],
		zSubs=
FindRoot[Thread[
	(subXtXtp1[hmFunc,linMod,
		xtm1,epst,ZZks,zzNext,xxGuess]//N//Expand//Simplify)==0],
		findRootArg]},
		Join[Thread[xxTargets->(forZSubs/.zSubs)],zSubs]]
		],1->flatXtm1Eps]]]]


computeNonFPart[BB_?MatrixQ,phi_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,xtm1_?MatrixQ,epst_?MatrixQ]:=
computeNonFPart[BB,phi,psiEps,psiC,xtm1,epst]=(BB.xtm1 + phi.psiEps.epst+
Inverse[IdentityMatrix[Length[xtm1]]-fmat] . phimat . psic)

computeFPower[FF_?MatrixQ,kk_Integer]:=
computeFPower[FF,kk]=FF .computeFPower[FF,kk-1]/;kk>=1
computeFPower[FF,0]:=IdentityMatrix[Length[FF]]

computeFPartK[FF_?MatrixQ,phi_?MatrixQ,psiZ_?MatrixQ,kk_Integer,ZZk_?MatrixQ]:=
computeFPartK[FF,phi,psiZ,kk,ZZk]=FF.phi.psiZ.ZZk

computeFPart[FF_?MatrixQ,phi_?MatrixQ,psiZ_?MatrixQ,ZZks:{_Function...},xxGuess_?MatrixQ]:=
With[{kk=Length[ZZks],zzkMats=Transpose[{#@@ Flatten[xxGuess]}]&/@ZZks},
Sum[computeFPartK[FF,phi,psiZ,ii,zzkMats[[ii]]],{ii,kk}]]

computeNextXt[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},xtm1_?MatrixQ,epst_?MatrixQ,
	ZZks:{_Function...},zzNext_?MatrixQ,xxGuess_?MatrixQ]:=
Module[{},
computeNextXt[linMod,xtm1,epst,ZZks,zzNext,xxGuess]=
computeNonFPart[BB,phi,psiEps,psiC,xtm1,epst]+
computeFPart[FF,phi,psiZ,ZZks,xxGuess]+phi.zzNext]


computeNextXtp1[linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},xtm1_?MatrixQ,epst_?MatrixQ,
	ZZks:{_Function...},zzNext_?MatrixQ,xxGuess_?MatrixQ]:=
Module[{},
computeNextXtp1[linMod,xtm1,epst,ZZks,zzNext,xxGuess]=
With[{xt=computeNextXt[linMod,xtm1,epst,ZZks,zzNext,xxGuess]},
	computeNextXt[linMod,xt,0*epst,
		If[Length[ZZks]>0,Drop[ZZks,1],{}],
		If[Length[ZZks]>0,Transpose[{ZZks[[1]]@@Flatten[xxGuess]}],0*zzNext],xxGuess]]]

Print["should memoize subXtXtp1"]
subXtXtp1[aFunc_Function,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ},xtm1_?MatrixQ,epst_?MatrixQ,
	ZZks:{_Function...},zzNext_?MatrixQ,xxGuess_?MatrixQ]:=
	With[{xt=computeNextXt[linMod,xtm1,epst,ZZks,zzNext,xxGuess],
		xtp1=computeNextXtp1[linMod,xtm1,epst,ZZks,zzNext,xxGuess]},
		aFunc[xtm1,xt,xtp1,epst]]

applyZs[theFuncs:{_InterpolatingFunction[___]..},xtm1_?MatrixQ,epst_?MatrixQ]:=
Through[theFuncs[Join[xtm1,epst]]]

genNextZVars[numConstr_Integer]:=
Module[{},
genNextZVars[numConstr]=
Table[
makeProtectedSymbol["zzzVar$"<>"$"<>ToString[ii]],{ii,numConstr}]]/;And[numConstr>=0]


genNextXVars[numVars_Integer]:=
Module[{},
genNextXVars[numVars]=
Table[
makeProtectedSymbol["xxxVar$"<>"$"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]



gridPts[rngs:{{_?NumberQ,_?NumberQ,_?NumberQ}..}]:=
With[{funcForPts=(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]] @#) &},
With[{oneDimPts=funcForPts/@rngs},
With[{theOuter=Outer[List,Sequence@@#]&[oneDimPts]},
Flatten[theOuter,Depth[theOuter]-3]]]]





oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]



(*
http://mathematica.stackexchange.com/questions/1803/how-to-compile-effectively
computeNonFPart=Compile[{{BB,_Real,2},{phi,_Real,2},{psiEps,_Real,2},{xtm1,_Real,2},{epst,_Real,2}},
	BB . xtm1 + phi.psiEps . epst]
	
computeFPartk=Compile[{{FF,_Real,2},{phi,_Real,2},{kk,_Integer},{ZZk,_Real,2}},
	computeFPower[FF,kk] . phi . ZZk]
	


	
*)	
	
End[]
EndPackage[]

(*
(* Exported symbols added here with SymbolName::usage *) 
genFinalRE::usage=
"genFinalPF[modSpecific:{{bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ},"<>
"compCon:{_Function...},stateSel_List,xtm1_?MatrixQ,noZFuncsGuess_,"<>
	"{iterStateDim_Integer,neq_Integer,nlag_Integer,nlead_Integer,nShocks_Integer},fpSolver_},"<>
"iOrd_Integer,gSpec:{{_Integer,_?NumberQ,_?NumberQ}..},"<>
"initFuncs_List,,expctSpec:{{anEpsVar_,aDist_},opts_:{}},iters_Integer]"
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
Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]


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
