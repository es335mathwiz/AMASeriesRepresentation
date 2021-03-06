(* Wolfram Language Package *)

BeginPackage["betterRBCTrips`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  



anXBetterRBCTrips::usage="for test input";
anEpsBetterRBCTrips::usage="for test input";
anXEpsBetterRBCTrips::usage="for test input";
aZBetterRBCTrips::usage="for test input";
anXEpsZsBetterRBCTrips::usage="for test input";

anXFlatBetterRBCTrips::usage="for test input";
anEpsFlatBetterRBCTrips::usage="for test input";
anXEpsFlatBetterRBCTrips::usage="for test input";
aZFlatBetterRBCTrips::usage="for test input";
anXEpsZsFlatBetterRBCTrips::usage="for test input";

probDimsBetterRBCTrips::usage="for test input";
simpRBCExactDRBetterTrips::usage="simpRBCExactDR"
betterRBCTripsExactCondExp::usage="betterRBCTripsExactCondExp"
theDistBetterRBCTrips::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterRBCTrips::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterRBCTrips::usage="linear model matrices for approx"
aGSpecBetterRBCTrips::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
eqnsCompiledBetterRBCTrips::usage="model equations function"
rbcEqnsBetterRBCTrips::usage="model equations"
eqnsEulerCompiledBetterRBCTrips::usage="eqnsEulerCompiledBetterRBCTrips"
betterRBCTripsExactXZ::usage="betterExactXZ"
betterRBCTripsExactZ::usage="betterExactZ"
simulateBetterRBCExactTrips::usage="simulateBetterRBCTripsRBCExact[numPers_Integer]"
betterRBCTripsMean::usage="betterRBCTripsMean"
betterRBCTripsSD::usage="betterRBCTripsSD"
betterRBCTripsvv::usage="betterRBCTripsvv"
betterRBCTripsMinZ::usage="betterRBCTripsMinZ"
betterRBCTripsMaxZ::usage="betterRBCTripsMaxZ"

Begin["`Private`"] (* Begin Private Context *) 








CRRAUDrv[cc_,eta_]:=If[eta===1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
CRRAUDrv[cc[t],1]-
(delta*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* (theta[t]*CRRAUDrv[cc[t],1])),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t])
}

discMapEqns00=(Append[rbcEqns[[{1,2,3}]],
(rbcEqns[[4]]/.{xx_-yy_->Log[xx]-Log[yy]})]/.{Log[betterRBCTrips`Private`theta[zz__]]->lnTheta[zz],theta[xx__]->E^lnTheta[xx]})//PowerExpand

zfEqns=discMapEqns00[[{2,3,4}]]//PowerExpand
discMapEqns01=Append[zfEqns/.t->t+1,discMapEqns00[[1]]]//PowerExpand
(*soln=Solve[Thread[discMapEqns01==0],{cc[t+1],kk[t+1],nlPart[t+1],lnTheta[t+1]}]*)

(*parameters page 21 using state 1*)
paramSubs={
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01
} ;


forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];


rbcCompileGuts=(rbcEqns/.{
cc[t-1]->cctm1,kk[t-1]->kktm1,nlPart[t-1]->nltm1,theta[t-1]->thtm1,
cc[t]->cct,kk[t]->kkt,nlPart[t]->nlt,theta[t]->tht,
cc[t+1]->cctp1,kk[t+1]->kktp1,nlPart[t+1]->nltp1,theta[t+1]->thtp1,
eps[theta][t]->epsVal
}//.paramSubs)//N


  rbcEqnsBetterRBCTrips=eqnsCompiledBetterRBCTrips={
 { {True&,
  Compile @@ {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
({cct^(-1) - (alpha*delta*nltp1)/kkt^(1-alpha),
cct + kkt - 1.*kktm1^(alpha)*thetat, 
nlt - thetat/cct,
  thetat - ((N[E]^epsVal)*(thetatm1^(rho)))}/.paramSubs),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},
   True&},
 {True&,
  Compile @@ {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
({cct^(-1) - (alpha*delta*nltp1)/kkt^(1-alpha),
cct + kkt - 1.*kktm1^(alpha)*thetat, 
nlt - thetat/cct,
  thetat - ((N[E]^epsVal)*(thetatm1^(rho)))}/.paramSubs),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},
   True&}},
Function[{aPt,allRes},
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Flatten[allRes[[2]]],Flatten[allRes[[1]]]]]}


(*
theFRExt01=genFRExtFunc[{4,1,4},linModBetterRBCTrips,{genX0Z0Funcs[linModBetterRBCTrips],2},eqnsCompiledBetterRBCTrips,
"xVarRanges"->{{0.01,2},{0.01,2},{0.01,20},{0.85,1.3}}];

causes error a
CompiledFunction::cfn: 
   Numerical error encountered at instruction 2; proceeding with
     uncompiled evaluation.

eqnsCompiledBetterRBCTrips  @@ Flatten[{{1}, {0.0187324}, {1}, {1.1}, {0.293437}, {-0.0351748},      {7.51431}, {1.08125}, {0.232894}, {0.120986}, {3.96721}, 
     {1.07709}, {-0.0124264}}]

*)
(*
eqnsEulerCompiledBetterRBCTrips=Compile @@ {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{((kkt^(16/25)) - (0.342*nltp1)*cct)/cct,
cct + kkt - 1.*kktm1^(9/25)*thetat, 
nlt - thetat/cct,
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)},"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}
*)

Needs["CompiledFunctionTools`"]





(*If[Length[ssSolnSubsRE]===0,*)
(*Print["computing steady state subs"];*)
thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
anExpPF=1;
Off[Solve::ifun]
thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta][[2]]];
kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk][[-1]]];
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];
(*Print["RE done now PF"];*)
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
On[Solve::ifun]
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];




simpRBCExactDRBetterTrips = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]]




theDistBetterRBCTrips={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterRBCTrips={{{ee,PerfectForesight}}};





betterRBCTripsExactCondExp = (*AMASeriesRepresentation`Private`*)makeREIterFunc[simpRBCExactDRBetterTrips,theDistBetterRBCTrips]


betterExactZ=
Function[{cc, kk, nl, th, eps},
With[{hm=getH[linModBetterRBCTrips],pc=getPsiC[linModBetterRBCTrips],pe=getPsiEps[linModBetterRBCTrips],
xt=Flatten[simpRBCExactDRBetterTrips[cc,kk,nl,th,eps]]},
With[{xtp1=Flatten[betterRBCTripsExactCondExp @@ xt]},
hm.Transpose[{Join[{cc,kk,nl,th},xt,xtp1]}]-pc-pe*eps]]]

betterExactXZ=
Function[{cc, kk, nl, th, eps},
With[{xval=simpRBCExactDRBetterTrips[cc,kk,nl,th,eps],
zval=betterExactZ[cc,kk,nl,th,eps]},
Join[xval,zval]]]

psiz=IdentityMatrix[4]

(*Print["RE solutions"]*)
hmatSymbSlowRawRE00=(((equationsToMatrix[
rbcEqns]//FullSimplify)));
hmatSymbSlowRawRE01=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)))//FullSimplify;
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[4]]]+hmatSymbRE[[All,4+Range[4]]]+hmatSymbRE[[All,8+Range[4]]];

ssSolnVecRE={{cc},{kk},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

(*Print["computing and simplifying the symbolic b phi f etc"]*)
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterRBCTrips={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{}};
		     




    
anXBetterRBCTrips=Transpose[{{.2,.18,1.0,1.01}}];
anEpsBetterRBCTrips={{0.01}};
anXEpsBetterRBCTrips=Join[anXBetterRBCTrips,anEpsBetterRBCTrips]
aZBetterRBCTrips=Transpose[{{.1,.2,.3,.4}}]
anXEpsZsBetterRBCTrips=Join[anXEpsBetterRBCTrips,aZBetterRBCTrips];

anXFlatBetterRBCTrips=anXBetterRBCTrips//Flatten;
anEpsFlatBetterRBCTrips=anEpsBetterRBCTrips//Flatten;
anXEpsFlatBetterRBCTrips=anXEpsBetterRBCTrips//Flatten;
aZFlatBetterRBCTrips=aZBetterRBCTrips//Flatten;
anXEpsZsFlatBetterRBCTrips=anXEpsZsBetterRBCTrips//Flatten;

probDimsBetterRBCTrips={4,1,4};


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc //.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;
kHigh = 1.2*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;


simulateBetterRBCExactTrips[numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterRBCTrips[[1,1,2]],numPers],
initVec={99,kVal,99,thVal}},
FoldList[Flatten[simpRBCExactDRBetterTrips@@ Append[Flatten[#1],#2]]&,initVec,draws]]

aGSpecBetterRBCTrips={{1,3},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
	(*	
	 aGSpecBetterRBCTrips={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};*)
	(*
	 aGSpecBetterRBCTrips={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,sigHigh}}};
	 *)

   theRes=simulateBetterRBCExactTrips[200];
justKT=theRes[[All,{2,4}]];
betterRBCTripsMean=Mean[justKT]
betterRBCTripsSD=StandardDeviation[justKT]
normedRes=(#/betterRBCTripsSD)&/@((#-betterRBCTripsMean)&/@justKT)
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
betterRBCTripsMinZ=Min/@Transpose[zz];
betterRBCTripsMaxZ=Max/@Transpose[zz];
{ig,theKs,ig,theThetas}=Transpose[theRes];

betterRBCTripsMean=Append[betterRBCTripsMean,0]
betterRBCTripsSD=Append[betterRBCTripsSD,sigVal]
betterRBCTripsMinZ=Append[betterRBCTripsMinZ,-3]
betterRBCTripsMaxZ=Append[betterRBCTripsMaxZ,3]
betterRBCTripsvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0},{0}}}}]},{{{0,0,1}}}}]



zPts=backXtoZ[Transpose[{theKs,theThetas,Table[0,{Length[theKs]}]}],betterRBCTripsMean,betterRBCTripsSD,betterRBCTripsvv];



End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBCTrips.m"]
