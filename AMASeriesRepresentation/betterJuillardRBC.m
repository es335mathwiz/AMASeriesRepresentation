(* Wolfram Language Package *)

BeginPackage["betterJuillardRBC`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetterJuillard::usage="for test input";
anEpsBetterJuillard::usage="for test input";
anXEpsBetterJuillard::usage="for test input";
aZBetterJuillard::usage="for test input";
anXEpsZsBetterJuillard::usage="for test input";

anXFlatBetterJuillard::usage="for test input";
anEpsFlatBetterJuillard::usage="for test input";
anXEpsFlatBetterJuillard::usage="for test input";
aZFlatBetterJuillard::usage="for test input";
anXEpsZsFlatBetterJuillard::usage="for test input";

probDimsBetterJuillard::usage="for test input";

simpRBCExactDRBetterJuillard::usage="simpRBCExactDR"

theDistBetterJuillard::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterJuillard::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterJuillard::usage="linear model matrices for approx"
aGSpecBetterJuillard::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsFunctionalNextBetterJuillard::usage="model equations function"
Begin["`Private`"] (* Begin Private Context *) 








CRRAUDrv[ccStar_,eta_]:=If[eta==1,D[Log[ccStar],ccStar],D[(1/(1-eta))*(ccStar^(1-eta)-1),ccStar]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
CRRAUDrv[ccStar[t],1]-
(delta*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
ccStar[t] + kk[t]-((1-dd)*kk[t-1]+(theta[t])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* theta[t]*CRRAUDrv[ccStar[t],1]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
ii[t] -(kk[t]-(1-dd)*kk[t-1]),
cc[t]-ccStar[t]
}


rbcEqnsCompSlack={
CRRAUDrv[ccStar[t],1]-
(delta*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
ccStar[t] + kk[t]-((1-dd)*kk[t-1]+(theta[t])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)*theta[t]* CRRAUDrv[ccStar[t],1]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
ii[t] -(kk[t]-(1-dd)*kk[t-1]),
cc[t] - Min[ccStar[t],(theta[t-1])*(kk[t-1]^alpha) - gamma*iiSSVal]
}


(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100,
rho->95/100,
sigma->1/100,
dd->10/10,
gamma->  -10000/1000(* from luca paper*)
} ;


forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];





(*If[Length[ssSolnSubsRE]===0,*)
Print["computing steady state subs"];
thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
anExpPF=1;
thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta][[2]]];
kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk][[-1]]];
cStarSSSubRE=ccStar->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE],{cStarSSSubRE}];
ISSSubRE={ii->dd*(kk/.kSSSubRE),iiSSVal->dd*(kk/.kSSSubRE)}/.simpParamSubs;
ssSolnSubsRE=
Flatten[{thSubsRE,kSSSubRE,cSSSubRE,cStarSSSubRE,nlPartSSSubRE,ISSSubRE}];
Print["RE done now PF"];
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
cStarSSSubPF=ccStar->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,cStarSSSubPF,thSubsPF};
  ISSSubPF={ii->dd*kSSSubPF,iiSSVal->dd*(kk/.kSSSubPF)}/.simpParamSubs;
ssSolnSubsPF=
Flatten[{thSubsPF,kSSSubPF,cSSSubPF,cStarSSSubPF,nlPartSSSubPF,ISSSubPF}];



Print["okay to check"]

simpRBCExactDRBetterJuillard = 
 Function[{cc,ccStar, ii,kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{ccStart=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
With[{iit=kkt-(1-dd)*kk//.simpParamSubs//N},
Transpose[{{ccStart,ccStart,iit,kkt,tht/ccStart,tht}}]]]]]]



theDistBetterJuillard={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterJuillard={{{ee,PerfectForesight}}};



(*betterJuillardRBCExactCondExp = makeREIterFunc[betterJuillardRBCExactDR,theDist]
*)

psiz=IdentityMatrix[6]

Print["RE solutions"]==0
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

Print["here mmm"]


hmatSymbRE=hmatSymbRawRE//.simpSubs//.simpParamSubs
hSumRE=hmatSymbRE[[All,Range[6]]]+hmatSymbRE[[All,6+Range[6]]]+hmatSymbRE[[All,12+Range[6]]];

Print["here nnn"]

ssSolnVecRE={{cc},{ccStar},{ii},{kk},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;

Print["here ooo"]

{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

Print["here zzz"]

linModBetterJuillard={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};

Print["here yyy"]


    
anXBetterJuillard=Transpose[{{.1,.2,.3,.18,1.0,1.1}}];
anEpsBetterJuillard={{0.01}};
anXEpsBetterJuillard=Join[anXBetterJuillard,anEpsBetterJuillard]
aZBetterJuillard=Transpose[{{.1,.2,.3,.4,.2,.1}}]
anXEpsZsBetterJuillard=Join[anXEpsBetterJuillard,aZBetterJuillard];

anXFlatBetterJuillard=anXBetterJuillard//Flatten;
anEpsFlatBetterJuillard=anEpsBetterJuillard//Flatten;
anXEpsFlatBetterJuillard=anXEpsBetterJuillard//Flatten;
aZFlatBetterJuillard=aZBetterJuillard//Flatten;
anXEpsZsFlatBetterJuillard=anXEpsZsBetterJuillard//Flatten;

probDimsBetterJuillard={6,1,6};


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
cStarVal = (ccStar //.cStarSSSubRE//.(simpParamSubs//N))//N ;
cVal = (ccStar //.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;(*to accStaromodate range in Luca toolkit paper*)
kHigh = 4*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;

aGSpecBetterJuillard={{1,2,3,5},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};



rbcCompileGuts=((rbcEqnsCompSlack/.{
cc[t-1]->cctm1,ccStar[t-1]->ccStartm1,kk[t-1]->kktm1,nlPart[t-1]->nltm1,theta[t-1]->thetatm1,
ii[t-1]->iitm1,
cc[t]->cct,ccStar[t]->ccStart,kk[t]->kkt,nlPart[t]->nlt,theta[t]->thetat,
ii[t]->iit,
cc[t+1]->cctp1,ccStar[t+1]->ccStartp1,kk[t+1]->kktp1,nlPart[t+1]->nltp1,theta[t+1]->thetatp1,
ii[t+1]->iitp1,
eps[theta][t]->epsVal
}//.ssSolnSubsRE)//.paramSubs)//N

funcArgs=
{cctm1,ccStartm1, iitm1, kktm1,  nltm1, thetatm1, 
cct,ccStart, iit, kkt, nlt, thetat, 
cctp1,ccStartp1, iitp1, kktp1,  nltp1, thetatp1, epsVal};


Print["here xxx"]

rbcEqnsFunctionalNextBetterJuillard=Function  @@ {funcArgs,rbcCompileGuts}
(*
rbcEqnsFunctionalNextBetterJuillard=Compile @@ {
{
{cctm1,_Real},{ccStartm1,_Real},{iitm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{ccStart,_Real},{iit,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{ccStartp1,_Real},{iitp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(rbcCompileGuts),
"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}
*)

Needs["CompiledFunctionTools`"]




End[] (* End Private Context *)

EndPackage[]
