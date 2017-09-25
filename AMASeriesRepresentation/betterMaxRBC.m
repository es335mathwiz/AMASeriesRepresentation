(* Wolfram Language Package *)

BeginPackage["betterMaxRBC`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetterMax::usage="for test input";
anEpsBetterMax::usage="for test input";
anXEpsBetterMax::usage="for test input";
aZBetterMax::usage="for test input";
anXEpsZsBetterMax::usage="for test input";

anXFlatBetterMax::usage="for test input";
anEpsFlatBetterMax::usage="for test input";
anXEpsFlatBetterMax::usage="for test input";
aZFlatBetterMax::usage="for test input";
anXEpsZsFlatBetterMax::usage="for test input";

probDimsBetterMax::usage="for test input";

(*simpRBCExactDRBetterMax::usage="simpRBCExactDR"*)

theDistBetterMax::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterMax::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterMax::usage="linear model matrices for approx"
aGSpecBetterMax::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsBetterMax::usage="model equations function"
rbcBetterCons::usage="rbcBetterCons[cctm1_,IItm1_,kktm1_, nltm1_, thetatm1_, VVtm1_, cct_, IIt_, kkt_, nlt_, thetat_, VVt_, cctp1_, IItp1_, kktp1_, nltp1_, thetatp1_, VVtp1_, epsVal_]"
objBetterMax::usage="objBetterMax[cctm1_,IItm1_,kktm1_, nltm1_, thetatm1_, VVtm1_, cct_, IIt_, kkt_, nlt_, thetat_, VVt_, cctp1_, IItp1_, kktp1_, nltp1_, thetatp1_, VVtp1_, epsVal_]"

Begin["`Private`"] (* Begin Private Context *) 








CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]
CRRAU[cc_,eta_]:=If[eta==1,Log[cc],(1/(1-eta))*(cc^(1-eta)-1)]


(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
CRRAUDrv[cc[t],1]-
(delta*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* theta[t]*CRRAUDrv[cc[t],1]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
VV[t] - (Log[cc[t]] +delta*VV[t+1])
}





(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100,
rho->95/100,
sigma->1/100,
dd->1/10,
gamma->0/1000(* from luca paper*)
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
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ISSSubRE={II->dd*(kk/.kSSSubRE),IISSVal->dd*(kk/.kSSSubRE)}/.simpParamSubs;VVSSSubRE=VV->((Log[cc]/(1-delta))/.cSSSubRE)/.simpParamSubs;
ssSolnSubsRE=
Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE,ISSSubRE,VVSSSubRE}];
Print["RE done now PF"];
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ISSSubPF={II->dd*kSSSubPF,IISSVal->dd*kSSSubPF}/.simpParamSubs;VVSSSubPF=VV->((Log[cc]/(1-delta))/.cSSSubPF)/.simpParamSubs;
ssSolnSubsPF=
Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF,ISSSubPF,VVSSSubPF}];



Print["okay to check"]

(*simpRBCExactDRBetterMax = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]]

*)

theDistBetterMax={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterMax={{{ee,PerfectForesight}}};



(*betterMaxRBCExactCondExp = makeREIterFunc[betterMaxRBCExactDR,theDist]
*)

psiz=IdentityMatrix[6]

Print["RE solutions"]==0
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[6]]]+hmatSymbRE[[All,6+Range[6]]]+hmatSymbRE[[All,12+Range[6]]];

ssSolnVecRE={{cc},{II},{kk},{nlPart},{theta},{VV}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1,2}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterMax={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};



    
anXBetterMax=Transpose[{{99,99,.18,99,1.01,99}}];
anEpsBetterMax={{0.01}};
anXEpsBetterMax=Join[anXBetterMax,anEpsBetterMax]
aZBetterMax=Transpose[{{.1,.2,.3,.4,.2,.1}}]
anXEpsZsBetterMax=Join[anXEpsBetterMax,aZBetterMax];

anXFlatBetterMax=anXBetterMax//Flatten;
anEpsFlatBetterMax=anEpsBetterMax//Flatten;
anXEpsFlatBetterMax=anXEpsBetterMax//Flatten;
aZFlatBetterMax=aZBetterMax//Flatten;
anXEpsZsFlatBetterMax=anXEpsZsBetterMax//Flatten;

probDimsBetterMax={6,1,6};


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc //.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 0.8*kVal//N;(*to accomodate range in Luca toolkit paper*)
kHigh = 1.2*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;

aGSpecBetterMax={{1,2,4,6},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};



objBetterMax[cctm1_,IItm1_,kktm1_, nltm1_, thetatm1_, VVtm1_, cct_, IIt_, kkt_, nlt_, thetat_, VVt_, cctp1_, IItp1_, kktp1_, nltp1_, thetatp1_, VVtp1_, epsVal_]:=Log[cct]+ 0.95*VVtp1


rbcBetterCons[cctm1_,IItm1_,kktm1_, nltm1_, thetatm1_, VVtm1_, cct_, IIt_, kkt_, nlt_, thetat_, VVt_, cctp1_, IItp1_, kktp1_, nltp1_, thetatp1_, VVtp1_, epsVal_]:=
Module[{},{
 cct + kkt  - 1.*kktm1^(9/25)*thetat==0, 
 nlt - thetat/cct==0,
 thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)==0, 
 IIt - (1.*kkt - 0.9*kktm1)==0, 
 VVt - (Log[cct]+0.95*VVtp1)==0}]

Needs["CompiledFunctionTools`"]




End[] (* End Private Context *)

EndPackage[]
