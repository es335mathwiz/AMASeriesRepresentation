(* Wolfram Language Package *)

BeginPackage["betterCnstrnRBC`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetterCnstrn::usage="for test input";
anEpsBetterCnstrn::usage="for test input";
anXEpsBetterCnstrn::usage="for test input";
aZBetterCnstrn::usage="for test input";
anXEpsZsBetterCnstrn::usage="for test input";

anXFlatBetterCnstrn::usage="for test input";
anEpsFlatBetterCnstrn::usage="for test input";
anXEpsFlatBetterCnstrn::usage="for test input";
aZFlatBetterCnstrn::usage="for test input";
anXEpsZsFlatBetterCnstrn::usage="for test input";

probDimsBetterCnstrn::usage="for test input";

(*simpRBCExactDRBetterCnstrn::usage="simpRBCExactDR"*)

theDistBetterCnstrn::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterCnstrn::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterCnstrn::usage="linear model matrices for approx"
aGSpecBetterCnstrn::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsFunctionalNextBetterCnstrn::usage="model equations function"
Begin["Private`"] (* Begin Private Context *) 








CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
CRRAUDrv[cc[t],1]-lambda[t]-
(delta*(theta[t])*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((1-dd)*kk[t-1]+(theta[t-1])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* CRRAUDrv[cc[t],1]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
lambda[t]*(II[t]-gamma*IISSVal)
}


rbcEqnsCompSlack={
CRRAUDrv[cc[t],1]-lambda[t]-
(delta*(theta[t])*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) ))))==0,
cc[t] + kk[t]-((1-dd)*kk[t-1]+(theta[t-1])*(kk[t-1]^alpha))==0,
nlPart[t] - (nlPartRHS=(1)* CRRAUDrv[cc[t],1])==0,
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t])==0,
II[t] -(kk[t]-(1-dd)*kk[t-1])==0,
(cc[t]>0&&(((II[t]>=gamma*IISSVal)&&lambda[t]==0)||
((II[t]==gamma*IISSVal)&&lambda[t]>=0)))
}




(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100,
rho->95/100,
sigma->1/100,
dd->1/10,
gamma->975/1000(* from luca paper*)
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
ISSSubRE={II->dd*(kk/.kSSSubRE),IISSVal->dd*(kk/.kSSSubRE)}/.simpParamSubs;lambdaSSSubRE=lambda->0;
ssSolnSubsRE=
Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE,ISSSubRE,lambdaSSSubRE}];
Print["RE done now PF"];
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ISSSubPF={II->dd*kSSSubPF,IISSVal->dd*kSSSubPF}/.simpParamSubs;lambdaSSSubPF=lambda->0;
ssSolnSubsPF=
Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF,ISSSubPF,lambdaSSSubPF}];



Print["okay to check"]

(*simpRBCExactDRBetterCnstrn = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]]

*)

theDistBetterCnstrn={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterCnstrn={{{ee,PerfectForesight}}};



(*betterCnstrnRBCExactCondExp = makeREIterFunc[betterCnstrnRBCExactDR,theDist]
*)

psiz=IdentityMatrix[6]

Print["RE solutions"]==0
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[6]]]+hmatSymbRE[[All,6+Range[6]]]+hmatSymbRE[[All,12+Range[6]]];

ssSolnVecRE={{cc},{II},{kk},{lambda},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterCnstrn={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};



    
anXBetterCnstrn=Transpose[{{.2,0,.18,0,1.0,1.1}}];
anEpsBetterCnstrn={{0.01}};
anXEpsBetterCnstrn=Join[anXBetterCnstrn,anEpsBetterCnstrn]
aZBetterCnstrn=Transpose[{{.1,.2,.3,.4,.2,.1}}]
anXEpsZsBetterCnstrn=Join[anXEpsBetterCnstrn,aZBetterCnstrn];

anXFlatBetterCnstrn=anXBetterCnstrn//Flatten;
anEpsFlatBetterCnstrn=anEpsBetterCnstrn//Flatten;
anXEpsFlatBetterCnstrn=anXEpsBetterCnstrn//Flatten;
aZFlatBetterCnstrn=aZBetterCnstrn//Flatten;
anXEpsZsFlatBetterCnstrn=anXEpsZsBetterCnstrn//Flatten;

probDimsBetterCnstrn={6,1,6};


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc //.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;
kHigh = 4*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;

aGSpecBetterCnstrn={{1,2,4,5},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};



rbcCompileGuts=((rbcEqnsCompSlack/.{
cc[t-1]->cctm1,kk[t-1]->kktm1,nlPart[t-1]->nltm1,theta[t-1]->thetatm1,
lambda[t-1]->lamtm1,II[t-1]->IItm1,
cc[t]->cct,kk[t]->kkt,nlPart[t]->nlt,theta[t]->thetat,
lambda[t]->lamt,II[t]->IIt,
cc[t+1]->cctp1,kk[t+1]->kktp1,nlPart[t+1]->nltp1,theta[t+1]->thetatp1,
lambda[t+1]->lamtp1,II[t+1]->IItp1,
eps[theta][t]->epsVal
}//.ssSolnSubsRE)//.paramSubs)//N

funcArgs=
{cctm1, IItm1, kktm1, lamtm1, nltm1, thetatm1, cct, IIt, kkt, lamt, nlt, 
 thetat, cctp1, IItp1, kktp1, lamtp1, nltp1, thetatp1, epsVal};




rbcEqnsFunctionalNextBetterCnstrn=Function  @@ {funcArgs,rbcCompileGuts}
(*
rbcEqnsFunctionalNextBetterCnstrn=Compile @@ {
{
{cctm1,_Real},{IItm1,_Real},{kktm1,_Real},{lamtm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{IIt,_Real},{kkt,_Real},{lamt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{IItp1,_Real},{kktp1,_Real},{lamtp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(rbcCompileGuts),
"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}
*)

Needs["CompiledFunctionTools`"]




End[] (* End Private Context *)

EndPackage[]
