(* Wolfram Language Package *)

BeginPackage["betterRSRBC`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetterRSRBC::usage="for test input";
anEpsBetterRSRBC::usage="for test input";
anXEpsBetterRSRBC::usage="for test input";
aZBetterRSRBC::usage="for test input";
anXEpsZsBetterRSRBC::usage="for test input";

anXFlatBetterRSRBC::usage="for test input";
anEpsFlatBetterRSRBC::usage="for test input";
anXEpsFlatBetterRSRBC::usage="for test input";
aZFlatBetterRSRBC::usage="for test input";
anXEpsZsFlatBetterRSRBC::usage="for test input";

probDimsBetterRSRBC::usage="for test input";

simpRBCExactDRBetterRSRBC::usage="simpRBCExactDR"

theDistBetterRSRBC::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterRSRBC::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterRSRBC::usage="linear model matrices for approx"
aGSpecBetterRSRBC::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsFunctionalBetterRSRBC::usage="model equations function"

silly=Compile[{xx},Switch[xx,1.,77.,2.,32.]]


Begin["`Private`"] (* Begin Private Context *) 








CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)


(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100,
rho->95/100,
sigma->1/100
} ;


forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];

rbcEqns=Switch[rgmt,
0,rbcEqns00,
1,rbcEqns01
]

compArgSubs={
rgm[t-1]->rgmtm1,cc[t-1]->cctm1,kk[t-1]->kktm1,nlPart[t-1]->nltm1,theta[t-1]->thtm1,
rgm[t]->rgmt,cc[t]->cct,kk[t]->kkt,nlPart[t]->nlt,theta[t]->tht,
rgm[t+1]->rgmtp1,cc[t+1]->cctp1,kk[t+1]->kktp1,nlPart[t+1]->nltp1,theta[t+1]->thtp1,
eps[theta][t]->epsVal
};

rbcEqns00=
{
CRRAUDrv[cc[t],1]-
(delta*(theta[t])*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t-1])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* CRRAUDrv[cc[t],1]),
theta[t]-((E^(rho*Log[theta[t-1]] + eps[theta][t]))+
(E^(rho*Log[theta[t-1]] + eps[theta][t]))
)
}/.compArgSubs//.paramSubs

rbcEqns01={
CRRAUDrv[cc[t],1]-
(delta*(theta[t])*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t-1])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* CRRAUDrv[cc[t],1]),
theta[t]-(
rgm[t](E^(rho*Log[theta[t-1]] + eps[theta][t]))+
(E^(rho*Log[theta[t-1]] + eps[theta][t]))
)
}/.compArgSubs//.paramSubs

(*
rbcEqnsFunctionalBetterRSRBC=Compile @@ {
{
{rgmtm1,_Integer},{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thtm1,_Real},
{rgmt,_Integer},{cct,_Real},{kkt,_Real},{nlt,_Real},{tht,_Real},
{rgmtp1,_Integer},{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thtp1,_Real},
{epsVal,_Real}
},
Which[
rgmt== 0, 
{cct^(-1) - (171*nltp1*
    tht)/(500*kkt^(16/25)), 
 cct + kkt - 
  kktm1^(9/25)*thtm1, 
 -cct^(-1) + nlt, 
 tht - E^epsVal*
   thtm1^(19/20) - E^epsVal*
   rgmt*thtm1^(19/20)},
rgmt==1,
{cct^(-1) - (171*nltp1*
    tht)/(500*kkt^(16/25)), 
 cct + kkt - 
  kktm1^(9/25)*thtm1, 
 -cct^(-1) + nlt, 
 tht - E^epsVal*
   thtm1^(19/20) - E^epsVal*
   rgmt*thtm1^(19/20)}],(*{{Which[___],_Real,1}},*)
"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}
*)

Needs["CompiledFunctionTools`"]

Print["should compile these functions in betterRSRBC.m"];
rbcEqnsFunctionalBetterRSRBC=
{Function @@ {
{
rgmtm1,cctm1,kktm1,nltm1,thtm1,
rgmt,cct,kkt,nlt,tht,
rgmtp1,cctp1,kktp1,nltp1,thtp1,
epsVal
},
{cct^(-1) - (171*nltp1*
    tht)/(500*kkt^(16/25)), 
 cct + kkt - 
  kktm1^(9/25)*thtm1, 
 -cct^(-1) + nlt, 
 tht - E^epsVal*
   thtm1^(19/20) - E^epsVal*
   thtm1^(19/20)}},
Function @@ {
{
rgmtm1,cctm1,kktm1,nltm1,thtm1,
rgmt,cct,kkt,nlt,tht,
rgmtp1,cctp1,kktp1,nltp1,thtp1,
epsVal
},
{cct^(-1) - (171*nltp1*
    tht)/(500*kkt^(16/25)), 
 cct + kkt - 
  kktm1^(9/25)*thtm1, 
 -cct^(-1) + nlt, 
 tht - E^epsVal*
   thtm1^(19/20) - E^epsVal*
   thtm1^(19/20)}}}


(*

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
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];
Print["RE done now PF"];
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];




simpRBCExactDRBetterRSRBC = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]]



theDistBetterRSRBC={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterRSRBC={{{ee,PerfectForesight}}};



betterRSRBCExactCondExp = makeREIterFunc[betterRSRBCExactDR,theDist]


psiz=IdentityMatrix[4]

Print["RE solutions"]
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

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterRSRBC={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};



    
anXBetterRSRBC=Transpose[{{.2,.18,1.0,1.1}}];
anEpsBetterRSRBC={{0.01}};
anXEpsBetterRSRBC=Join[anXBetterRSRBC,anEpsBetterRSRBC]
aZBetterRSRBC=Transpose[{{.1,.2,.3,.4}}]
anXEpsZsBetterRSRBC=Join[anXEpsBetterRSRBC,aZBetterRSRBC];

anXFlatBetterRSRBC=anXBetterRSRBC//Flatten;
anEpsFlatBetterRSRBC=anEpsBetterRSRBC//Flatten;
anXEpsFlatBetterRSRBC=anXEpsBetterRSRBC//Flatten;
aZFlatBetterRSRBC=aZBetterRSRBC//Flatten;
anXEpsZsFlatBetterRSRBC=anXEpsZsBetterRSRBC//Flatten;

probDimsBetterRSRBC={4,1,4};


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



(*one regime*)
aGSpecBetterRSRBC={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}},1};
probMat={{1}}

*)
End[] (* End Private Context *)

EndPackage[]
