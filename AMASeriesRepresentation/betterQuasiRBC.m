(* Wolfram Language Package *)

BeginPackage["betterQuasiRBC`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetterQuasi::usage="for test input";
anEpsBetterQuasi::usage="for test input";
anXEpsBetterQuasi::usage="for test input";
aZBetterQuasi::usage="for test input";
anXEpsZsBetterQuasi::usage="for test input";

anXFlatBetterQuasi::usage="for test input";
anEpsFlatBetterQuasi::usage="for test input";
anXEpsFlatBetterQuasi::usage="for test input";
aZFlatBetterQuasi::usage="for test input";
anXEpsZsFlatBetterQuasi::usage="for test input";

probDimsBetterQuasi::usage="for test input";

simpRBCExactDRBetterQuasi::usage="simpRBCExactDR"

theDistBetterQuasi::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterQuasi::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterQuasi::usage="linear model matrices for approx"
aGSpecBetterQuasi::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsFunctionalNextBetterQuasi::usage="model equations function"
rbcEqnsFunctionBetterQuasi::usage="rbcEqnsFunctionBetterQuasi"
Begin["Private`"] (* Begin Private Context *) 








CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
CRRAUDrv[cc[t],1]-
(delta*(theta[t])*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t-1])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* CRRAUDrv[cc[t],1]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
kkDrv[t] -0
}




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


rbcCompileGuts=(rbcEqns/.{
cc[t-1]->cctm1,kk[t-1]->kktm1,kkDrv[t-1]->kkDrvtm1,nlPart[t-1]->nltm1,theta[t-1]->thtm1,
cc[t]->cct,kk[t]->kkt,kkDrv[t]->kkDrvt,nlPart[t]->nlt,theta[t]->tht,
cc[t+1]->cctp1,kk[t+1]->kktp1,kkDrv[t+1]->kkDrvtp1,nlPart[t+1]->nltp1,theta[t+1]->thtp1,
eps[theta][t]->epsVal
}//.paramSubs)//N

rbcEqnsFunctionalNextBetterQuasi=Compile @@ {
args={
{cctm1,_Real},{kktm1,_Real},{kkDrvtm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{kkDrvt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{kkDrvtp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
eqns={cct^(-1) - (0.342*((1.)*(nltp1(*thetatp1/cctp1*))))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
nlt - thetat/cct,
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20),
kkDrvt},"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}


fArgs=First /@args;

rbcEqnsFunctionBetterQuasi=Function @@{fArgs,eqns};


Needs["CompiledFunctionTools`"]





(*If[Length[ssSolnSubsRE]===0,*)
Print["computing steady state subs"];
thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
anExpPF=1;
thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta][[2]]];
kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk][[-1]]];
kkDrvSSSubRE=kkDrv->0;
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE,kkDrvSSSubRE}];
Print["RE done now PF"];
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
kkDrvSSSubPF=kkDrv->0;
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF,kkDrvSSSubPF}];




simpRBCExactDRBetterQuasi = 
 Function[{cc, kk,kkDrv, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
With[{kkDrvt=0},
Transpose[{{cct,kkt,kkDrvt,tht/cct,tht}}]]]]]]



theDistBetterQuasi={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterQuasi={{{ee,PerfectForesight}}};



betterQuasiRBCExactCondExp = makeREIterFunc[betterQuasiRBCExactDR,theDist]


psiz=IdentityMatrix[5]

Print["RE solutions"]
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[5]]]+hmatSymbRE[[All,5+Range[5]]]+hmatSymbRE[[All,10+Range[5]]];

ssSolnVecRE={{cc},{kk},{kkDrv},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterQuasi={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};



    
anXBetterQuasi=Transpose[{{.2,.18,0,1.0,1.1}}];
anEpsBetterQuasi={{0.01}};
anXEpsBetterQuasi=Join[anXBetterQuasi,anEpsBetterQuasi]
aZBetterQuasi=Transpose[{{.1,.2,.3,.4,.5}}]
anXEpsZsBetterQuasi=Join[anXEpsBetterQuasi,aZBetterQuasi];

anXFlatBetterQuasi=anXBetterQuasi//Flatten;
anEpsFlatBetterQuasi=anEpsBetterQuasi//Flatten;
anXEpsFlatBetterQuasi=anXEpsBetterQuasi//Flatten;
aZFlatBetterQuasi=aZBetterQuasi//Flatten;
anXEpsZsFlatBetterQuasi=anXEpsZsBetterQuasi//Flatten;

probDimsBetterQuasi={5,1,5};


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
kkDrvVal = (kkDrv //.kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc //.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;
kHigh = 4*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;

aGSpecBetterQuasi={{1,3,4},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};

End[] (* End Private Context *)

EndPackage[]
