(* Wolfram Language Package *)

BeginPackage["betterRBCCompSlack`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetterCompSlack::usage="for test input";
anEpsBetterCompSlack::usage="for test input";
anXEpsBetterCompSlack::usage="for test input";
aZBetterCompSlack::usage="for test input";
anXEpsZsBetterCompSlack::usage="for test input";

anXFlatBetterCompSlack::usage="for test input";
anEpsFlatBetterCompSlack::usage="for test input";
anXEpsFlatBetterCompSlack::usage="for test input";
aZFlatBetterCompSlack::usage="for test input";
anXEpsZsFlatBetterCompSlack::usage="for test input";

probDimsBetterCompSlack::usage="for test input";
theDistBetterCompSlack::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterCompSlack::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterCompSlack::usage="linear model matrices for approx"
aGSpecBetterCompSlack::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
eqnsCompiledBetterCompSlack::usage="model equations function"
rbcEqnsBetterCompSlack::usage="model equations"
eqnsEulerCompiledBetterCompSlack::usage="eqnsEulerCompiledBetterCompSlack"


Begin["`Private`"] (* Begin Private Context *) 












(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)





discMapEqns00=(Append[rbcEqns[[{1,2,3}]],
(rbcEqns[[4]]/.{xx_-yy_->Log[xx]-Log[yy]})]/.{Log[betterRBC`Private`theta[zz__]]->lnTheta[zz],theta[xx__]->E^lnTheta[xx]})//PowerExpand

zfEqns=discMapEqns00[[{2,3,4}]]//PowerExpand
discMapEqns01=Append[zfEqns/.t->t+1,discMapEqns00[[1]]]//PowerExpand
(*soln=Solve[Thread[discMapEqns01==0],{cc[t+1],kk[t+1],nlPart[t+1],lnTheta[t+1]}]*)

(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
beta->1,
delta->95/100,
rho->95/100,
sigma->1/100,
dd->1
} ;


forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];



rbcEqns={
lam[t] -1/cc[t],
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -(nlPartRHS=lam[t]*theta[t]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
lam[t] +mu[t] - (alpha*kk[t]^(-1+alpha)*delta*nlPart[t+1]+lam[t+1]*delta*(1-dd)+mu[t+1]*delta*(1-dd)),
mu[t]*(kk[t]-(1-dd)*kk[t-1]-upsilon*Iss)
}
(*
((betterRBCCompSlack`Private`rbcEqns/.betterRBCCompSlack`Private`paramSubs)
/.{
eps[betterRBCCompSlack`Private`theta][t]->epsVal,
betterRBCCompSlack`Private`cc[t-1]->cctm1,
betterRBCCompSlack`Private`kk[t-1]->kktm1,
betterRBCCompSlack`Private`lam[t-1]->lamtm1,
betterRBCCompSlack`Private`nlPart[t-1]->nltm1,
betterRBCCompSlack`Private`theta[t-1]->thetatm1,
betterRBCCompSlack`Private`cc[t]->cct,
betterRBCCompSlack`Private`kk[t]->kkt,
betterRBCCompSlack`Private`lam[t]->lamt,
betterRBCCompSlack`Private`nlPart[t]->nlt,
betterRBCCompSlack`Private`theta[t]->thetat,
betterRBCCompSlack`Private`cc[t+1]->cctp1,
betterRBCCompSlack`Private`kk[t+1]->kktp1,
betterRBCCompSlack`Private`lam[t+1]->lamtp1,
betterRBCCompSlack`Private`nlPart[t+1]->nltp1,
betterRBCCompSlack`Private`theta[t+1]->thetat
})//InputForm


*)
rbcEqnsBetterCompSlack=eqnsCompiledBetterCompSlack=Compile @@ {
{
{cctm1,_Real},{kktm1,_Real},{lamtm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{lamt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{lamtp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{-cct^(-1) + lamt, cct + kkt - kktm1^(9/25)*thetat, nlt - lamt*thetat, 
 thetat - E^epsVal*thetatm1^(19/20), 
 lamt - (171*nltp1)/(500*kkt^(16/25))},
"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}




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
lamSSSubRE=lam->(1/cc)/.cSSSubRE;
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE,lamSSSubRE}];
(*Print["RE done now PF"];*)
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
On[Solve::ifun]
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];






theDistBetterCompSlack={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterCompSlack={{{ee,PerfectForesight}}};










psiz=IdentityMatrix[5]

(*Print["RE solutions"]*)
hmatSymbSlowRawRE00=(((equationsToMatrix[
rbcEqns]//FullSimplify)));
hmatSymbSlowRawRE01=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)))//FullSimplify;
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[5]]]+hmatSymbRE[[All,5+Range[5]]]+hmatSymbRE[[All,10+Range[5]]];

ssSolnVecRE={{cc},{kk},{lam},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;



{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];


(*Print["computing and simplifying the symbolic b phi f etc"]*)
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterCompSlack={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};




anXBetterCompSlack=Transpose[{{.2,.18,1.0,99,1.01}}];
anEpsBetterCompSlack={{0.01}};
anXEpsBetterCompSlack=Join[anXBetterCompSlack,anEpsBetterCompSlack]
aZBetterCompSlack=Transpose[{{.1,.2,.3,.4,.5}}]
anXEpsZsBetterCompSlack=Join[anXEpsBetterCompSlack,aZBetterCompSlack];

anXFlatBetterCompSlack=anXBetterCompSlack//Flatten;
anEpsFlatBetterCompSlack=anEpsBetterCompSlack//Flatten;
anXEpsFlatBetterCompSlack=anXEpsBetterCompSlack//Flatten;
aZFlatBetterCompSlack=aZBetterCompSlack//Flatten;
anXEpsZsFlatBetterCompSlack=anXEpsZsBetterCompSlack//Flatten;

probDimsBetterCompSlack={5,1,5};



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

	(*
aGSpecBetterCompSlack={{1,3,4},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
	 aGSpecBetterCompSlack={{1,3,4},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};*)

	 aGSpecBetterCompSlack={{1,3,4},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};
End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBC.m"]
