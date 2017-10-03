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

genCompSlackEqns::usage="genCompSlackEqns[alpha_?NumberQ,beta_?NumberQ,delta_?NumberQ,rho_?NumberQ,sigma_?NumberQ,dd_?NumberQ,upsilon_?NumberQ]"
Begin["`Private`"] (* Begin Private Context *) 











(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)






(*parameters page 21 using state 1*)
(*
paramSubs={
alpha->.33,
beta->1,
delta->.95,
rho->.9,
sigma->.013,
dd->1,
upsilon->0.975
} ;
*)

(*parameters page 28 guerrieri iacoviello*)
paramSubs={
alpha->.36,
beta->1,
delta->.96,
rho->.95,
sigma->.01,
dd->1,
upsilon->0.975
} ;


forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs,simpSubs];



rbcEqns={
lam[t] -1/cc[t],
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -(nlPartRHS=lam[t]*theta[t]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
lam[t] +mu1[t] - (alpha*kk[t]^(-1+alpha)*delta*nlPart[t+1]+lam[t+1]*delta*(1-dd)+mu1[t+1]*delta*(1-dd)),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
mu1[t]*(kk[t]-(1-dd)*kk[t-1]-upsilon*IIss)
}

argsSubs={
cc[t-1]->cctm1,
II[t-1]->iitm1,
kk[t-1]->kktm1,
lam[t-1]->lamtm1,
mu1[t-1]->mu1tm1,
nlPart[t-1]->nltm1,
theta[t-1]->thetatm1,
cc[t]->cct,
II[t]->iit,
kk[t]->kkt,
lam[t]->lamt,
mu1[t]->mu1t,
nlPart[t]->nlt,
theta[t]->thetat,
cc[t+1]->cctp1,
II[t+1]->iitp1,
kk[t+1]->kktp1,
lam[t+1]->lamtp1,
mu1[t+1]->mu1tp1,
nlPart[t+1]->nltp1,
theta[t+1]->thetatp1,
eps[theta][t]->epsVal
}

thePatterns=makePatternArgs[Last/@argsSubs]


genCompSlackEqns[alpha_?NumberQ,beta_?NumberQ,delta_?NumberQ,rho_?NumberQ,sigma_?NumberQ,dd_?NumberQ,upsilon_?NumberQ]:=
Module[{},
With[{eqnsName=Unique["eqnsName"],theGuts=Flatten[
({(rbcEqns/.paramSubs)/.argsSubs}/.ssSolnSubsRE)//N]
},
SetDelayed[
eqnsName[Apply[Sequence,thePatterns]],theGuts];DistributeDefinitions[eqnsName];eqnsName]
]





(*
(((betterRBCCompSlack`Private`rbcEqns/.betterRBCCompSlack`Private`paramSubs)
/.{
eps[betterRBCCompSlack`Private`theta][t]->epsVal,
betterRBCCompSlack`Private`cc[t-1]->cctm1,
betterRBCCompSlack`Private`II[t-1]->iitm1,
betterRBCCompSlack`Private`kk[t-1]->kktm1,
betterRBCCompSlack`Private`lam[t-1]->lamtm1,
betterRBCCompSlack`Private`mu1[t-1]->mu1tm1,
betterRBCCompSlack`Private`nlPart[t-1]->nltm1,
betterRBCCompSlack`Private`theta[t-1]->thetatm1,
betterRBCCompSlack`Private`cc[t]->cct,
betterRBCCompSlack`Private`II[t]->iit,
betterRBCCompSlack`Private`kk[t]->kkt,
betterRBCCompSlack`Private`lam[t]->lamt,
betterRBCCompSlack`Private`mu1[t]->mu1t,
betterRBCCompSlack`Private`nlPart[t]->nlt,
betterRBCCompSlack`Private`theta[t]->thetat,
betterRBCCompSlack`Private`cc[t+1]->cctp1,
betterRBCCompSlack`Private`II[t+1]->iitp1,
betterRBCCompSlack`Private`kk[t+1]->kktp1,
betterRBCCompSlack`Private`lam[t+1]->lamtp1,
betterRBCCompSlack`Private`mu1[t+1]->mu1tp1,
betterRBCCompSlack`Private`nlPart[t+1]->nltp1,
betterRBCCompSlack`Private`theta[t+1]->thetat
})/.
betterRBCCompSlack`Private`ssSolnSubsRE)//N//InputForm

*)





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
IISSubRE=({IIss->(dd*kk/.kSSSubRE),II->(dd*kk/.kSSSubRE)}/.simpParamSubs);
mu1SSSubRE=mu1->0;
pos1SSSubRE=pos1->0;
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE,lamSSSubRE,IISSubRE,mu1SSSubRE,pos1SSSubRE}];
(*Print["RE done now PF"];*)
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
On[Solve::ifun]
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];





theDistBetterCompSlack={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterCompSlack={{{ee,PerfectForesight}}};










psiz=IdentityMatrix[7]

(*Print["RE solutions"]*)
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;

psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]




hmatSymbRE=hmatSymbRawRE//.simpParamSubs
hSumRE=hmatSymbRE[[All,Range[7]]]+hmatSymbRE[[All,7+Range[7]]]+hmatSymbRE[[All,2*7+Range[7]]];





ssSolnVecRE={{cc},{II},{kk},{lam},{mu1},{nlPart},{theta}}//.ssSolnSubsRE;
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

(*

*)

rbcEqnsBetterCompSlack=genCompSlackEqns @@ {alpha,beta,delta,rho,sigma,dd,upsilon}/.simpParamSubs




anXBetterCompSlack=Transpose[{{99,99,.18,99,99,99,1.01}}];
anEpsBetterCompSlack={{0.01}};
anXEpsBetterCompSlack=Join[anXBetterCompSlack,anEpsBetterCompSlack]
aZBetterCompSlack=Transpose[{{.1,.2,.3,.4,.5,.6,.7}}]
anXEpsZsBetterCompSlack=Join[anXEpsBetterCompSlack,aZBetterCompSlack];

anXFlatBetterCompSlack=anXBetterCompSlack//Flatten;
anEpsFlatBetterCompSlack=anEpsBetterCompSlack//Flatten;
anXEpsFlatBetterCompSlack=anXEpsBetterCompSlack//Flatten;
aZFlatBetterCompSlack=aZBetterCompSlack//Flatten;
anXEpsZsFlatBetterCompSlack=anXEpsZsBetterCompSlack//Flatten;

probDimsBetterCompSlack={7,1,7};



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
aGSpecBetterCompSlack={{1,2,4,5,6},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
	 aGSpecBetterCompSlack={{1,2,4,5,6},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};*)

	 aGSpecBetterCompSlack={{1,2,4,5,6},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};

(*
{alpha,beta,delta,rho,sigma,dd,upsilon}
*)

tryit=genCompSlackEqns[.36,1,.95,.95,.01,1,0.975]

End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBCCompSlack.m"]
