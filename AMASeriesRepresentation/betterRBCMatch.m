(* Wolfram Language Package *)

BeginPackage["betterRBCMatch`", 
{ "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetterMatch::usage="for test input";
anEpsBetterMatch::usage="for test input";
anXEpsBetterMatch::usage="for test input";
aZBetterMatch::usage="for test input";
anXEpsZsBetterMatch::usage="for test input";

anXFlatBetterMatch::usage="for test input";
anEpsFlatBetterMatch::usage="for test input";
anXEpsFlatBetterMatch::usage="for test input";
aZFlatBetterMatch::usage="for test input";
anXEpsZsFlatBetterMatch::usage="for test input";

probDimsBetterMatch::usage="for test input";
theDistBetterMatch::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterMatch::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterMatch::usage="linear model matrices for approx"
aGSpecBetterMatch::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
eqnsCompiledBetterMatch::usage="model equations function"
rbcEqnsBetterMatch::usage="model equations"
eqnsEulerCompiledBetterMatch::usage="eqnsEulerCompiledBetterMatch"


Begin["`Private`"] (* Begin Private Context *) 











(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
lam[t] -1/cc[t],
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -(nlPartRHS=lam[t]*theta[t]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
lam[t] - alpha*kk[t]^(-1+alpha)*delta*nlPart[t+1]
}



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




rbcEqnsBetterMatch=eqnsCompiledBetterMatch=Compile @@ {
{
{cctm1,_Real},{kktm1,_Real},{lamtm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{lamt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{lamtp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{
-cct^(-1) + lamt , 
 cct + kkt - (kktm1^.36)*thetat, 
nlt - lamt*thetat, 
 thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20),
 lamt - .36*(kkt^(-1 + .36))*0.95*nltp1
},
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






theDistBetterMatch={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterMatch={{{ee,PerfectForesight}}};










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

linModBetterMatch={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};




anXBetterMatch=Transpose[{{.2,.18,1.0,99,1.01}}];
anEpsBetterMatch={{0.01}};
anXEpsBetterMatch=Join[anXBetterMatch,anEpsBetterMatch]
aZBetterMatch=Transpose[{{.1,.2,.3,.4,.5}}]
anXEpsZsBetterMatch=Join[anXEpsBetterMatch,aZBetterMatch];

anXFlatBetterMatch=anXBetterMatch//Flatten;
anEpsFlatBetterMatch=anEpsBetterMatch//Flatten;
anXEpsFlatBetterMatch=anXEpsBetterMatch//Flatten;
aZFlatBetterMatch=aZBetterMatch//Flatten;
anXEpsZsFlatBetterMatch=anXEpsZsBetterMatch//Flatten;

probDimsBetterMatch={5,1,5};



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
aGSpecBetterMatch={{1,3,4},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
	 aGSpecBetterMatch={{1,3,4},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};*)

	 aGSpecBetterMatch={{1,3,4},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};
End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBCMatch.m"]
