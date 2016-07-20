(* Wolfram Language Package *)

BeginPackage["betterZeroFRBC`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetterZeroF::usage="for test input";
anEpsBetterZeroF::usage="for test input";
anXEpsBetterZeroF::usage="for test input";
aZBetterZeroF::usage="for test input";
anXEpsZsBetterZeroF::usage="for test input";

anXFlatBetterZeroF::usage="for test input";
anEpsFlatBetterZeroF::usage="for test input";
anXEpsFlatBetterZeroF::usage="for test input";
aZFlatBetterZeroF::usage="for test input";
anXEpsZsFlatBetterZeroF::usage="for test input";

probDimsBetterZeroF::usage="for test input";

simpRBCExactDRBetterZeroF::usage="simpRBCExactDR"
betterZeroFRBCExactCondExp::usage="betterZeroFRBCExactCondExp"
theDistBetterZeroF::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterZeroF::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterZeroF::usage="linear model matrices for approx"
aGSpecBetterZeroF::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsFunctionalNextBetterZeroF::usage="model equations function"
Begin["`Private`"] (* Begin Private Context *) 








CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



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
cc[t-1]->cctm1,kk[t-1]->kktm1,nlPart[t-1]->nltm1,theta[t-1]->thtm1,
cc[t]->cct,kk[t]->kkt,nlPart[t]->nlt,theta[t]->tht,
cc[t+1]->cctp1,kk[t+1]->kktp1,nlPart[t+1]->nltp1,theta[t+1]->thtp1,
eps[theta][t]->epsVal
}//.paramSubs)//N

rbcEqnsFunctionalNextBetterZeroF=Compile @@ {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*((1.)*(nltp1(*thetatp1/cctp1*))))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
nlt - thetat/cct,
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)},"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}


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
cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE);
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];
Print["RE done now PF"];
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];




simpRBCExactDRBetterZeroF = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]]



theDistBetterZeroF={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterZeroF={{{ee,PerfectForesight}}};



betterZeroFRBCExactCondExp = makeREIterFunc[simpRBCExactDRBetterZeroF,theDist]


psiz=IdentityMatrix[4]

Print["RE solutions"]
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



{zfSymbSlowRawRE,hfSymbSlowRawRE}=symbolicAR[hmatSymbSlowRawRE00];


amatSymbSlowRawRE=symbolicTransitionMatrix[hfSymbSlowRawRE];
{evlsSymbSlowRawRE,evcsSymbSlowRawRE}=Eigensystem[Transpose[amatSymbSlowRawRE]];
			 (*

qmatSymbSlowRawRE=Join[zfSymbSlowRawRE,evcsSymbSlowRawRE[[{1}]]];


			  *)


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;


tryHmat=ArrayFlatten[{{bmatSymbRE,-IdentityMatrix[4],ConstantArray[0,{4,4}]}}]


linModBetterZeroF={tryHmat//N,bmatSymbRE // N, -IdentityMatrix[4] // N, 
    0*fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};



    
anXBetterZeroF=Transpose[{{.2,.18,1.0,1.1}}];
anEpsBetterZeroF={{0.01}};
anXEpsBetterZeroF=Join[anXBetterZeroF,anEpsBetterZeroF]
aZBetterZeroF=Transpose[{{.1,.2,.3,.4}}]
anXEpsZsBetterZeroF=Join[anXEpsBetterZeroF,aZBetterZeroF];

anXFlatBetterZeroF=anXBetterZeroF//Flatten;
anEpsFlatBetterZeroF=anEpsBetterZeroF//Flatten;
anXEpsFlatBetterZeroF=anXEpsBetterZeroF//Flatten;
aZFlatBetterZeroF=aZBetterZeroF//Flatten;
anXEpsZsFlatBetterZeroF=anXEpsZsBetterZeroF//Flatten;

probDimsBetterZeroF={4,1,4};


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
aGSpecBetterZeroF={{1,3},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
	 aGSpecBetterZeroF={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};*)

	 aGSpecBetterZeroF={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};
End[] (* End Private Context *)

EndPackage[]
