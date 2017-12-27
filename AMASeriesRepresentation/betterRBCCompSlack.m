(* Wolfram Language Package *)

BeginPackage["betterRBCCompSlack`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
rbcEqnsBetterCSTrips::usage="rbcEqnsBetterCSTrips"
eqnsCompiledBetterCSTrips::usage="eqnsCompiledBetterCSTrips"

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
rbcEqnsBetterBackLookingCompSlack::usage="rbcEqnsBetterBackLookingCompSlack"
rbcEqnsBetterBackLookingExpCompSlack::usage="rbcEqnsBetterBackLookingCompSlack"
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
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01,
dd->1,
upsilon->0.975
} ;

*)
(*parameters page 28 guerrieri iacoviello*)

paramSubs={
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01,
dd->.1,
upsilon->0.975
} ;



forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs,simpSubs];



rbcEqnsBinding={
lam[t] -1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -(nlPartRHS=lam[t]*theta[t]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
lam[t] +mu1[t] - (alpha*kk[t]^(-1+alpha)*delta*nlPart[t+1]+lam[t+1]*delta*(1-dd)+mu1tp1*delta*(1-dd)),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
(kk[t]-(1-dd)*kk[t-1]-upsilon*IIss)
}
rbcEqnsNotBinding={
lam[t] -1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -(nlPartRHS=lam[t]*theta[t]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t]),
lam[t] +mu1[t] - (alpha*kk[t]^(-1+alpha)*delta*nlPart[t+1]+lam[t+1]*delta*(1-dd)+mu1[t+1]*delta*(1-dd)),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
mu1[t]
}
boolNotBinding= And[(kk[t]-(1-dd)*kk[t-1]-upsilon*IIss)>0,mu1==0]
  boolBinding= And[(kk[t]-(1-dd)*kk[t-1]-upsilon*IIss)==0,mu1>=0]
rbcBackLookingEqns={E^(rho*Log[theta[t-1]] + eps[theta][t])}
rbcBackLookingExpEqns={Expectation[rbcBackLookingEqns[[1]],eps[theta][t] \[Distributed] NormalDistribution[0,sigma]]}

ssEqnSubs=
{xx_Symbol[t+v_.]->xx}
rbcEqnsNotBindingSubbed=((rbcEqnsNotBinding/.paramSubs)/.eps[theta][t]->0)


Print[{forFR,theVars=Cases[Variables[forFR=(rbcEqnsNotBindingSubbed/.ssEqnSubs)],_Symbol]}]




frArg=Transpose[{theVars,{.3599,.187,.187,0,0,4,1}}]

frArg=MapThread[Prepend[#1,#2]&,{{{.3599,.9},{.187,.35},{.187,.9},{1.,9.},{-.01,0.1},{1.,9.},{.9,1.1}},theVars}]



Print[
ssFRSolnSubs=Prepend[Chop[FindRoot[forFR,frArg,MaxIterations->1000(*,WorkingPrecision->50*)]],IIss->.372634]]
Print["errs=",(forFR )//.ssFRSolnSubs]


(*
{-cc^(-1) + lam, cc + kk - kk^0.36*theta, nlPart - lam*theta, 
 -theta^0.95 + theta, 0.9524999999999999*lam + 0.9524999999999999*mu1 - 
  (0.34199999999999997*nlPart)/kk^0.64, II - 0.95*kk, mu1}


.1
{-cc^(-1) + lam, cc + kk - kk^0.36*theta, nlPart - lam*theta, 
 -theta^0.95 + theta, 0.14500000000000002*lam + 
  0.14500000000000002*mu1 - (0.34199999999999997*nlPart)/kk^0.64, 
 II - 0.09999999999999998*kk, mu1}


*)

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

thePatterns=makeBlankPatternArgs[Last/@argsSubs]
lagEpsPatterns=makeBlankPatternArgs[Last/@(Append[argsSubs[[Range[7]]],argsSubs[[-1]]])]
lagPatterns=makeBlankPatternArgs[Last/@(argsSubs[[Range[7]]])]

genCompSlackEqns[alpha_?NumberQ,beta_?NumberQ,delta_?NumberQ,rho_?NumberQ,sigma_?NumberQ,dd_?NumberQ,upsilon_?NumberQ]:=
Module[{},
With[{eqnsName=Unique["eqnsName"],eqnsBackLookingName=Unique["eqnsBLName"],eqnsBackLookingExpName=Unique["eqnsBLExpName"],
theGuts=Flatten[
({(rbcEqnsNotBinding/.paramSubs)/.argsSubs}/.ssSolnSubsRE)//N],
theBLGuts=Flatten[
({(rbcBackLookingEqns/.paramSubs)/.argsSubs}/.ssSolnSubsRE)//N],
theBLExpGuts=Flatten[
({(rbcBackLookingExpEqns/.paramSubs)/.argsSubs}/.ssSolnSubsRE)//N]
},
SetDelayed[
eqnsName[Apply[Sequence,thePatterns]],theGuts];DistributeDefinitions[eqnsName];
SetDelayed[
eqnsBackLookingName[Apply[Sequence,lagEpsPatterns]],theBLGuts];DistributeDefinitions[eqnsName];
SetDelayed[
eqnsBackLookingExpName[Apply[Sequence,lagPatterns]],theBLExpGuts];DistributeDefinitions[eqnsName];
{eqnsName,eqnsBackLookingName,eqnsBackLookingExpName}]]




(*
{-1./cct + lamt, cct + kkt - 1.*kktm1^0.33*thetat, nlt - 1.*lamt*thetat, 
 thetat - 1.*2.718281828459045^epsVal*thetatm1^0.9, 
 lamt - 0.864*lamtp1 + mu1t - 0.864*mu1tp1 - 
  (0.3168*nltp1)/kkt^0.6699999999999999, iit - 1.*kkt + 0.9*kktm1, 
 mu1t}


*)






eqnsForBind=(((betterRBCCompSlack`Private`rbcEqnsBinding/.betterRBCCompSlack`Private`paramSubs)
/.{
eps[betterRBCCompSlack`Private`theta][t]->epsVal,
betterRBCCompSlack`Private`cc[t-1]->Global`cctm1,
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
})//.
betterRBCCompSlack`Private`ssFRSolnSubs)//N

	
(*
dd->.1
{-1./cct + lamt, cct + iit - 1.*kktm1^0.36*thetat, nlt - 1.*lamt*thetat, 
 thetat - 1.*2.718281828459045^epsVal*thetatm1^0.95, 
 lamt - 0.855*lamtp1 + mu1t - 0.855*betterRBCCompSlack`Private`mu1tp1 - 
  (0.34199999999999997*nltp1)/kkt^0.64, iit - 1.*kkt + 0.9*kktm1, 
 -0.37263436422602103 + kkt - 0.9*kktm1}

*)

eqnsForNotBind=(((betterRBCCompSlack`Private`rbcEqnsNotBinding/.betterRBCCompSlack`Private`paramSubs)
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
})//.
betterRBCCompSlack`Private`ssFRSolnSubs)//N

(*

dd->.1
{-1./cct + lamt, cct + iit - 1.*kktm1^0.36*thetat, nlt - 1.*lamt*thetat, 
 thetat - 1.*2.718281828459045^epsVal*thetatm1^0.95, 
 lamt - 0.855*lamtp1 + mu1t - 0.855*mu1tp1 - 
  (0.34199999999999997*nltp1)/kkt^0.64, iit - 1.*kkt + 0.9*kktm1, mu1t}

*)

theProduct=0.9755*IIss//.ssFRSolnSubs;
  rbcEqnsBetterCompSlack=eqnsCompiledBetterCompSlack={
 { {(Print["pre1"];True)&,
  Compile @@ {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},
  Function[Print["func:",{#8,#9,#10,#11,#3,#9 >(theProduct)}];#9>(theProduct)]},
 {(Print["pre2"];True)&,
  Compile @@ {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},(Print["post2"];True)&}},
Function[{aPt,allRes},
If[allRes[[1]]===$Failed,Print["constraint violated"];Flatten[allRes[[2]]],Print["constraint not violated"];Flatten[allRes[[1]]]]]
}







theDistBetterCompSlack={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterCompSlack={{{ee,PerfectForesight}}};










psiz=IdentityMatrix[7]

(*Print["RE solutions"]*)
hmatSymbRawRE=(((equationsToMatrix[
rbcEqnsNotBinding/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssFRSolnSubs)/.{eps[_]->0}//FullSimplify;

psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqnsNotBinding)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssFRSolnSubs}/.simpParamSubs]




hmatSymbRE=hmatSymbRawRE//.simpParamSubs
hSumRE=hmatSymbRE[[All,Range[7]]]+hmatSymbRE[[All,7+Range[7]]]+hmatSymbRE[[All,2*7+Range[7]]];





ssSolnVecRE={{cc},{II},{kk},{lam},{mu1},{nlPart},{theta}}//.ssFRSolnSubs;
psicSymbRE=hSumRE . ssSolnVecRE;



{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];



{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];


qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];


(*Print["computing and simplifying the symbolic b phi f etc"]*)
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

(*
{rbcEqnsBetterCompSlack,rbcEqnsBetterBackLookingCompSlack,rbcEqnsBetterBackLookingExpCompSlack}=genCompSlackEqns @@ {alpha,beta,delta,rho,sigma,dd,upsilon}/.simpParamSubs
*)

linModBetterCompSlack={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,(*{{7,rbcEqnsBetterBackLookingCompSlack,rbcEqnsBetterBackLookingExpCompSlack}}*){}};Print["took out back looking equations"];

(*

*)





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



thVal=(theta//.ssFRSolnSubs//.(simpParamSubs//N))//N;
kVal = (kk //.ssFRSolnSubs//.(simpParamSubs//N))//N;
cVal = (cc //.ssFRSolnSubs//.(simpParamSubs//N))//N ;
(*following guerrieri and iacoviello Appendix A*)
kLow = kVal*.95//N;
kHigh = kVal*1.4//N;
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





End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBCCompSlack.m"]
