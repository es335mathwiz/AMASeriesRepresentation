(* Wolfram Language Package *)

BeginPackage["betterRBCFixCompSlack`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
rbcEqnsBetterCSTrips::usage="rbcEqnsBetterCSTrips"
eqnsCompiledBetterCSTrips::usage="eqnsCompiledBetterCSTrips"

anXBetterFixCompSlack::usage="for test input";
anEpsBetterFixCompSlack::usage="for test input";
anXEpsBetterFixCompSlack::usage="for test input";
aZBetterFixCompSlack::usage="for test input";
anXEpsZsBetterFixCompSlack::usage="for test input";

anXFlatBetterFixCompSlack::usage="for test input";
anEpsFlatBetterFixCompSlack::usage="for test input";
anXEpsFlatBetterFixCompSlack::usage="for test input";
aZFlatBetterFixCompSlack::usage="for test input";
anXEpsZsFlatBetterFixCompSlack::usage="for test input";

probDimsBetterFixCompSlack::usage="for test input";
theDistBetterFixCompSlack::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterFixCompSlack::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterFixCompSlack::usage="linear model matrices for approx"
aGSpecBetterFixCompSlack::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsBetterBackLookingFixCompSlack::usage="rbcEqnsBetterBackLookingFixCompSlack"
rbcEqnsBetterBackLookingExpFixCompSlack::usage="rbcEqnsBetterBackLookingFixCompSlack"
eqnsCompiledBetterFixCompSlack::usage="model equations function"
rbcEqnsBetterFixCompSlack::usage="model equations"
eqnsEulerCompiledBetterFixCompSlack::usage="eqnsEulerCompiledBetterFixCompSlack"

genFixCompSlackEqns::usage="genFixCompSlackEqns[alpha_?NumberQ,beta_?NumberQ,delta_?NumberQ,rho_?NumberQ,sigma_?NumberQ,dd_?NumberQ,upsilon_?NumberQ]"
simulateBetterRBCCS::usage="simulateBetterRBCExact[numPers_Integer]"
betterRBCCSMean::usage="betterRBCCSMean"
betterRBCCSSD::usage="betterRBCCSSD"
betterRBCCSvv::usage="betterRBCCSvv"
betterRBCCSMinZ::usage="betterRBCCSMinZ"
betterRBCCSMaxZ::usage="betterRBCCSMaxZ"

chkBounded::usage="chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]"


iterateRBCCSDRCE::usage="iterateRBCCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]"


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


(*+mu1[t+1]*delta*(1-dd)*)
(*lam[t] +mu1[t] - (alpha*kk[t]^(-1+alpha)*delta*nlPart[t+1]+lam[t+1]*delta*(1-dd)+mu1[t+1]*delta*(1-dd))*)
(* +lam[t+1]*delta*(1-dd)*)




preRbcEqnsBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha)))-lam[t+1]*delta*(1-dd)+mu1[t]-mu1[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
mu1[t]
}

rbcEqnsNotBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha))) -lam[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1])+mu1[t]-mu1[t+1]*delta*(1-dd),
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

frArg=MapThread[Prepend[#1,#2]&,{{{.3599,2},{0,.35},{.187,.9},{-9.,9.},{-.01,0.1},{-9.,9.},{.9,1.1}},theVars}]



Print[
ssFRSolnSubs=Prepend[Chop[FindRoot[forFR,frArg,MaxIterations->1000(*,WorkingPrecision->50*)]],IIss->0]]
Print["errs=",(forFR )//.ssFRSolnSubs]

theProduct=upsilon*II//.ssFRSolnSubs/.betterRBCFixCompSlack`Private`paramSubs;

















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

genFixCompSlackEqns[alpha_?NumberQ,beta_?NumberQ,delta_?NumberQ,rho_?NumberQ,sigma_?NumberQ,dd_?NumberQ,upsilon_?NumberQ]:=
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



Print["here01"]


preRbcEqnsBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha)))-lam[t+1]*delta*(1-dd)+mu1[t]-mu1[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
mu1[t]
}


Print["here now"]
eqnsForBind=(((betterRBCFixCompSlack`Private`preRbcEqnsBinding/.betterRBCFixCompSlack`Private`paramSubs)/.{
eps[betterRBCFixCompSlack`Private`theta][t]->epsVal,
betterRBCFixCompSlack`Private`cc[t-1]->Global`cctm1,
betterRBCFixCompSlack`Private`II[t-1]->iitm1,
betterRBCFixCompSlack`Private`kk[t-1]->kktm1,
betterRBCFixCompSlack`Private`lam[t-1]->lamtm1,
betterRBCFixCompSlack`Private`mu1[t-1]->mu1tm1,
betterRBCFixCompSlack`Private`nlPart[t-1]->nltm1,
betterRBCFixCompSlack`Private`theta[t-1]->thetatm1,
betterRBCFixCompSlack`Private`cc[t]->cct,
betterRBCFixCompSlack`Private`II[t]->iit,
betterRBCFixCompSlack`Private`kk[t]->kkt,
betterRBCFixCompSlack`Private`lam[t]->lamt,
betterRBCFixCompSlack`Private`mu1[t]->mu1t,
betterRBCFixCompSlack`Private`nlPart[t]->nlt,
betterRBCFixCompSlack`Private`theta[t]->thetat,
betterRBCFixCompSlack`Private`cc[t+1]->cctp1,
betterRBCFixCompSlack`Private`II[t+1]->iitp1,
betterRBCFixCompSlack`Private`kk[t+1]->kktp1,
betterRBCFixCompSlack`Private`lam[t+1]->lamtp1,
betterRBCFixCompSlack`Private`mu1[t+1]->mu1tp1,
betterRBCFixCompSlack`Private`nlPart[t+1]->nltp1,
betterRBCFixCompSlack`Private`theta[t+1]->thetat
})//.
betterRBCFixCompSlack`Private`ssFRSolnSubs)//N
Print[eqnsForBind]
	
(*
dd->.1
{-1./cct + lamt, cct + iit - 1.*kktm1^0.36*thetat, nlt - 1.*lamt*thetat, 
 thetat - 1.*2.718281828459045^epsVal*thetatm1^0.95, 
 lamt - 0.855*lamtp1 + mu1t - 0.855*betterRBCFixCompSlack`Private`mu1tp1 - 
  (0.34199999999999997*nltp1)/kkt^0.64, iit - 1.*kkt + 0.9*kktm1, 
 -0.37263436422602103 + kkt - 0.9*kktm1}

*)

rbcEqnsBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha)))-lam[t+1]*delta*(1-dd)+mu1[t]-mu1[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
II[t] -theProduct
}



Print["here02"]

eqnsForNotBind=(((betterRBCFixCompSlack`Private`rbcEqnsNotBinding/.betterRBCFixCompSlack`Private`paramSubs)
/.{
eps[betterRBCFixCompSlack`Private`theta][t]->epsVal,
betterRBCFixCompSlack`Private`cc[t-1]->cctm1,
betterRBCFixCompSlack`Private`II[t-1]->iitm1,
betterRBCFixCompSlack`Private`kk[t-1]->kktm1,
betterRBCFixCompSlack`Private`lam[t-1]->lamtm1,
betterRBCFixCompSlack`Private`mu1[t-1]->mu1tm1,
betterRBCFixCompSlack`Private`nlPart[t-1]->nltm1,
betterRBCFixCompSlack`Private`theta[t-1]->thetatm1,
betterRBCFixCompSlack`Private`cc[t]->cct,
betterRBCFixCompSlack`Private`II[t]->iit,
betterRBCFixCompSlack`Private`kk[t]->kkt,
betterRBCFixCompSlack`Private`lam[t]->lamt,
betterRBCFixCompSlack`Private`mu1[t]->mu1t,
betterRBCFixCompSlack`Private`nlPart[t]->nlt,
betterRBCFixCompSlack`Private`theta[t]->thetat,
betterRBCFixCompSlack`Private`cc[t+1]->cctp1,
betterRBCFixCompSlack`Private`II[t+1]->iitp1,
betterRBCFixCompSlack`Private`kk[t+1]->kktp1,
betterRBCFixCompSlack`Private`lam[t+1]->lamtp1,
betterRBCFixCompSlack`Private`mu1[t+1]->mu1tp1,
betterRBCFixCompSlack`Private`nlPart[t+1]->nltp1,
betterRBCFixCompSlack`Private`theta[t+1]->thetat
})//.
betterRBCFixCompSlack`Private`ssFRSolnSubs)//N

(*

dd->.1
{-1./cct + lamt, cct + iit - 1.*kktm1^0.36*thetat, nlt - 1.*lamt*thetat, 
 thetat - 1.*2.718281828459045^epsVal*thetatm1^0.95, 
 lamt - 0.855*lamtp1 + mu1t - 0.855*mu1tp1 - 
  (0.34199999999999997*nltp1)/kkt^0.64, iit - 1.*kkt + 0.9*kktm1, mu1t}

*)


  rbcEqnsBetterFixCompSlack=eqnsCompiledBetterFixCompSlack={
 { {True&,
  Compile @@ {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},
  Function[{aPt,aRes},
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[2,1]]>(theProduct)]]]},
 {True&,
  Compile @@ {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},(True)&}},
Function[{aPt,allRes},
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Print["constraint violated"];Flatten[allRes[[2]]],Print["constraint not violated"];Flatten[allRes[[1]]]]]
}




Print["here03"]



theDistBetterFixCompSlack={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetterFixCompSlack={{{ee,PerfectForesight}}};










psiz=IdentityMatrix[7]

(*Print["RE solutions"]*)
hmatSymbRawRE=(((equationsToMatrix[
rbcEqnsNotBinding/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssFRSolnSubs)/.{eps[_]->0}//FullSimplify;

psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqnsNotBinding)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssFRSolnSubs}/.simpParamSubs]


Print["here04"]


hmatSymbRE=hmatSymbRawRE//.simpParamSubs
hSumRE=hmatSymbRE[[All,Range[7]]]+hmatSymbRE[[All,7+Range[7]]]+hmatSymbRE[[All,2*7+Range[7]]];





ssSolnVecRE={{cc},{II},{kk},{lam},{mu1},{nlPart},{theta}}//.ssFRSolnSubs;
psicSymbRE=hSumRE . ssSolnVecRE;



{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];



{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];


qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["here05"]


(*Print["computing and simplifying the symbolic b phi f etc"]*)
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;
Print["here06"]

(*
{rbcEqnsBetterFixCompSlack,rbcEqnsBetterBackLookingFixCompSlack,rbcEqnsBetterBackLookingExpFixCompSlack}=genFixCompSlackEqns @@ {alpha,beta,delta,rho,sigma,dd,upsilon}/.simpParamSubs
*)

linModBetterFixCompSlack={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,(*{{7,rbcEqnsBetterBackLookingFixCompSlack,rbcEqnsBetterBackLookingExpFixCompSlack}}*){}};Print["took out back looking equations"];

Print["here07"]



(*

*)





anXBetterFixCompSlack=Transpose[{{99,99,.18,99,99,99,1.01}}];
anEpsBetterFixCompSlack={{0.01}};
anXEpsBetterFixCompSlack=Join[anXBetterFixCompSlack,anEpsBetterFixCompSlack]
aZBetterFixCompSlack=Transpose[{{.1,.2,.3,.4,.5,.6,.7}}]
anXEpsZsBetterFixCompSlack=Join[anXEpsBetterFixCompSlack,aZBetterFixCompSlack];

anXFlatBetterFixCompSlack=anXBetterFixCompSlack//Flatten;
anEpsFlatBetterFixCompSlack=anEpsBetterFixCompSlack//Flatten;
anXEpsFlatBetterFixCompSlack=anXEpsBetterFixCompSlack//Flatten;
aZFlatBetterFixCompSlack=aZBetterFixCompSlack//Flatten;
anXEpsZsFlatBetterFixCompSlack=anXEpsZsBetterFixCompSlack//Flatten;

probDimsBetterFixCompSlack={7,1,7};



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
aGSpecBetterFixCompSlack={{1,2,4,5,6},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
	 aGSpecBetterFixCompSlack={{1,2,4,5,6},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};*)

	 aGSpecBetterFixCompSlack={{1,2,4,5,6},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};

(*
{alpha,beta,delta,rho,sigma,dd,upsilon}
*)



simulateBetterRBCCS[numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterFixCompSlack[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}],
fMul=Inverse[IdentityMatrix[7]-fmatSymbRE]},
With[{mats=FoldList[(bmatSymbRE . #1+ (phimatSymbRE .psiepsSymbRE .{{#2}})+
fMul.phimatSymbRE.psicSymbRE)&,initVec,draws]},
Flatten/@mats]]

simulateBetterRBCCS[anAugDR_Function,numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterFixCompSlack[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}]},
With[{mats=FoldList[((anAugDR @@ Flatten[{#1[[Range[7]]],#2}]))&,initVec,draws]},
Flatten/@mats]]

Print["here08"]

iterateRBCCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]:=
With[{mats=
NestList[((anAugDR @@ Flatten[{#1[[Range[7]]],#2}]))&,initVec,draws]},
Flatten/@mats]

chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]:=
Module[{},
If[
Catch[
Nest[With[{val=Apply[anAugDRCE,Flatten[#]][[Range[7]]]},
If[Norm[val]>lim,Throw[False,"chkBounded"],val]]&,aPt,numPers],
"chkBounded"]===False,False,True]]

Print["about to simulate fixed seed"];
SeedRandom[1234]
theRes=simulateBetterRBCCS[200];
Print["done simulate"];
justKT=theRes[[All,{3,7}]];
betterRBCCSMean=Mean[justKT];
betterRBCCSSD=StandardDeviation[justKT];

normedRes=(#/betterRBCCSSD)&/@((#-betterRBCCSMean)&/@justKT);
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
betterRBCCSMinZ=Min/@Transpose[zz];
betterRBCCSMaxZ=Max/@Transpose[zz];
{ig,ig,theKs,ig,ig,ig,theThetas}=Transpose[theRes];

betterRBCCSMean=Append[betterRBCCSMean,0];
betterRBCCSSD=Append[betterRBCCSSD,sigVal];
betterRBCCSMinZ=Append[betterRBCCSMinZ,-3];
betterRBCCSMaxZ=Append[betterRBCCSMaxZ,3];
betterRBCCSvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0},{0}}}}]},{{{0,0,1}}}}];


End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBCFixCompSlack.m"]


