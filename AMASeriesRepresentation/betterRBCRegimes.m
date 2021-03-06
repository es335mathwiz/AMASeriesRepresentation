

(* Wolfram Language Package *)

BeginPackage["betterRBCRegimes`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
rbcEqnsBetterCSTrips::usage="rbcEqnsBetterCSTrips"

anXBetterRegimes::usage="for test input";
anEpsBetterRegimes::usage="for test input";
anXEpsBetterRegimes::usage="for test input";
aZBetterRegimes::usage="for test input";
anXEpsZsBetterRegimes::usage="for test input";

anXFlatBetterRegimes::usage="for test input";
anEpsFlatBetterRegimes::usage="for test input";
anXEpsFlatBetterRegimes::usage="for test input";
aZFlatBetterRegimes::usage="for test input";
anXEpsZsFlatBetterRegimes::usage="for test input";

probDimsBetterRegimes::usage="for test input";
theDistBetterRegimes::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetterRegimes::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetterRegimes::usage="linear model matrices for approx"
aGSpecBetterRegimes::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsBetterBackLookingRegimes::usage="rbcEqnsBetterBackLookingRegimes"
rbcEqnsBetterBackLookingExpRegimes::usage="rbcEqnsBetterBackLookingRegimes"
rbcEqnsBetterRegimes::usage="model equations"
rbcEqnsBetterRegimes::usage="modEqnsRegimes"
eqnsEulerCompiledBetterRegimes::usage="eqnsEulerCompiledBetterRegimes"




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



(*parameters page 28 guerrieri iacoviello*)

paramSubs01={
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01,
dd->.1,
upsilon->0.975
} ;

paramSubs02={
alpha->.26,
beta->1,
eta->1,
delta->.85,
rho->.45,
sigma->.01,
dd->.1,
upsilon->0.275
} ;



forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs01;
simpParamSubs=Join[paramSubs01,forParamSubs,simpSubs];




rbcEqnsNotBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha))) -lam[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1])+mu1[t]-mu1[t+1]*delta*(1-dd),
mu1[t]
}


rbcBackLookingEqns={E^(rho*Log[theta[t-1]] + eps[theta][t])}
rbcBackLookingExpEqns={Expectation[rbcBackLookingEqns[[1]],eps[theta][t] \[Distributed] NormalDistribution[0,sigma]]}







ssEqnSubs=
{xx_Symbol[t+v_.]->xx}
rbcEqnsNotBindingSubbed=((rbcEqnsNotBinding/.paramSubs01)/.eps[theta][t]->0)


theVars=Cases[Variables[forFR=(rbcEqnsNotBindingSubbed/.ssEqnSubs)],_Symbol]



frArg=MapThread[Prepend[#1,#2]&,{{{.3599,2},{0,.35},{.187,.9},{-9.,9.},{-.01,0.1},{-9.,9.},{.9,1.1}},theVars}]




ssFRSolnSubs=Prepend[Chop[FindRoot[forFR,frArg,MaxIterations->1000]],IIss->0];


theProduct=upsilon*II//.ssFRSolnSubs/.betterRBCRegimes`Private`paramSubs01;




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




theArgs={cctm1,iitm1,kktm1,lamtm1,mutm1,nltm1,thetatm1,epsVal};

rbcEqnsBetterBackLookingRegimes=
Apply[Function , ({theArgs,rbcBackLookingEqns/.argsSubs}/.paramSubs01)]


rbcEqnsBetterBackLookingExpRegimes=
Apply[Function , ({Drop[theArgs,-1],rbcBackLookingExpEqns/.argsSubs}/.paramSubs01)]

preRbcEqnsBinding={
lam[t] +1/cc[t],
cc[t] + II[t]-((theta[t])*(kk[t-1]^alpha)),
nlPart[t] -((lam[t])*theta[t]),
theta[t]-(N[E]^(eps[theta][t]))*(theta[t-1]^rho) ,
(lam[t]) -(alpha*delta*nlPart[t+1]/(kk[t]^(1-alpha)))-lam[t+1]*delta*(1-dd)+mu1[t]-mu1[t+1]*delta*(1-dd),
II[t] -(kk[t]-(1-dd)*kk[t-1]),
mu1[t]
}




eqnsForBind01=(((betterRBCRegimes`Private`preRbcEqnsBinding/.betterRBCRegimes`Private`paramSubs01)/.{
eps[betterRBCRegimes`Private`theta][t]->epsVal,
betterRBCRegimes`Private`cc[t-1]->Global`cctm1,
betterRBCRegimes`Private`II[t-1]->iitm1,
betterRBCRegimes`Private`kk[t-1]->kktm1,
betterRBCRegimes`Private`lam[t-1]->lamtm1,
betterRBCRegimes`Private`mu1[t-1]->mu1tm1,
betterRBCRegimes`Private`nlPart[t-1]->nltm1,
betterRBCRegimes`Private`theta[t-1]->thetatm1,
betterRBCRegimes`Private`cc[t]->cct,
betterRBCRegimes`Private`II[t]->iit,
betterRBCRegimes`Private`kk[t]->kkt,
betterRBCRegimes`Private`lam[t]->lamt,
betterRBCRegimes`Private`mu1[t]->mu1t,
betterRBCRegimes`Private`nlPart[t]->nlt,
betterRBCRegimes`Private`theta[t]->thetat,
betterRBCRegimes`Private`cc[t+1]->cctp1,
betterRBCRegimes`Private`II[t+1]->iitp1,
betterRBCRegimes`Private`kk[t+1]->kktp1,
betterRBCRegimes`Private`lam[t+1]->lamtp1,
betterRBCRegimes`Private`mu1[t+1]->mu1tp1,
betterRBCRegimes`Private`nlPart[t+1]->nltp1,
betterRBCRegimes`Private`theta[t+1]->thetat
})//.
betterRBCRegimes`Private`ssFRSolnSubs)//N



eqnsForNotBind01=(((betterRBCRegimes`Private`rbcEqnsNotBinding/.betterRBCRegimes`Private`paramSubs01)
/.{
eps[betterRBCRegimes`Private`theta][t]->epsVal,
betterRBCRegimes`Private`cc[t-1]->cctm1,
betterRBCRegimes`Private`II[t-1]->iitm1,
betterRBCRegimes`Private`kk[t-1]->kktm1,
betterRBCRegimes`Private`lam[t-1]->lamtm1,
betterRBCRegimes`Private`mu1[t-1]->mu1tm1,
betterRBCRegimes`Private`nlPart[t-1]->nltm1,
betterRBCRegimes`Private`theta[t-1]->thetatm1,
betterRBCRegimes`Private`cc[t]->cct,
betterRBCRegimes`Private`II[t]->iit,
betterRBCRegimes`Private`kk[t]->kkt,
betterRBCRegimes`Private`lam[t]->lamt,
betterRBCRegimes`Private`mu1[t]->mu1t,
betterRBCRegimes`Private`nlPart[t]->nlt,
betterRBCRegimes`Private`theta[t]->thetat,
betterRBCRegimes`Private`cc[t+1]->cctp1,
betterRBCRegimes`Private`II[t+1]->iitp1,
betterRBCRegimes`Private`kk[t+1]->kktp1,
betterRBCRegimes`Private`lam[t+1]->lamtp1,
betterRBCRegimes`Private`mu1[t+1]->mu1tp1,
betterRBCRegimes`Private`nlPart[t+1]->nltp1,
betterRBCRegimes`Private`theta[t+1]->thetat
})//.
betterRBCRegimes`Private`ssFRSolnSubs)//N



eqnsForBind02=(((betterRBCRegimes`Private`preRbcEqnsBinding/.betterRBCRegimes`Private`paramSubs02)/.{
eps[betterRBCRegimes`Private`theta][t]->epsVal,
betterRBCRegimes`Private`cc[t-1]->Global`cctm1,
betterRBCRegimes`Private`II[t-1]->iitm1,
betterRBCRegimes`Private`kk[t-1]->kktm1,
betterRBCRegimes`Private`lam[t-1]->lamtm1,
betterRBCRegimes`Private`mu1[t-1]->mu1tm1,
betterRBCRegimes`Private`nlPart[t-1]->nltm1,
betterRBCRegimes`Private`theta[t-1]->thetatm1,
betterRBCRegimes`Private`cc[t]->cct,
betterRBCRegimes`Private`II[t]->iit,
betterRBCRegimes`Private`kk[t]->kkt,
betterRBCRegimes`Private`lam[t]->lamt,
betterRBCRegimes`Private`mu1[t]->mu1t,
betterRBCRegimes`Private`nlPart[t]->nlt,
betterRBCRegimes`Private`theta[t]->thetat,
betterRBCRegimes`Private`cc[t+1]->cctp1,
betterRBCRegimes`Private`II[t+1]->iitp1,
betterRBCRegimes`Private`kk[t+1]->kktp1,
betterRBCRegimes`Private`lam[t+1]->lamtp1,
betterRBCRegimes`Private`mu1[t+1]->mu1tp1,
betterRBCRegimes`Private`nlPart[t+1]->nltp1,
betterRBCRegimes`Private`theta[t+1]->thetat
})//.
betterRBCRegimes`Private`ssFRSolnSubs)//N



eqnsForNotBind02=(((betterRBCRegimes`Private`rbcEqnsNotBinding/.betterRBCRegimes`Private`paramSubs02)
/.{
eps[betterRBCRegimes`Private`theta][t]->epsVal,
betterRBCRegimes`Private`cc[t-1]->cctm1,
betterRBCRegimes`Private`II[t-1]->iitm1,
betterRBCRegimes`Private`kk[t-1]->kktm1,
betterRBCRegimes`Private`lam[t-1]->lamtm1,
betterRBCRegimes`Private`mu1[t-1]->mu1tm1,
betterRBCRegimes`Private`nlPart[t-1]->nltm1,
betterRBCRegimes`Private`theta[t-1]->thetatm1,
betterRBCRegimes`Private`cc[t]->cct,
betterRBCRegimes`Private`II[t]->iit,
betterRBCRegimes`Private`kk[t]->kkt,
betterRBCRegimes`Private`lam[t]->lamt,
betterRBCRegimes`Private`mu1[t]->mu1t,
betterRBCRegimes`Private`nlPart[t]->nlt,
betterRBCRegimes`Private`theta[t]->thetat,
betterRBCRegimes`Private`cc[t+1]->cctp1,
betterRBCRegimes`Private`II[t+1]->iitp1,
betterRBCRegimes`Private`kk[t+1]->kktp1,
betterRBCRegimes`Private`lam[t+1]->lamtp1,
betterRBCRegimes`Private`mu1[t+1]->mu1tp1,
betterRBCRegimes`Private`nlPart[t+1]->nltp1,
betterRBCRegimes`Private`theta[t+1]->thetat
})//.
betterRBCRegimes`Private`ssFRSolnSubs)//N



  rbcEqnsBetterRegime01={
 { 
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind01),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],
Function[{aPt,aRes},
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[2,1]]>(theProduct)]]]},
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForBind01),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],(True)&}},
Function[{aPt,allRes},
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Flatten[allRes[[2]]],Flatten[allRes[[1]]]]]
}

  rbcEqnsBetterRegime02={
 { 
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind02),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],
Function[{aPt,aRes},
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[2,1]]>(theProduct)]]]},
{True&,
Apply[Compile , {
{
{cctm1,_Real},{iitm1,_Real},{kktm1,_Real},{lamtm1,_Real},{mu1tm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{iit,_Real},{kkt,_Real},{lamt,_Real},{mu1t,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{iitp1,_Real},{kktp1,_Real},{lamtp1,_Real},{mu1tp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
(eqnsForBind02),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}],(True)&}},
Function[{aPt,allRes},
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Flatten[allRes[[2]]],Flatten[allRes[[1]]]]]
}
(*https://en.wikipedia.org/wiki/Sigmoid_function*)
sigmoidPair[xx_,bb_]:=  With[{pp=E^(bb*xx)/(E^(bb*xx)+1)},{pp,1-pp}]
(*
probFunc[cct_?NumberQ,iit_?NumberQ,kkt_?NumberQ,lamt_?NumberQ,
mu1t_?NumberQ,nlt_?NumberQ,thetat_?NumberQ]:=
With[{pk=sigmoidPair[kkt,1.],pc=sigmoidPair[cct,2.]},
{pk,pc}]
*)
probFunc[cct_?NumberQ,iit_?NumberQ,kkt_?NumberQ,lamt_?NumberQ,
mu1t_?NumberQ,nlt_?NumberQ,thetat_?NumberQ]:=
{{0.9,0.1},{0.2,0.8}}



rbcEqnsBetterRegimes={{rbcEqnsBetterRegime01,
 rbcEqnsBetterRegime02},probFunc}






theDistBetterRegimes={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs01;
thePFDistBetterRegimes={{{ee,PerfectForesight}}};




psiz=IdentityMatrix[7]

hmatSymbRawRE=(((equationsToMatrix[
rbcEqnsNotBinding/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssFRSolnSubs)/.{eps[_]->0}//FullSimplify;

psiepsSymbRE=-Transpose[{((Map[D[#,eps[theta][t]]&, rbcEqnsNotBinding])/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssFRSolnSubs}/.simpParamSubs]


hmatSymbRE=hmatSymbRawRE//.simpParamSubs
hSumRE=hmatSymbRE[[All,Range[7]]]+hmatSymbRE[[All,7+Range[7]]]+hmatSymbRE[[All,2*7+Range[7]]];


ssSolnVecRE={{cc},{II},{kk},{lam},{mu1},{nlPart},{theta}}//.ssFRSolnSubs;
psicSymbRE=hSumRE . ssSolnVecRE;




{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];



{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];


qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];


{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModBetterRegimes={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{4,rbcEqnsBetterBackLookingRegimes,rbcEqnsBetterBackLookingExpRegimes}}}




anXBetterRegimes=Transpose[{{99,99,.18,99,99,99,1.01}}];
anEpsBetterRegimes={{0.01}};
anXEpsBetterRegimes=Join[anXBetterRegimes,anEpsBetterRegimes]
aZBetterRegimes=Transpose[{{.1,.2,.3,.4,.5,.6,.7}}]
anXEpsZsBetterRegimes=Join[anXEpsBetterRegimes,aZBetterRegimes];

anXFlatBetterRegimes=anXBetterRegimes//Flatten;
anEpsFlatBetterRegimes=anEpsBetterRegimes//Flatten;
anXEpsFlatBetterRegimes=anXEpsBetterRegimes//Flatten;
aZFlatBetterRegimes=aZBetterRegimes//Flatten;
anXEpsZsFlatBetterRegimes=anXEpsZsBetterRegimes//Flatten;

probDimsBetterRegimes={7,1,7};





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


aGSpecBetterRegimes={{1,2,4,5,6},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};




simulateBetterRBCCS[numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterRegimes[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}],
fMul=Inverse[IdentityMatrix[7]-fmatSymbRE]},
With[{mats=FoldList[(bmatSymbRE . #1+ (phimatSymbRE .psiepsSymbRE .{{#2}})+
fMul.phimatSymbRE.psicSymbRE)&,initVec,draws]},
Map[Flatten,mats]]]

simulateBetterRBCCS[anAugDR_Function,numPers_Integer]:=
With[{draws=RandomVariate[theDistBetterRegimes[[1,1,2]],numPers],
initVec=Transpose[{{99,99,kVal,99,99,99,thVal}}]},
With[{mats=FoldList[((Apply[anAugDR , Flatten[{#1[[Range[7]]],#2}]]))&,initVec,draws]},Map[Flatten,mats]]]





iterateRBCCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]:=
With[{mats=
NestList[((Apply[anAugDR , Flatten[{#1[[Range[7]]],#2}]]))&,initVec,draws]},
Map[Flatten,mats]]

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




normedRes=Map[(#/betterRBCCSSD)&,(Map[(#-betterRBCCSMean)&,justKT])];
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
betterRBCCSMinZ=Map[Min,Transpose[zz]];
betterRBCCSMaxZ=Map[Max,Transpose[zz]];
{ig,ig,theKs,ig,ig,ig,theThetas}=Transpose[theRes];

betterRBCCSMean=Append[betterRBCCSMean,0];
betterRBCCSSD=Append[betterRBCCSSD,sigVal];
betterRBCCSMinZ=Append[betterRBCCSMinZ,-3];
betterRBCCSMaxZ=Append[betterRBCCSMaxZ,3];
betterRBCCSvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0},{0}}}}]},{{{0,0,1}}}}];


End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBCRegimes.m"]



