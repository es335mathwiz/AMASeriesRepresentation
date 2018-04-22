
(* Wolfram Language Package *)
Print["start reading betterRBC.m"]
BeginPackage["betterRBC`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
anXBetter::usage="for test input";
anEpsBetter::usage="for test input";
anXEpsBetter::usage="for test input";
aZBetter::usage="for test input";
anXEpsZsBetter::usage="for test input";

anXFlatBetter::usage="for test input";
anEpsFlatBetter::usage="for test input";
anXEpsFlatBetter::usage="for test input";
aZFlatBetter::usage="for test input";
anXEpsZsFlatBetter::usage="for test input";

probDimsBetter::usage="for test input";
simpRBCExactX0Z0CEBetter::usage="simpRBCExactX0Z0CEBetter"
simpRBCExactZCEBetter::usage="simpRBCExactZCEBetter"
simpRBCExactZBetter::usage="simpRBCExactZBetter"
simpRBCExactX0Z0Better::usage = "simpRBCExactX0Z0Better"
simpRBCExactDRCEBetter::usage="simpRBCExactDRCEBetter";
simpRBCExactDRBetter::usage="simpRBCExactDR"
betterRBCExactCondExp::usage="betterRBCExactCondExp"
theDistBetter::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistBetter::usage="theDist={{{ee,PerfectForesight]}}};"
linModBetter::usage="linear model matrices for approx"
aGSpecBetter::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
eqnsCompiledBetter::usage="model equations function"
rbcEqnsBetter::usage="model equations"
eqnsEulerCompiledBetter::usage="eqnsEulerCompiledBetter"
betterExactXZ::usage="betterExactXZ"
betterExactZ::usage="betterExactZ"
simulateBetterRBCExact::usage="simulateBetterRBCExact[numPers_Integer]"
betterRBCMean::usage="betterRBCMean"
betterRBCSD::usage="betterRBCSD"
betterRBCvv::usage="betterRBCvv"
betterRBCMinZ::usage="betterRBCMinZ"
betterRBCMaxZ::usage="betterRBCMaxZ"
Print["at Private"]
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

discMapEqns00=(Append[rbcEqns[[{1,2,3}]],
(rbcEqns[[4]]/.{xx_-yy_->Log[xx]-Log[yy]})]/.{Log[betterRBC`Private`theta[zz__]]->lnTheta[zz],theta[xx__]->E^lnTheta[xx]})//PowerExpand

zfEqns=discMapEqns00[[{2,3,4}]]//PowerExpand
discMapEqns01=Append[zfEqns/.t->t+1,discMapEqns00[[1]]]//PowerExpand
(*soln=Solve[Thread[discMapEqns01==0],{cc[t+1],kk[t+1],nlPart[t+1],lnTheta[t+1]}]*)

(*parameters page 21 using state 1*)
(*
paramSubs=Rationalize[{
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01
  } ];
*)
paramSubs={
alpha->.36,
beta->1,
eta->1,
delta->.95,
rho->.95,
sigma->.01
  };

forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];
(*

rbcCompileGuts=(betterRBC`Private`rbcEqns/.{
betterRBC`Private`cc[t-1]->cctm1,
betterRBC`Private`kk[t-1]->kktm1,
betterRBC`Private`nlPart[t-1]->nltm1,
betterRBC`Private`theta[t-1]->thtm1,
betterRBC`Private`cc[t]->cct,
betterRBC`Private`kk[t]->kkt,
betterRBC`Private`nlPart[t]->nlt,
betterRBC`Private`theta[t]->tht,
betterRBC`Private`cc[t+1]->cctp1,
betterRBC`Private`kk[t+1]->kktp1,
betterRBC`Private`nlPart[t+1]->nltp1,
betterRBC`Private`theta[t+1]->thtp1,
eps[betterRBC`Private`theta][t]->epsVal
}//.betterRBC`Private`paramSubs)//N//InputForm
{cct^(-1) - (0.34199999999999997*nltp1)/kkt^0.64, 
 cct + kkt - 1.*kktm1^0.36*tht, nlt - (1.*tht)/cct, 
 tht - 1.*2.718281828459045^epsVal*thtm1^0.95}

*)

rbcEqnsBetter=eqnsCompiledBetter=Apply[Compile, {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
({cct^(-1) - (alpha*delta*nltp1)/kkt^(1-alpha),
cct + kkt - 1.*kktm1^(alpha)*thetat, 
nlt - thetat/cct,
thetat - ((N[E]^epsVal)*(thetatm1^(rho)))}/.paramSubs),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}]

rbcEqnsBetterSymb[
cctm1_,kktm1_,nltm1_,thetatm1_,
cct_,kkt_,nlt_,thetat_,
cctp1_,kktp1_,nltp1_,thetatp1_,
epsVal_]:=
({cct^(-1) - (alpha*delta*nltp1)/kkt^(1-alpha),
cct + kkt - 1.*kktm1^(alpha)*thetat, 
nlt - thetat/cct,
thetat - ((N[E]^epsVal)*(thetatm1^(rho)))}/.paramSubs)

(*
causes error a
CompiledFunction::cfn: 
   Numerical error encountered at instruction 2; proceeding with
     uncompiled evaluation.




Apply[eqnsCompiledBetter  , Flatten[{{1}, {0.0187324}, {1}, {1.1}, {0.293437}, {-0.0351748},      {7.51431}, {1.08125}, {0.232894}, {0.120986}, {3.96721}, 
     {1.07709}, {-0.0124264}}]]

*)
(*
eqnsEulerCompiledBetter=Apply[Compile , {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{((kkt^(16/25)) - (0.342*nltp1)*cct)/cct,
cct + kkt - 1.*kktm1^(9/25)*thetat, 
nlt - thetat/cct,
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)},"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}]
*)

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
nlPartSSSubRE=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.Join[thSubsRE,Append[kSSSubRE,cSSSubRE]];
ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE,nlPartSSSubRE}];
(*Print["RE done now PF"];*)
thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]];
kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]];
On[Solve::ifun]
cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF);
nlPartSSSubPF=(nlPart->(nlPartRHS/.xxxx_[t]->xxxx))//.{kSSSubPF,cSSSubPF,thSubsPF};
ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF,nlPartSSSubPF}];




simpRBCExactDRBetter = 
 Function[{cc, kk, nl, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht/cct,tht}}]]]]]

thExp=Expectation[(tht^rho)*E^eps,eps \[Distributed] NormalDistribution[0,sigma]]
kkExp=Expectation[(((tht^rho)*E^eps)*alpha*delta*kkt^alpha),eps \[Distributed] NormalDistribution[0,sigma]]

ccExp=Expectation[(((((tht^rho)*E^eps)*kkt^alpha)*(1-alpha*delta))),eps \[Distributed] NormalDistribution[0,sigma]]

nnExp=Expectation[((tht^rho)*E^eps)/((((((tht^rho)*E^eps)*kkt^alpha)*(1-alpha*delta)))),eps \[Distributed] NormalDistribution[0,sigma]]
 
simpRBCExactDRCEBetter = 
Apply[  Function , {{cct, kkt, nlt, tht}, Flatten[
               {ccExp,kkExp,nnExp,thExp}//.paramSubs]}]

makeExactArgs[kk_,tt_,ee_]:=
    With[{xt = Flatten[simpRBCExactDRBetter[ig, kk, ig, tt, ee]]}, 
     With[{xtp1 = Flatten[Apply[simpRBCExactDRCEBetter , xt]]}, 
      Append[Join[{999, kk, 999, tt}, xt, xtp1], ee]]]




(*Print["RE solutions"]*)
hmatSymbSlowRawRE00=(((equationsToMatrix[
rbcEqns]//FullSimplify)));
hmatSymbSlowRawRE01=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)))//FullSimplify;
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((Map[D[#,eps[theta][t]]&, rbcEqns])/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[4]]]+hmatSymbRE[[All,4+Range[4]]]+hmatSymbRE[[All,8+Range[4]]];

ssSolnVecRE={{cc},{kk},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

(*Print["computing and simplifying the symbolic b phi f etc"]*)
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

psiz=IdentityMatrix[4]

linModBetter={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{}};

(*linModBetter=Map[Rationalize[#,1/100000000]&,linModBetter,{-1}]*)




simpRBCExactZBetter = 
Apply[ Function , {{cct, kkt, nlt, tht,ee},
   With[{args=makeExactArgs[kkt,tht,ee],
     pc=getPsiC[linModBetter],pe=getPsiEps[linModBetter]},
With[{zt=Flatten[
(getH[linModBetter] . Transpose[{ Drop[Flatten[args],-1]}])-pc-pe*ee]},zt]]}]

simpRBCExactZCEBetter = 
Apply[ Function, {{cct, kkt, nlt, tht},
   With[{args=makeExactArgs[kkt,tht,ee],
     pc=getPsiC[linModBetter],pe=getPsiEps[linModBetter]},
With[{zt=Flatten[
(getH[linModBetter] . Transpose[{ Drop[Flatten[args],-1]}])-pc-pe*ee]},
  Expectation[zt,ee \[Distributed]NormalDistribution[0,sigma//.paramSubs]]]]}]

simpRBCExactX0Z0CEBetter = 
Apply[ Function , {{cct, kkt, nlt, tht},
              Transpose[{Flatten[
         Join[simpRBCExactDRCEBetter[cct,kkt,nlt,tht],
              simpRBCExactZCEBetter[cct,kkt,nlt,tht]]]}]}]



theDistBetter={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistBetter={{{ee,PerfectForesight}}};



simulateBetterRBCExact[numPers_Integer]:=
With[{draws=RandomVariate[theDistBetter[[1,1,2]],numPers],
initVec={99,betterRBC`Private`kVal,99,betterRBC`Private`thVal}},
FoldList[Flatten[Apply[simpRBCExactDRBetter,Append[Flatten[#1],#2]]]&,initVec,draws]]


betterRBCExactCondExp = (*AMASeriesRepresentation`Private`*)makeREIterFunc[simpRBCExactDRBetter,theDistBetter]


betterExactZ=
Function[{cc, kk, nl, th, eps},
With[{hm=getH[linModBetter],pc=getPsiC[linModBetter],pe=getPsiEps[linModBetter],
xt=Flatten[simpRBCExactDRBetter[cc,kk,nl,th,eps]]},
With[{xtp1=Flatten[Apply[betterRBCExactCondExp, xt]]},
hm.Transpose[{Join[{cc,kk,nl,th},xt,xtp1]}]-pc-pe*eps]]]

betterExactXZ=
Function[{cc, kk, nl, th, eps},
With[{xval=simpRBCExactDRBetter[cc,kk,nl,th,eps],
zval=betterExactZ[cc,kk,nl,th,eps]},
Join[xval,zval]]]



    
anXBetter=Transpose[{{.2,.18,1.0,1.1}}];
anEpsBetter={{0.01}};
anXEpsBetter=Join[anXBetter,anEpsBetter]
aZBetter=Transpose[{{.1,.2,.3,.4}}]
anXEpsZsBetter=Join[anXEpsBetter,aZBetter];

anXFlatBetter=anXBetter//Flatten;
anEpsFlatBetter=anEpsBetter//Flatten;
anXEpsFlatBetter=anXEpsBetter//Flatten;
aZFlatBetter=aZBetter//Flatten;
anXEpsZsFlatBetter=anXEpsZsBetter//Flatten;

probDimsBetter={4,1,4};


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk //.kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc //.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;
kHigh = 1.2*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;

        (*
aGSpecBetter={{1,3},2,{{6,kLow,kHigh},{10,thLow,thHigh},{6,sigLow,3*sigHigh}}};
         aGSpecBetter={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};*)

         aGSpecBetter={{1,3},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,sigHigh}}};
Print["about to simulate fixed seed"];
SeedRandom[1234]
theRes=simulateBetterRBCExact[200];
justKT=theRes[[All,{2,4}]];
betterRBCMean=Mean[justKT]
betterRBCSD=StandardDeviation[justKT]
normedRes=Map[(#/betterRBCSD)&,(Map[(#-betterRBCMean)&,justKT])]
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
betterRBCMinZ=Map[Min,Transpose[zz]];
betterRBCMaxZ=Map[Max,Transpose[zz]];
{ig,theKs,ig,theThetas}=Transpose[theRes];

betterRBCMean=Append[betterRBCMean,0]
betterRBCSD=Append[betterRBCSD,sigVal]
betterRBCMinZ=Append[betterRBCMinZ,-3]
betterRBCMaxZ=Append[betterRBCMaxZ,3]
betterRBCvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0},{0}}}}]},{{{0,0,1}}}}]
(*
Print["at first export"]
Export["ergodicV.pdf", MatrixForm[betterRBCvv//N]];
Print["at second export"]
Export["ergodicMaxZ.pdf", MatrixForm[betterRBCMaxZ//N]];
Print["at next export"]
Export["ergodicMinZ.pdf", MatrixForm[betterRBCMinZ//N]];
Print["at next export"]
Export["ergodicMean.pdf", MatrixForm[betterRBCMean//N]];
Print["at next export"]
Export["ergodicSD.pdf", MatrixForm[betterRBCSD//N]];
Print["at next export"]
Export["ergodicKTheta.pdf",ListPlot[Transpose[{theKs,theThetas}],PlotLabel->"Ergodic Values for K and \[Theta]"]];
Print["at next export"]
zPts=backXtoZ[Transpose[{theKs,theThetas,Table[0,{Length[theKs]}]}],betterRBCMean,betterRBCSD,betterRBCvv];Print["errBndLoc=",errBndLoc];
        Export["ergodicZs.pdf",ListPlot[zPts[[All,{1,2}]],PlotLabel->"Ergodic Values for K and \[Theta]"]];
Print["after last export"]
*)
End[] (* End Private Context *)

EndPackage[]
Print["done reading betterRBC.m"]


