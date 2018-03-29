(* Wolfram Language Package *)

BeginPackage["nkZLB`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  

anXnkZLB::usage="for test input";
anEpsnkZLB::usage="for test input";
anXEpsnkZLB::usage="for test input";
aZnkZLB::usage="for test input";
anXEpsZsnkZLB::usage="for test input";

anXFlatnkZLB::usage="for test input";
anEpsFlatnkZLB::usage="for test input";
anXEpsFlatnkZLB::usage="for test input";
aZFlatnkZLB::usage="for test input";
anXEpsZsFlatnkZLB::usage="for test input";

probDimsnkZLB::usage="for test input";
theDistnkZLB::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistnkZLB::usage="theDist={{{ee,PerfectForesight]}}};"
linModnkZLB::usage="linear model matrices for approx"
aGSpecnkZLB::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
nkEqnsNkBackLookingFixCompSlack::usage="nkEqnsNkBackLookingFixCompSlack"
nkEqnsNkBackLookingExpFixCompSlack::usage="nkEqnsNkBackLookingFixCompSlack"
nkEqnsnkZLB::usage="model equations"
nkEqnsnkZLBNot::usage="model equations"
eqnsEulerCompilednkZLB::usage="eqnsEulerCompilednkZLB"

simulateNkNKCS::usage="simulateNkNKExact[numPers_Integer]"
nkNKCSMean::usage="nkNKCSMean"
nkNKCSSD::usage="nkNKCSSD"
nkNKCSvv::usage="nkNKCSvv"
nkNKCSMinZ::usage="nkNKCSMinZ"
nkNKCSMaxZ::usage="nkNKCSMaxZ"

chkBounded::usage="chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]"


iterateNKCSDRCE::usage="iterateNKCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]"


Begin["`Private`"] (* Begin Private Context *) 











(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)



(*parameters page 28 guerrieri iacoviello*)

paramSubs={
beta->0.99,
phiP->100.,
piBar->1.005,
epsi->6.,
gammaPi->2.,
rho->.95,
sigma->.5/100,
theBound->1
} ;



forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=paramSubs;

(*
(ct/ctp1)*(pitp1/piBar -1)*(pitp1*ytp1)/(piBar*yt)//ExpandAll

-((ct*pitp1*ytp1)/(ctp1*piBar*yt)) + (ct*pitp1^2*ytp1)/(ctp1*piBar^2*yt)
*)

nkEqnsCommon={
1/CC[t] - ((beta*RR[t]/eta[t])*nl1[t+1]),
nl1[t]-(1/(CC[t]*pi[t])),
Log[eta[t]]-(rho*Log[eta[t-1]] +eps[eta][t]),
phiP*(pi[t]/piBar -1)*pi[t]/piBar -
(
(beta*phiP/eta[t])*(
(CC[t]/(piBar*piBar*YY[t])*nl2[t+1])-(CC[t]/(piBar*YY[t])*nl3[t+1])
)+(epsi-1)*((epsi/(epsi-1))*CC[t]-1)
),
nl2[t]-((pi[t]*pi[t]*YY[t])/CC[t]),
nl3[t]-(pi[t]*YY[t]/CC[t]),
YY[t]-(CC[t]+(phiP/2)*(((pi[t]/piBar)-1)^2)*YY[t])
}

nkEqnsNotBinding=Append[nkEqnsCommon,
RR[t]-((piBar/beta)*((pi[t]/piBar)^gammaPi))]

nkEqnsBinding=Append[nkEqnsCommon,RR[t]-theBound]/.paramSubs



nkBackLookingEqns={(rho*Log[eta[t-1]] +eps[eta][t])}
nkBackLookingExpEqns={Expectation[nkBackLookingEqns[[1]],eps[eta][t] \[Distributed] NormalDistribution[0,sigma]]}







ssEqnSubs=
{xx_Symbol[t+v_.]->xx}
nkEqnsNotBindingSubbed=(((nkEqnsNotBinding/.ssEqnSubs)/.paramSubs)/.eps[eta][t]->0)



nkEqnsBindingSubbed=(((nkEqnsBinding/.ssEqnSubs)/.paramSubs)/.eps[eta][t]->0)



theVars=Cases[Variables[forFR=(nkEqnsNotBindingSubbed/.ssEqnSubs)],_Symbol]

forFRBind=(nkEqnsBindingSubbed/.ssEqnSubs)

(*
{CC, eta, nl1, nl2, nl3, pi, RR, YY}
*)
startVals={{.1,2},{.99,1.01},{0,11},{0.1,9.},{.01,11},{1.004,1.006},{0.8,2},{.1,10}}


frArg=MapThread[Prepend[#1,#2]&,
{startVals,theVars}]


(*
ssFRSolnSubs=Chop[FindRoot[forFR,frArg,MaxIterations->1000]];
*)
ssFRSolnSubs=Flatten[Chop[NSolve[forFR,theVars(*,MaxIterations->1000*)]]];
ssFRBindSolnSubs=Flatten[Chop[NSolve[forFRBind,theVars(*,MaxIterations->1000*)]]];

(*
{nkZLB`Private`CC -> 0.8333333333333334, nkZLB`Private`eta -> 1., 
 nkZLB`Private`nl1 -> 1.1940298507462686, nkZLB`Private`nl2 -> 1.010025, 
 nkZLB`Private`nl3 -> 1.005, nkZLB`Private`pi -> 1.005, 
 nkZLB`Private`RR -> 1.0151515151515151, 
 nkZLB`Private`YY -> 0.8333333333333334}
*)
{nkZLB`Private`CC -> 0.9333333333333334, nkZLB`Private`eta -> 1., 
 nkZLB`Private`nl1 -> 1.1940298507462686, nkZLB`Private`nl2 -> 1.010025, 
 nkZLB`Private`nl3 -> 1.005, nkZLB`Private`pi -> 1.005, 
 nkZLB`Private`RR -> 1.0151515151515151, 
 nkZLB`Private`YY -> 0.8333333333333334}




psiz=IdentityMatrix[8]

hmatSymbRawRE=(((curious=equationsToMatrix[
nkEqnsNotBinding/.simpParamSubs/.eps[eta][t]->0]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssFRSolnSubs)//FullSimplify;



psiepsSymbRE=-Transpose[{((D[#,eps[eta][t]]&/@ nkEqnsNotBinding)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssFRSolnSubs}/.simpParamSubs]



hmatSymbRE=hmatSymbRawRE//.simpParamSubs
hSumRE=hmatSymbRE[[All,Range[8]]]+hmatSymbRE[[All,8+Range[8]]]+hmatSymbRE[[All,2*8+Range[8]]];



ssSolnVecRE=Transpose[{theVars}]//.ssFRSolnSubs;
psicSymbRE=hSumRE . ssSolnVecRE;



{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];




{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];




qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1,2}]]];



{bmatSymbRE,phimatSymbRE,fmatSymbRE}=Chop[symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify];

linModnkZLB=Chop[{hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{(*{4,nkEqnsNkBackLookingFixCompSlack,nkEqnsNkBackLookingExpFixCompSlack}*)}}]
Print["took out backlooking"]







argsSubs={
CC[t-1]->CCtm1,
eta[t-1]->etatm1, 
nl1[t-1]->nl1tm1, 
nl2[t-1]->nl2tm1, 
nl3[t-1]->nl3tm1, 
pi[t-1]->pitm1, 
RR[t-1]->RRtm1, 
YY[t-1]->YYtm1,
CC[t]->CCt,
eta[t]->etat, 
nl1[t]->nl1t, 
nl2[t]->nl2t, 
nl3[t]->nl3t, 
pi[t]->pit, 
RR[t]->RRt, 
YY[t]->YYt,
CC[t+1]->CCtp1,
eta[t+1]->etatp1, 
nl1[t+1]->nl1tp1, 
nl2[t+1]->nl2tp1, 
nl3[t+1]->nl3tp1, 
pi[t+1]->pitp1, 
RR[t+1]->RRtp1, 
YY[t+1]->YYtp1,
eps[eta][t]->epsVal
}



theArgs={CCtm1,etatm1,nl1tm1,nl2tm1,nl3tm1,pitm1,RRtm1,YYtm1,epsVal};



nkEqnsNkBackLookingFixCompSlack=
Function @@ ({theArgs,nkBackLookingEqns/.argsSubs}/.paramSubs)


nkEqnsNkBackLookingExpFixCompSlack=
Function @@ ({Drop[theArgs,-1],nkBackLookingExpEqns/.argsSubs}/.paramSubs)



eqnsForBind=((nkEqnsBinding/.nkZLB`Private`paramSubs)/.argsSubs)



eqnsForNotBind=((nkZLB`Private`nkEqnsNotBinding/.nkZLB`Private`paramSubs)/.argsSubs)


  nkEqnsnkZLB={
 { 
{True&,
Compile @@ {
{
{CCtm1,_Real},{etatm1,_Real},{nl1tm1,_Real},{nl2tm1,_Real},{nl3tm1,_Real},{pitm1,_Real},{RRtm1,_Real},{YYtm1,_Real},
{CCt,_Real},{etat,_Real},{nl1t,_Real},{nl2t,_Real},{nl3t,_Real},{pit,_Real},{RRt,_Real},{YYt,_Real},
{CCtp1,_Real},{etatp1,_Real},{nl1tp1,_Real},{nl2tp1,_Real},{nl3tp1,_Real},{pitp1,_Real},{RRtp1,_Real},{YYtp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},
Function[{aPt,aRes},
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[7,1]]>=1]]]},
{(True)&,
Compile @@ {
{
{CCtm1,_Real},{etatm1,_Real},{nl1tm1,_Real},{nl2tm1,_Real},{nl3tm1,_Real},{pitm1,_Real},{RRtm1,_Real},{YYtm1,_Real},
{CCt,_Real},{etat,_Real},{nl1t,_Real},{nl2t,_Real},{nl3t,_Real},{pit,_Real},{RRt,_Real},{YYt,_Real},
{CCtp1,_Real},{etatp1,_Real},{nl1tp1,_Real},{nl2tp1,_Real},{nl3tp1,_Real},{pitp1,_Real},{RRtp1,_Real},{YYtp1,_Real},
{epsVal,_Real}
},
(eqnsForBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},(True)&}},
Function[{aPt,allRes},Print["nkZLB:",{aPt,allRes}];
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Flatten[allRes[[2]]],
If[True(*allRes[[1,1,1]]>=allRes[[2,1,1]]*),Flatten[allRes[[1]]],Flatten[allRes[[2]]]]]]
}

nkEqnsnkZLBNot={
 { 
{True&,
Compile @@ {
{
{CCtm1,_Real},{etatm1,_Real},{nl1tm1,_Real},{nl2tm1,_Real},{nl3tm1,_Real},{pitm1,_Real},{RRtm1,_Real},{YYtm1,_Real},
{CCt,_Real},{etat,_Real},{nl1t,_Real},{nl2t,_Real},{nl3t,_Real},{pit,_Real},{RRt,_Real},{YYt,_Real},
{CCtp1,_Real},{etatp1,_Real},{nl1tp1,_Real},{nl2tp1,_Real},{nl3tp1,_Real},{pitp1,_Real},{RRtp1,_Real},{YYtp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},
Function[{aPt,aRes},
If[aRes===$Failed,False,And[aRes[[1,1]]>0,aRes[[7,1]]>=1]]]},
{(True)&,
Compile @@ {
{
{CCtm1,_Real},{etatm1,_Real},{nl1tm1,_Real},{nl2tm1,_Real},{nl3tm1,_Real},{pitm1,_Real},{RRtm1,_Real},{YYtm1,_Real},
{CCt,_Real},{etat,_Real},{nl1t,_Real},{nl2t,_Real},{nl3t,_Real},{pit,_Real},{RRt,_Real},{YYt,_Real},
{CCtp1,_Real},{etatp1,_Real},{nl1tp1,_Real},{nl2tp1,_Real},{nl3tp1,_Real},{pitp1,_Real},{RRtp1,_Real},{YYtp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},(True)&}},
Function[{aPt,allRes},Print["nkZLB:",{aPt,allRes}];
If[And[allRes[[1]]===$Failed,allRes[[2]]===$Failed],Throw[$Failed,"noSolutionFound"]];
If[allRes[[1]]===$Failed,Flatten[allRes[[2]]],
If[True(*allRes[[1,1,1]]>=allRes[[2,1,1]]*),Flatten[allRes[[1]]],Flatten[allRes[[2]]]]]]
}


theDistnkZLB={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistnkZLB={{{ee,PerfectForesight}}};




anXnkZLB=Transpose[{{99,1.02,99,99,99,99,99,99}}];
anEpsnkZLB={{0.01}};
anXEpsnkZLB=Join[anXnkZLB,anEpsnkZLB]
aZnkZLB=Transpose[{{.1,.2,.3,.4,.5,.6,.7,.8}}]
anXEpsZsnkZLB=Join[anXEpsnkZLB,aZnkZLB];

anXFlatnkZLB=anXnkZLB//Flatten;
anEpsFlatnkZLB=anEpsnkZLB//Flatten;
anXEpsFlatnkZLB=anXEpsnkZLB//Flatten;
aZFlatnkZLB=aZnkZLB//Flatten;
anXEpsZsFlatnkZLB=anXEpsZsnkZLB//Flatten;

probDimsnkZLB={8,1,8};


etaVal=(eta//.ssFRSolnSubs//.(simpParamSubs//N))//N;
etaLow = .9;
etaHigh = 1.1;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;


aGSpecnkZLB={{1,3,4,5,6,7,8},1,{{4,etaLow,etaHigh},{3,sigLow,3*sigHigh}}};




simulateNkNKCS[numPers_Integer]:=
With[{draws=RandomVariate[theDistnkZLB[[1,1,2]],numPers],
initVec=Transpose[{{99,etaVal,99,99,99,99,99,99}}],
fMul=Inverse[IdentityMatrix[8]-fmatSymbRE]},
With[{mats=FoldList[(bmatSymbRE . #1+ (phimatSymbRE .psiepsSymbRE .{{#2}})+
fMul.phimatSymbRE.psicSymbRE)&,initVec,draws]},
Flatten/@mats]]

simulateNkNKCS[anAugDR_Function,numPers_Integer]:=
With[{draws=RandomVariate[theDistnkZLB[[1,1,2]],numPers],
initVec=Transpose[{{99,etaVal,99,99,99,99,99,99}}]},
With[{mats=FoldList[((anAugDR @@ Flatten[{#1[[Range[8]]],#2}]))&,initVec,draws]},Flatten/@mats]]


iterateNKCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]:=
With[{mats=
NestList[((anAugDR @@ Flatten[{#1[[Range[8]]],#2}]))&,initVec,draws]},
Flatten/@mats]

chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]:=
Module[{},
If[
Catch[
Nest[With[{val=Apply[anAugDRCE,Flatten[#]][[Range[8]]]},
If[Norm[val]>lim,Throw[False,"chkBounded"],val]]&,aPt,numPers],
"chkBounded"]===False,False,True]]

Print["about to simulate fixed seed"];
SeedRandom[1234]
theRes=simulateNkNKCS[200];
Print["done simulate"];
justEta=theRes[[All,{2}]];
nkNKCSMean=Mean[justEta];
nkNKCSSD=StandardDeviation[justEta];

normedRes=(#/nkNKCSSD)&/@((#-nkNKCSMean)&/@justEta);
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
nkNKCSMinZ=Min/@Transpose[zz];
nkNKCSMaxZ=Max/@Transpose[zz];
{ig,theEtas,ig,ig,ig,ig,ig,ig}=Transpose[theRes];

Print["try 4 time SD for eta range"];
nkNKCSMean=Append[nkNKCSMean,0];
nkNKCSSD=Append[4*nkNKCSSD,sigVal];
nkNKCSMinZ=Append[nkNKCSMinZ,-3];
nkNKCSMaxZ=Append[nkNKCSMaxZ,3];
nkNKCSvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0}}}}]},{{{0,1}}}}];
(*
*)

End[] (* End Private Context *)

EndPackage[]
Print["done reading nkZLB.m"]


