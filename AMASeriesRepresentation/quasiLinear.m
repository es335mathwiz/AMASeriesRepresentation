(* Wolfram Language Package *)

BeginPackage["quasiLinear`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`","nkZLB`"}]
(* Exported symbols added here with SymbolName::usage *)  

anXqLin::usage="for test input";
anEpsqLin::usage="for test input";
anXEpsqLin::usage="for test input";
aZqLin::usage="for test input";
anXEpsZsqLin::usage="for test input";

anXFlatqLin::usage="for test input";
anEpsFlatqLin::usage="for test input";
anXEpsFlatqLin::usage="for test input";
aZFlatqLin::usage="for test input";
anXEpsZsFlatqLin::usage="for test input";

probDimsqLin::usage="for test input";
theDistqlZLB::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistqlZLB::usage="theDist={{{ee,PerfectForesight]}}};"
linModQLZLB::usage="linear model matrices for approx"
aGSpecqLin::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
qLinEqnsQLinBackLookingFixCompSlack::usage="qLinEqnsQLinBackLookingFixCompSlack"
qLinEqnsQLinBackLookingExpFixCompSlack::usage="qLinEqnsQLinBackLookingFixCompSlack"
qLinEqnsqLin::usage="model equations"
qLinEqnsqLinNot::usage="model equations"
eqnsEulerCompiledqLin::usage="eqnsEulerCompiledqLin"

simulateQLinQLINCS::usage="simulateQLinQLINExact[numPers_Integer]"
qlCSMean::usage="qlCSMean"
qlCSSD::usage="qlCSSD"
qlCSvv::usage="qlCSvv"
qlCSMinZ::usage="qlCSMinZ"
qlCSMaxZ::usage="qlCSMaxZ"

chkBounded::usage="chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]"


iterateQLINCSDRCE::usage="iterateQLINCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]"


Begin["`Private`"] (* Begin Private Context *) 

(*parameters page 28 guerrieri iacoviello*)
(*
paramSubs={
kappa->(nkZLB`Private`epsi-1)/nkZLB`Private`phiP,
RR->Log[nkZLB`Private`piBar/nkZLB`Private`beta],
beta->nkZLB`Private`beta,
gammaPi->2,
gammaY->2,
rho->nkZLB`Private`rho,
sigma->150*nkZLB`Private`sigma}//.nkZLB`Private`paramSubs;

stdeta=Sqrt[(stdinnov^2)/(1-rho^2)]
psi=stdeta*Sqrt[ns-1]


*)

paramSubs={
beta->.99,
gammaPi->2,
gammaY->0,
kappa->0.05,
piBar->1.005,
RR->Log[piBar/beta],
rho->0.91,
sigma->0.00025}

happy=Solve[4==Sqrt[((sigTry)^2)/(1-(rho^2))]/.paramSubs,sigTry]

qlEqnsCommon={
yy[t] - (yy[t+1] -rr[t]+pi[t+1]+eta[t-1]),
pi[t]-(beta*pi[t+1]+kappa*yy[t]),
eta[t] -(rho*eta[t-1] +eps[eta][t])
	}
qlEqnsNotBinding=Append[qlEqnsCommon,rr[t]-gammaPi*pi[t]-gammaY*yy[t]]
qlEqnsBinding=Append[qlEqnsCommon,rr[t]-(-RR)]


ssEqnSubs=
{xx_Symbol[t+v_.]->xx}

qlEqnsNotBindingSubbed=(((qlEqnsNotBinding/.ssEqnSubs)//.paramSubs)/.eps[eta][t]->0)



psiz=IdentityMatrix[4]

hmatSymbRawRE=(((curious=equationsToMatrix[
qlEqnsNotBinding//.paramSubs/.eps[eta][t]->0]//FullSimplify)/.{xxxx_[t+_.]->xxxx})(*//.ssSolnSubs*))//FullSimplify;


hmatSymbRE=hmatSymbRawRE//.paramSubs

{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.paramSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];




{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];


qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1,2}]]];



{bmatSymbRE,phimatSymbRE,fmatSymbRE}=Chop[symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify];



psiepsSymbRE=-Transpose[{((D[#,eps[eta][t]]&/@ qlEqnsNotBinding)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})}//.paramSubs]



psicSymbRE=ConstantArray[0,{4,1}]




linModQLZLB=Chop[{hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{}}]
Print["took out backlooking"]







argsSubs={
eta[t-1]->etatm1, 
pi[t-1]->pitm1, 
rr[t-1]->rrtm1, 
yy[t-1]->YYtm1,
eta[t]->etat, 
pi[t]->pit, 
rr[t]->rrt, 
yy[t]->yyt,
eta[t+1]->etatp1, 
pi[t+1]->pitp1, 
rr[t+1]->rrtp1, 
yy[t+1]->yytp1,
eps[eta][t]->epsVal
}



theArgs={etatm1,pitm1,rrtm1,yytm1,epsVal};


eqnsForBind=((qlEqnsBinding//.paramSubs)/.argsSubs)


eqnsForNotBind=((qlEqnsNotBinding//.paramSubs)/.argsSubs)

dollarFailed=$Failed

qlEqnsqlZLB={
 { 
{True&,
Compile @@ {
{
{etatm1,_Real},{pitm1,_Real},{rrtm1,_Real},{yytm1,_Real},
{etat,_Real},{pit,_Real},{rrt,_Real},{yyt,_Real},
{etatp1,_Real},{pitp1,_Real},{rrtp1,_Real},{yytp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[dollarFailed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},
Function[{aPt,aRes},
If[aRes===Failed,False,Print[{"huh",(-RR//.paramSubs),aRes[[3,1]]}];And[aRes[[3,1]]>=(-RR//.paramSubs)]]]},
{(True)&,
Compile @@ {
{
{etatm1,_Real},{pitm1,_Real},{rrtm1,_Real},{yytm1,_Real},
{etat,_Real},{pit,_Real},{rrt,_Real},{yyt,_Real},
{etatp1,_Real},{pitp1,_Real},{rrtp1,_Real},{yytp1,_Real},
{epsVal,_Real}
},
(eqnsForBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[dollarFailed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},(True)&}},
Function[{aPt,allRes},Print["qlZLB:",{aPt,allRes}];
If[And[allRes[[1]]===dollarFailed,allRes[[2]]===dollarFailed],Throw[dollarFailed,"noSolutionFound"]];
If[allRes[[1]]===dollarFailed,Flatten[allRes[[2]]],
If[True(*allRes[[1,1,1]]>=allRes[[2,1,1]]*),Flatten[allRes[[1]]],Flatten[allRes[[2]]]]]]
}

qlEqnsqlZLBNot={
 { 
{True&,
Compile @@ {
{
{etatm1,_Real},{pitm1,_Real},{rrtm1,_Real},{yytm1,_Real},
{etat,_Real},{pit,_Real},{rrt,_Real},{yyt,_Real},
{etatp1,_Real},{pitp1,_Real},{rrtp1,_Real},{yytp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[dollarFailed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},
Function[{aPt,aRes},
If[aRes===dollarFailed,False,And[aRes[[3,1]]>=(-RR//.paramSubs)]]]},
{(True)&,
Compile @@ {
{
{etatm1,_Real},{pitm1,_Real},{rrtm1,_Real},{yytm1,_Real},
{etat,_Real},{pit,_Real},{rrt,_Real},{yyt,_Real},
{etatp1,_Real},{pitp1,_Real},{rrtp1,_Real},{yytp1,_Real},
{epsVal,_Real}
},
(eqnsForNotBind),"RuntimeOptions"->{"RuntimeErrorHandler"->Function[dollarFailed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}},(True)&}},
Function[{aPt,allRes},Print["qlZLB:",{aPt,allRes}];
If[And[allRes[[1]]===dollarFailed,allRes[[2]]===dollarFailed],Throw[dollarFailed,"noSolutionFound"]];
If[allRes[[1]]===dollarFailed,Flatten[allRes[[2]]],
If[True(*allRes[[1,1,1]]>=allRes[[2,1,1]]*),Flatten[allRes[[1]]],Flatten[allRes[[2]]]]]]
}


theDistqlZLB={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDistqlZLB={{{ee,PerfectForesight}}};




anXqlZLB=Transpose[{{1.02,99,99,99}}];
anEpsqlZLB={{0.01}};
anXEpsqlZLB=Join[anXqlZLB,anEpsqlZLB]
aZqlZLB=Transpose[{{.1,.2,.3,.4}}]
anXEpsZsqlZLB=Join[anXEpsqlZLB,aZqlZLB];

anXFlatqlZLB=anXqlZLB//Flatten;
anEpsFlatqlZLB=anEpsqlZLB//Flatten;
anXEpsFlatqlZLB=anXEpsqlZLB//Flatten;
aZFlatqlZLB=aZqlZLB//Flatten;
anXEpsZsFlatqlZLB=anXEpsZsqlZLB//Flatten;

probDimsqlZLB={4,1,4};


etaVal=0
etaLow = -.1;
etaHigh = .1;
sigVal = sigma //. (paramSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;


aGSpecqLin={{2,3,4},1,{{4,etaLow,etaHigh},{3,sigLow,3*sigHigh}}};




simulateQlQLCS[numPers_Integer]:=
With[{draws=RandomVariate[theDistqlZLB[[1,1,2]],numPers],
initVec=Transpose[{{etaVal,99,99,99}}],
fMul=Inverse[IdentityMatrix[4]-fmatSymbRE]},
With[{mats=FoldList[(bmatSymbRE . #1+ (phimatSymbRE .psiepsSymbRE .{{#2}})+
fMul.phimatSymbRE.psicSymbRE)&,initVec,draws]},
Flatten/@mats]]

simulateQlQLCS[anAugDR_Function,numPers_Integer]:=
With[{draws=RandomVariate[theDistqlZLB[[1,1,2]],numPers],
initVec=Transpose[{{99,etaVal,99,99,99,99,99,99}}]},
With[{mats=FoldList[((anAugDR @@ Flatten[{#1[[Range[4]]],#2}]))&,initVec,draws]},Flatten/@mats]]


iterateQLCSDRCE[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ]:=
With[{mats=
NestList[((anAugDR @@ Flatten[{#1[[Range[4]]],#2}]))&,initVec,draws]},
Flatten/@mats]

chkBounded[anAugDRCE_Function,numPers_Integer,aPt_?MatrixQ,lim_?NumberQ]:=
Module[{},
If[
Catch[
Nest[With[{val=Apply[anAugDRCE,Flatten[#]][[Range[4]]]},
If[Norm[val]>lim,Throw[False,"chkBounded"],val]]&,aPt,numPers],
"chkBounded"]===False,False,True]]

Print["about to simulate fixed seed"];
SeedRandom[1234]
theSimRes=Drop[simulateQlQLCS[2000],200];
Print["done simulate"];
justEta=theSimRes[[All,{1}]];
qlCSMean=Mean[justEta];
qlCSSD=StandardDeviation[justEta];

normedRes=(#/qlCSSD)&/@((#-qlCSMean)&/@justEta);
{uu,ss,vv}=SingularValueDecomposition[normedRes];
zz=normedRes .vv;
qlCSMinZ=Min/@Transpose[zz];
qlCSMaxZ=Max/@Transpose[zz];
{theEtas,thePis,theRs,theYs}=Transpose[theSimRes];

Print["try 10 time SD for eta range"];
qlCSMean=Append[qlCSMean,0];
qlCSSD=Append[20*qlCSSD,sigVal];
qlCSMinZ=Append[qlCSMinZ,-3];
qlCSMaxZ=Append[qlCSMaxZ,3];
qlCSvv=ArrayFlatten[{{ArrayFlatten[{{vv,{{0}}}}]},{{{0,1}}}}];
(*
*)

End[] (* End Private Context *)
EndPackage[]
Print["done reading quasiLinear.m"]
