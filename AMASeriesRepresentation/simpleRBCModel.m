PrependTo[$Path,"../../mathAMA/AMAModel/"];
PrependTo[$Path,"../../mathAMA/NumericAMA/"];
PrependTo[$Path,"../../mathAMA/SymbolicAMA"];
PrependTo[$Path,"../../mathSmolyak/mathSmolyak/"];
PrependTo[$Path,"../../protectedSymbolsDir/ProtectedSymbols"];
PrependTo[$Path,"../../AMASeriesRepresentation/AMASeriesRepresentation"];
Print["reading simpleRBCModel.m"]
BeginPackage["simpleRBCModel`",{"AMASeriesRepresentation`",(*"occBindRecur`",*)"ProtectedSymbols`","AMAModel`","SymbolicAMA`","NumericAMA`"(*,"ProjectionInterface`"*)}]


(*
ratioThetaToC::usage="rbc model variable"
cc::usage="rbc model variable"
kk::usage="rbc model variable"
rho::usage="rbc model parameter"
theta::usage="rbc model parameter"
alpha::usage="rbc model parameter"
delta::usage="rbc model parameter"
sigma::usage="rbc model parameter"
rbcEqns::usage="rbc model equations"
simpParamSubs::usage="simpParamSubs=Join[paramSubs,forParamSubs]"
ssSolnSubsRE::usage="rational expectations steady state"
ssSolnSubsPF::usage="perfect foresight steady state"

compApproxRE::usage="compApproxRE[theHmat_?MatrixQ,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},kk_,theta_,epsNow_,iters_Integer]"
compApproxDiffRE::usage="compApproxDiffRE[theHmat_?MatrixQ,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},kk_,theta_,epsNow_,iters_Integer]"
maxZsRE::usage="maxZsRE[theHmat_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?Matrix,{{lowc_,highc_},{lowk_,highk_},{lowt_,hight_},{lowe_,highe_}},iters_Integer]"

compBounds::usage="compBounds[theHmat_?MatrixQ,linMod:{BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},{{lowc_,highc_},{lowk_,highk_},{lowt_,hight_},{lowe_,highe_}},iters_Integer]"

   *)
anX::usage="for test input";
anEps::usage="for test input";
anXEps::usage="for test input";
aZ::usage="for test input";
anXEpsZs::usage="for test input";

anXFlat::usage="for test input";
anEpsFlat::usage="for test input";
anXEpsFlat::usage="for test input";
aZFlat::usage="for test input";
anXEpsZsFlat::usage="for test input";

probDims::usage="for test input";

simpRBCExactDR::usage="simpRBCExactDR"

theDist::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDist::usage="theDist={{{ee,PerfectForesight]}}};"
linMod::usage="linear model matrices for approx"
aGSpec::usage="aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}}";
rbcEqnsFunctionalNext::usage="model equations function"
Begin["`Private`"]





CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
 CRRAUDrv[cc[t],1]-(delta*((theta[t+1])* CRRAUDrv[cc[t+1],1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t])*(kk[t-1]^alpha)),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t])
}
(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100,
rho->95/100,
sigma->1/100
} ;

theDist={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;
thePFDist = {{{ee, PerfectForesight}}};

forSubs={alpha^(1 - alpha)^(-1)*delta^(1 - alpha)^(-1)};
simpSubs=Thread[forSubs->nu];
forParamSubs=Thread[nu->forSubs]//.paramSubs;
simpParamSubs=Join[paramSubs,forParamSubs];


If[Length[ssSolnSubsRE]===0,
Print["computing steady state subs"];
thNow[lastTh_,eps_]:=(E^eps)*lastTh^rho/.simpParamSubs;
nxtK[lastK_,thNowVal_]:=((alpha*delta))*thNowVal*lastK^(alpha)/.simpParamSubs;
yNow[kLag_,thNowVal_]:=thNowVal*kLag^(alpha)/.simpParamSubs;
anExpRE=Expectation[E^ep,ep\[Distributed] NormalDistribution[0,sigma]/.simpParamSubs];
anExpPF=1;
Identity[thSubsRE=Flatten[Solve[theta==anExpRE*thNow[theta,0],theta]][[2]]];
Identity[kSSSubRE=Flatten[Solve[nxtK[kk,theta/.thSubsRE]==kk,kk]][[-1]]];
Identity[cSSSubRE=cc->(yNow[kk/.kSSSubRE,theta/.thSubsRE]-kk/.kSSSubRE)];
Identity[ssSolnSubsRE=Flatten[{thSubsRE,kSSSubRE,cSSSubRE}]];
Identity[thSubsPF=Flatten[Solve[theta==theta^rho,theta]][[1]]];
Identity[kSSSubPF=Flatten[Solve[nxtK[kk,theta/.thSubsPF]==kk,kk]][[-1]]];
Identity[cSSSubPF=cc->(yNow[kk/.kSSSubPF,theta/.thSubsPF]-kk/.kSSSubPF)];
Identity[ssSolnSubsPF=Flatten[{thSubsPF,kSSSubPF,cSSSubPF}]];
Print["done computing steady state subs"];
]




psiz=IdentityMatrix[3]

Print["RE solutions"]
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xx_[t+_.]->xx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xx_[t+_.]->xx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[3]]]+hmatSymbRE[[All,3+Range[3]]]+hmatSymbRE[[All,6+Range[3]]];

ssSolnVecRE={{cc},{kk},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

Print["defining exactdr"]
simpRBCExactDR = 
 Function[{cc, kk, th, eps}, 
With[{tht=(th^rho)*E^eps//.simpParamSubs//N},
With[{kkt=(tht*alpha*delta*kk^alpha)//.simpParamSubs//N},
With[{cct=((tht*kk^alpha)*(1-alpha*delta))//.simpParamSubs//N},
Transpose[{{cct,kkt,tht}}]]]]]

simpRBCExactCondExp = makeREIterFunc[simpRBCExactDR,theDist]








rbcEqnsFunctionalNext=Compile[
{
{ctm1,_Real},{kktm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*((1.*thetatp1)/cctp1))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)}]



linMod={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};
    
lilLinMod=linMod;
lilHmat=hmat;
Save["forLilMod.mth",{lilHmat,lilLinMod}]
simpX0Z0=genX0Z0Funcs[linMod];


 
thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk /.kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc /.cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;
kHigh = 4*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;
pdf=NormalDistribution[0, sigVal];
Print["curious simp",{thVal,kVal,cVal,ssSolnSubsRE,simpParamSubs}];

    
anX=Transpose[{{.2,.18,1.1}}];
anEps={{0.01}};
anXEps=Join[anX,anEps]
aZ=Transpose[{{.1,.2,.3}}]
anXEpsZs=Join[anXEps,aZ];

anXFlat=anX//Flatten;
anEpsFlat=anEps//Flatten;
anXEpsFlat=anXEps//Flatten;
aZFlat=aZ//Flatten;
anXEpsZsFlat=anXEpsZs//Flatten;

probDims={3,1,3};


aGSpec={{1},1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};
Print["path=",{$Path,Directory[]}]
Get["genArbLin.mth"]
End[]
EndPackage[]
Print["done reading simpleRBCModel.m"]

