(* Wolfram Language Package *)

BeginPackage["maliarRBCExample`", { "AMASeriesRepresentation`", "ProtectedSymbols`", "AMAModel`", "SymbolicAMA`", "NumericAMA`"}]
(* Exported symbols added here with SymbolName::usage *)  
eqnsCompiled::usage="compiled version of model equations"

Begin["`Private`"] (* Begin Private Context *) 


CRRAUDrv[cc_,eta_]:=If[eta==1,D[Log[cc],cc],D[(1/(1-eta))*(cc^(1-eta)-1),cc]]



(*pg 165 of  maliar maliar solving neoclassical growth model  
closed form solution version  beta=1 geometric discounting
chkcobb douglas production*)

rbcEqns={
CRRAUDrv[cc[t],1]-
(delta*(theta[t])*(nlPart[t+1]*((alpha *(kk[t]^(alpha-1)) )))),
cc[t] + kk[t]-((theta[t-1])*(kk[t-1]^alpha)),
nlPart[t] - (nlPartRHS=(1)* CRRAUDrv[cc[t],1]),
theta[t]-E^(rho*Log[theta[t-1]] + eps[theta][t])
}




(*parameters page 21 using state 1*)
paramSubs={
alpha->36/100,
delta->95/100,
rho->95/100,
sigma->1/100
} ;

rbcEqnsCompiled=Compile @@ {
{
{cctm1,_Real},{kktm1,_Real},{nltm1,_Real},{thetatm1,_Real},
{cct,_Real},{kkt,_Real},{nlt,_Real},{thetat,_Real},
{cctp1,_Real},{kktp1,_Real},{nltp1,_Real},{thetatp1,_Real},
{epsVal,_Real}
},
{cct^(-1) - (0.342*((1.*thetatp1)/cctp1))/kkt^(16/25), 
cct + kkt - 1.*kktm1^(9/25)*thetat, 
nlt - 1/cct,
thetat - 1.*2.718281828459045^epsVal*thetatm1^(19/20)}}

psiz=IdentityMatrix[4]
(*
Print["RE solutions"]
hmatSymbRawRE=(((equationsToMatrix[
rbcEqns/.simpParamSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)/.{eps[_]->0}//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ rbcEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.simpParamSubs]

hmatSymbRE=hmatSymbRawRE//.simpSubs
hSumRE=hmatSymbRE[[All,Range[4]]]+hmatSymbRE[[All,4+Range[4]]]+hmatSymbRE[[All,8+Range[4]]];

ssSolnVecRE={{cc},{kk},{nlPart},{theta}}//.ssSolnSubsRE;
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.simpParamSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linMod={bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};


theDist={{{ee,NormalDistribution[0,sigma]}}}//.paramSubs;




*)
End[] (* End Private Context *)

EndPackage[]