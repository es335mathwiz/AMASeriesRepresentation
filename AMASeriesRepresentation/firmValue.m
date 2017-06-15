BeginPackage["firmValue`",{"AMASeriesRepresentation`","ProtectedSymbols`","AMAModel`","SymbolicAMA`","NumericAMA`"}]


probDimsFV::usage="for test input";

exactDR::usage="FVExactDR"
exactCondExpFV::usage="FVExactCondExp"
theDistFV::usage="theDist={{{ee,NormalDistribution[0,sigVal]}}};"
thePFDistFV::usage="theDist={{{ee,PerfectForesight]}}};"
linModFV::usage="linear model matrices for approx"
aGSpecFV::usage="aGSpec={{2},1,{{1,divLow,divHigh},{3,sigLow,3*sigHigh}}}";
eqnsCompiledFV::usage="model equations function"
Begin["`Private`"]

FVSubs={rr->0.10,delta->0.60,sigma->0.01,dLow->0,dHigh->1};

aGSpecFV={{2},2,{{3,dLow,dHigh},{3,-3*sigma,3*sigma}}}/.FVSubs;


FVEqns={
vv[t+1] -( (1+rr)*vv[t] - div[t+1]),
div[t] -( (1-delta)*div[t-1] + eps[div][t])
};

eqnsCompiledFV=Compile @@ {
{
{divtm1,_Real},{vvtm1,_Real},
{divt,_Real},{vvt,_Real},
{divtp1,_Real},{vvtp1,_Real},
{epsVal,_Real}
},
{
vvtp1 -( (1+0.1)*vvt - divtp1),
divt -( (1-.6)*divtm1 + epsVal)
},"RuntimeOptions"->{"RuntimeErrorHandler"->Function[$Failed],"CatchMachineOverflow"->True,"CatchMachineUnderflow"->True}}


theDistFV={{{ee,NormalDistribution[0,sigma]}}}/.FVSubs;
thePFDistFV={{{ee,PerfectForesight}}};



ssSolnSubsRE={vv->0,div->0}

Print["RE solutions"]
hmatSymbRawRE=(((equationsToMatrix[
FVEqns/.{eps[_][_]->0}/.FVSubs]//FullSimplify)/.{xxxx_[t+_.]->xxxx})//.ssSolnSubsRE)//FullSimplify;
psiepsSymbRE=-Transpose[{((D[#,eps[theta][t]]&/@ FVEqns)/.{eps[_][_]->0,xxxx_[t+_.]->xxxx})//.ssSolnSubsRE}/.FVSubs]

hmatSymbRE=hmatSymbRawRE//.FVSubs
hSumRE=hmatSymbRE[[All,Range[2]]]+hmatSymbRE[[All,2+Range[2]]]+hmatSymbRE[[All,2*2+Range[2]]];

ssSolnVecRE={{0},{0}};
psicSymbRE=hSumRE . ssSolnVecRE;


{zfSymbRE,hfSymbRE}=symbolicAR[hmatSymbRE//.FVSubs];
amatSymbRE=symbolicTransitionMatrix[hfSymbRE];
{evlsSymbRE,evcsSymbRE}=Eigensystem[Transpose[amatSymbRE]];
qmatSymbRE=Join[zfSymbRE,evcsSymbRE[[{1}]]];

Print["computing and simplifying the symbolic b phi f etc"]
{bmatSymbRE,phimatSymbRE,fmatSymbRE}=symbolicComputeBPhiF[hmatSymbRE,qmatSymbRE]//Simplify;

linModFV={hmatSymbRE//N,bmatSymbRE // N, phimatSymbRE // N, 
    fmatSymbRE // N, psiepsSymbRE // N, 
    psicSymbRE // N, psiz // N,{{0}}};

psiz=IdentityMatrix[2]




End[]
EndPackage[]

