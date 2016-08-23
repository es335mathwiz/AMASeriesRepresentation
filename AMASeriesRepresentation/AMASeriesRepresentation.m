
BeginPackage["AMASeriesRepresentation`",
 {"JLink`","ProtectedSymbols`"}]

(*Begin Usage Definitions*)
PerfectForesight::usage="degenerate distribution implementing perfect foresight"
worstPathForErrDRREIntegrate::usage=
"place holder for worstPathForErrDRREIntegrate"

evalBadPathErrDRREIntegrate::usage=
"place holder for evalBadPathErrDRREIntegrate"

evalPathErrDRREIntegrate::usage=
"place holder for evalPathErrDRREIntegrate"

doFuncArg::usage=
"place holder for doFuncArg"

pathErrsDRPF::usage=
"place holder for pathErrsDRPF"

pathErrsDRREIntegrate::usage=
"place holder for pathErrsDRREIntegrate"

iterateDRPF::usage=
"place holder for iterateDRPF"

genNSFunc::usage=
"place holder for genNSFunc"

makeREIterFunc::usage=
"place holder for makeREIterFunc"

getRegimeTransProbFuncType::usage=
"place holder for getRegimeTransProbFuncType"

myNExpectation::usage=
"place holder for myNExpectation"

getDistribs::usage=
"place holder for getDistribs"

genXZFuncRE::usage=
"place holder for genXZFuncRE"

genIntVars::usage=
"place holder for genIntVars"

genXZREInterpFunc::usage=
"place holder for genXZREInterpFunc"

genX0Z0Funcs::usage=
"place holder for genX0Z0Funcs"

checkMod::usage=
"place holder for checkMod"

genFRFunc::usage=
"place holder for genFRFunc"

genFPFunc::usage=
"place holder for genFPFunc"

myFixedPoint::usage=
"place holder for myFixedPoint"


getH::usage=
"getH[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getB::usage=
"getB[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getF::usage=
"getF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"

getGridPtTrips::usage=
"place holder for getGridPtTrips"

getNumVars::usage=
"place holder for getNumVars"

makeInterpFunc::usage=
"place holder for makeInterpFunc"

nestIterREInterp::usage=
"place holder for nestIterREInterp"

genInterpData::usage=
"place holder for genInterpData"

oneDimGridPts::usage=
"place holder for oneDimGridPts"

gridPts::usage=
"place holder for gridPts"

fillIn::usage=
"place holder for fillIn"

fillInSymb::usage=
"place holder for fillInSymb"

doIterREInterp::usage=
"place holder for doIterREInterp"
 

getPhi::usage=
"getPhi[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getPsiZ::usage=
"getPsiZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getPsiC::usage=
"getPsiC[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getPsiEps::usage=
"getPsiEps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getNumZ::usage=
"getNumZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getNumZ::usage=
"getNumZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getNumZ::usage=
"getNumZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"


getNumZ::usage=
"getNumZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ]"<>
"number of z variables"

genZVars::usage=
"place holder for genZVars"


genEpsVars::usage=
"placehoder for usage"

multiStep::usage=
"place holder for multiStep"

multiStepZ::usage=
"place holder for multiStepZ"

multiStepX::usage=
"place holder for multiStepX"

checkLinMod::usage=
"place holder for checkLinMod"


genLilXkZkFunc::usage=
"genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ,XZFuncs:({_Function,_Integer}),xtGuess_?MatrixQ,drvPairs:({{{aa_Integer,bb_Integer}...},
     eqnFunc:(_Function|_CompiledFunction)}|{{},{}}):{{},{}}]"<>
"\ngenerate a function that computes x and z given a guess for xt\n"<>
"genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ,fCon_?MatrixQ,drvPairs:({{{aa_Integer,bb_Integer}...},
    eqnFunc:(_Function|_CompiledFunction)}|{{},{}}):{{},{}}]"<>
"\ngenerate a function that computes x z based on an assumed F sum\n"<>
"genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ,theZs:{_?MatrixQ..}]"<>
"\ngenerate a function that computes x and z given sequence of Zs\n"<>
" genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
   psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
   ,{}]"<>
"\ngenerate a function that computes x for Zs = 0\n"


fSumC::usage=
"compiled function computing the sum of the Zs weighted by F"


fSum::usage=
"place holder fSum"

getNumEpsVars::usage=
"place holder for getNumEpsVars"

iterateDRREIntegrate::usage=
"place holder for iterateDRREIntegrate"

genPath::usage=
"place holder for genPath"


Begin["`Private`"]


(*begin code for worstPathForErrDRREIntegrate*)

worstPathForErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{fMinRes=evalBadPathErrDRREIntegrate[drFunc,noEpsVec,distribSpec,eqnsFunc]},
        With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},
        With[{badPath=iterateDRREIntegrate[drFunc,Join[noEpsVec,badEps],distribSpec,2]},
                Join[badPath,badEps]]]]

worstPathForErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{fMinRes=
evalBadPathErrDRREIntegrate[phi,drFunc,noEpsVec,distribSpec,eqnsFunc]},
        With[{badEps=Transpose[{(Map[First,fMinRes[[2]]])/.fMinRes[[2]]}]},
        With[{badPath=iterateDRREIntegrate[drFunc,Join[noEpsVec,badEps],distribSpec,2]},
                Join[badPath,badEps]]]]

(*end code for worstPathForErrDRREIntegrate*)


(*begin code for evalBadPathErrDRREIntegrate*)
evalBadPathErrDRREIntegrate[drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
        With[{theVal=evalPathErrDRREIntegrate[drFunc,Join[noEpsVec,Transpose[{tryEps}]],distribSpec,eqnsFunc]},
                With[{theNorm=Norm[theVal,Infinity]},
                (*Print["stillex:",{tryEps,theVal,Norm[theVal,Infinity],theNorm}];*)theNorm]];
        With[{outerEVars=Table[Unique["eVs"],{getNumEpsVars[distribSpec]}]},
        With[{maxArgs=Map[{#,0}&,outerEVars],cons=Apply[And,  (Map[(-0.01<=#<=0.01)&, outerEVars])]},
        FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]


evalBadPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,noEpsVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
With[{funcName=Unique["fName"]},
funcName[tryEps:{_?NumberQ..}]:=
With[{theVal=evalPathErrDRREIntegrate[drFunc,Join[noEpsVec,Transpose[{tryEps}]],distribSpec,eqnsFunc]},
                (*Print["otherex:",theVal,Norm[theVal,Infinity]];*)Norm[theVal,Infinity]];
        With[{outerEVars=Table[Unique["eVs"],{getNumEpsVars[distribSpec]}]},
        With[{maxArgs=Map[{#,0}&,outerEVars],cons=Apply[And,  (Map[(-0.01<=#<=0.01)&, outerEVars])]},
        FindMaximum[{funcName[outerEVars],cons},maxArgs]]]]

(*end code for evalBadPathErrDRREIntegrate*)


(*begin code for evalPathErrDRREIntegrate*)
evalPathErrDRREIntegrate[drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
pathErrsDRREIntegrate[drFunc,initVec,distribSpec,eqnsFunc,2]//First



evalPathErrDRREIntegrate[phi_?MatrixQ,
drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction)]:=
phi . (pathErrsDRREIntegrate[drFunc,initVec,distribSpec,eqnsFunc,2])//First




(*end code for evalPathErrDRREIntegrate*)


(*begin code for doFuncArg*)
doFuncArg[pathNow_?MatrixQ,epsVals_?MatrixQ,numX_Integer,oSet_Integer]:=
With[{firstArg=Join[Identity[pathNow[[oSet*numX+Range[3*numX]]]],Identity[epsVals]]},
firstArg]


(*end code for doFuncArg*)


(*begin code for genPath*)


genPath[xzFunc_Function,
{XZFunc_Function,numSteps_Integer},xtm1Val_?MatrixQ,epsVal_?MatrixQ,numTerms_Integer]:=
With[{numXVars=Length[xtm1Val]},
With[{xtVal=Apply[xzFunc,Flatten[Join[xtm1Val,epsVal]]]},
With[{xzRes=If[numTerms==1,{},
Apply[multiStepX[{XZFunc,numSteps},numXVars,numTerms-1],Flatten[xtVal]]]},
        Join[xtm1Val,xtVal[[Range[numXVars]]],Apply[Join,xzRes]]]]]
(*end code for genPath*)


(*begin code for pathErrsDRPF*)
   
 
pathErrsDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]:=
With[{pathNow=iterateDRPF[drFunc,initVec,numEps,numPers],numX=Length[initVec]-numEps},
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
        restArgs=(Map[doFuncArg[pathNow,Table[{0},{numEps}],numX,#-2]&,Range[3,numPers]])},
With[{first=Transpose[{Apply[eqnsFunc,Flatten[firstArg]]}]},
        With[{theRest=Map[Transpose[{(Apply[eqnsFunc,Flatten[#]])}]&,restArgs]},
                Prepend[theRest,first]
]]]]/;
And[numPers>1]


(*end code for pathErrsDRPF*)


(*begin code for pathErrsDRREIntegrate*)
pathErrsDRREIntegrate[drFunc_Function,initVec_?MatrixQ,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},eqnsFunc:(_Function|_CompiledFunction),numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{pathNow=iterateDRREIntegrate[drFunc,initVec,distribSpec,numPers],numX=Length[initVec]-numEps},(*Print["pathErrsDRREIntegrate:",pathNow];*)
With[{firstArg=doFuncArg[pathNow,Identity[Reverse[initVec[[-Range[numEps]]]]],numX,0],
        restArgs=(Map[doFuncArg[pathNow,Table[{0},{numEps}],numX,#-2]&,Range[3,numPers]])},
With[{first=Transpose[{Apply[eqnsFunc,Flatten[firstArg]]}]},
        With[{theRest=Map[Transpose[{(Apply[eqnsFunc,Flatten[#]])}]&,restArgs]},(*Print["pathErrs:",{pathNow,theRest,first}];*)
                Prepend[theRest,first]
]]]]]/;
And[numPers>1]
 

(*end code for pathErrsDRREIntegrate*)


(*begin code for iterateDRPF*)
 
iterateDRPF[drFunc_Function,initVec_?MatrixQ,numEps_Integer,numPers_Integer]:=
With[{firVal=Apply[drFunc,Flatten[initVec]],numX=Length[initVec]-numEps,theZeros=Table[0,{numEps}]},
With[{iterated=
NestList[(Apply[drFunc,Flatten[Append[#[[Range[numX]]],theZeros]]])&,firVal,numPers-1]},
Join[initVec[[Range[numX]]],Apply[Join,(Map[#[[Range[numX]]]&,iterated])]]]]/;
And[numPers>0]


(*end code for iterateDRPF*)


(*begin code for genNSFunc*)
genNSFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc:(_Function|_CompiledFunction),opts:OptionsPattern[]]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX+numEps}],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
funcName[funcArgsNot:{_(*?NumberQ*)..}]:=
Module[{theVars=Join[funcArgsNot]},
Apply[eqnsFunc,(Flatten[Apply[xkFunc,theVars]])]];
ReplacePart[
Function[xxxx,With[{zVals=zArgs/.NSolve[funcName[Join[funcArgs,zArgs]],zArgs,Reals,Apply[Sequence,FilterRules[{opts},Options[NSolve]]]][[1]]},
Join[(Apply[xkFunc,Join[funcArgs,zVals]])[[numX+Range[numX]]],
Transpose[{zVals}]]]],
1->funcArgs]]


(*end code for genNSFunc*)


(*begin code for iterateDRREIntegrate*)
iterateDRREIntegrate[drFunc:(_Function|_CompiledFunction),initVec_?MatrixQ,
        distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numPers_Integer]:=
With[{numEps=getNumEpsVars[distribSpec],firVal=Apply[drFunc,Flatten[initVec]]},
        With[{numX=Length[initVec]-numEps,iterFunc=makeREIterFunc[drFunc,distribSpec]},
With[{iterated=
NestList[((Transpose[{Flatten[Apply[iterFunc,Flatten[#]]]}]))&,firVal,numPers-1]},
Join[initVec[[Range[numX]]],Apply[Join,(Map[Identity[#[[Range[numX]]]]&,iterated])]]]]]/;
And[numPers>0]


(*end code for iterateDRREIntegrate*)


(*begin code for makeREIterFunc*)

makeREIterFunc[drFunc:(_Function|_CompiledFunction),distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{numEps=getNumEpsVars[distribSpec]},
With[{numX=Length[drFunc[[1]]]-numEps,numZ=0},
        genXZFuncRE[{numX,numEps,numZ},drFunc,distribSpec]]]


(*end code for makeREIterFunc*)


(*begin code for getRegimeTransProbFuncType*)
getRegimeTransProbFuncType[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
If[regimeTransProbFunc=={},noTransFunc,regimeTransProbFunc[[2]]]

getRegimeTransProbFunc[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=regimeTransProbFunc[[3]]

(*end code for getRegimeTransProbFuncType*)


(*begin code for getNumEpsVars*)
getNumEpsVars[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=Length[expctSpec]


(*end code for getNumEpsVars*)


(*begin code for myNExpectation*)



myNExpectation[funcName_Symbol[funcArgs_List,idx_Integer],anEpsVar_\[Distributed] PerfectForesight]:=
Apply[funcName,Append[ReplacePart[{funcArgs},{{(1),(-1)}->0}],idx]]
myNExpectation[funcName_Symbol[funcArgs_List,idx_Integer],{anEpsVar_\[Distributed] PerfectForesight}]:=
Apply[funcName,Append[ReplacePart[{funcArgs},{{1,(-1)}->0}],idx]]

myNExpectation[funcName_Symbol[farg_List,idx_Integer],nArgs_List]:=Chop[NExpectation[funcName[farg,idx],nArgs]]


myNewNExpectation[fff_[fargs___],anEpsVar_\[Distributed] PerfectForesight]:=Module[{},Print["there",{(Apply[fff,{fargs}]),{fargs}/.anEpsVar->0}];(Apply[fff,{fargs}])/.anEpsVar->0]


myNewNExpectation[fff_[fargs___],distStuff_]:=Module[{},Print["jhere",{(Apply[fff,{fargs}]),{fargs}}];Chop[NExpectation[Applyp[fff,{fargs}],distStuff]]]



(*end code for myNExpectation*)


(*begin code for getDistribs*)

getDistribs[distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:= Map[Last,expctSpec]
(*{numReg,tranType,tranFunc}*)

(*end code for getDistribs*)


(*begin code for getNumVars*)
getNumVars[gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
(Length[getGridPtTrips[gSpec]]+If[numRegimes>0,2,0])

(*end code for getNumVars*)


(*begin code for getGridPtTrips*)

getGridPtTrips[gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=gSpec[[3]]
  

(*end code for getGridPtTrips*)


getH[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ]:=
theHMat


getB[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ]:=
BB


getF[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ]:=
FF


getPhi[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ]:=
phi


getPsiZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ]:=
psiZ


getPsiC[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ]:=
psiC


getPsiEps[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ]:=
psiEps


getNumZ[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ]:=
Length[getPsiZ[linMod][[1]]]


 genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
  psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
  ,{}]:=
With[{numZ=getNumZ[linMod]},
With[{fCon=ConstantArray[0,{1,numZ,1}]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},theRes]]]


genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ,theZs:{_?MatrixQ..}]:=
With[{fCon=fSumC[phi,FF,psiZ,theZs]},
With[{theRes=genLilXkZkFunc[linMod,fCon]},
theRes]]



genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ,XZFuncs:({_Function,_Integer}),xtGuess_?MatrixQ,drvPairs:({{{aa_Integer,bb_Integer}...},
    eqnFunc:(_Function|_CompiledFunction)}|{{},{}}):{{},{}}]:=
With[{fCon=fSum[linMod,XZFuncs,xtGuess]},
With[{theRes=genLilXkZkFunc[linMod,fCon,drvPairs]},
theRes]]


genLilXkZkFunc[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ, 
 psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ} 
 ,fCon_?MatrixQ,drvPairs:({{{aa_Integer,bb_Integer}...},
   eqnFunc:(_Function|_CompiledFunction)}|{{},{}}):{{},{}}]:=
With[{numXVars=Length[BB],numEpsVars=Length[psiEps[[1]]],
numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=Transpose[{genXtm1Vars[numXVars]}],
epsVars=Transpose[{genEpsVars[numEpsVars]}],
zVars=Transpose[{Reverse[Flatten[genZVars[0,numZVars]]]/.name_[t]->name}]},
With[{xtVals=genXtOfXtm1[linMod,xtm1Vars,epsVars,zVars,fCon]},
With[{xtp1Vals=genXtp1OfXt[linMod,xtVals,fCon]},
With[{fullVec=Join[xtm1Vars,xtVals,xtp1Vals,epsVars]},
With[{(*theDrvs=doImplicitDrv[linMod,fullVec,
zVars,xtm1Vars,epsVars,drvPairs]*)},(*Print["theDrvs",theDrvs];*)
ReplacePart[
Function[xxxx,fullVec],{1->Flatten[Join[xtm1Vars,epsVars,zVars]]}]
]]]]]]



fSumC=Compile[{{phi,_Real,2},{FF,_Real,2},{psiZ,_Real,2},{zPath,_Real,3}},
With[{numXVars=Length[psiZ]},
With[{fPows=Drop[NestList[FF.#&,IdentityMatrix[numXVars],Length[zPath]],-1]},
Apply[Plus,
MapThread[Dot[#1,phi.psiZ.#2]&,{fPows , zPath}]]]]]



fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
        {},
        xtGuess_?MatrixQ]:=
ConstantArray[0,{Length[psiZ],1}]

fSum[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
        {XZFunc_Function,numSteps_Integer},xtGuess_?MatrixQ]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xzRes=Apply[multiStepZ[{XZFunc,numSteps},numXVars,numZVars,numSteps], Flatten[xtGuess]]},
fSumC[phi,FF,psiZ,xzRes]]]


(*begin code for genXtm1Vars*)
genXtm1Vars[numVars_Integer]:=
Module[{},
genXtm1Vars[numVars]=
Table[
makeProtectedSymbol["xxxtm1Var"<>ToString[ii]],{ii,numVars}]]/;And[numVars>=0]

(*end code for genXtm1Vars*)


(*begin code for genXtOfXtm1*)
genXtOfXtm1[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xtm1Vars_?MatrixQ,epsVars_?MatrixQ,zVars_?MatrixQ,
        fCon_?MatrixQ]:=
With[{xtVals=BB.xtm1Vars+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC + phi . psiEps . epsVars+
phi . psiZ . zVars +FF.fCon},xtVals]

(*end code for genXtOfXtm1*)


(*begin code for genXtp1OfXt*)

genXtp1OfXt[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},xtVals_?MatrixQ,
        fCon_?MatrixQ]:=
With[{xtp1Vals=BB.xtVals+Inverse[IdentityMatrix[Length[xtVals]]-FF] . phi . psiC+fCon},xtp1Vals]


(*end code for genXtp1OfXt*)


(*begin code for genX0Z0Funcs*)
genX0Z0Funcs[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ}]:=
With[{numXVars=Length[BB],numZVars=Length[psiZ[[1]]]},
With[{xtm1Vars=genXtm1Vars[numXVars]},
With[{compArgs=xtm1Vars},
Apply[Function, {compArgs,Join[BB.Transpose[{xtm1Vars}]+
Inverse[IdentityMatrix[Length[xtm1Vars]]-FF] . phi . psiC,ConstantArray[0,{numZVars,1}]]}]]]]
(*end code for genX0Z0Funcs*)


(*begin code for genZVars*)
genZVars[horizons_Integer,numConstr_Integer]:=
genZVars[horizons,numConstr,0]
        
genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Module[{},
genZVars[horizons,numConstr,offset]=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]]/;offset<=0


genZVars[numConstr_Integer]:=
Reverse[Flatten[genZVars[0,numConstr]]](*
Module[{},
genZVars[numConstr]=
Table[
makeProtectedSymbol["zzzVar"<>ToString[ii]],{ii,numConstr}]]*)/;And[numConstr>=0]

(*end code for genZVars*)


(*begin code for genXtm1Vars*)
genEpsVars[numShocks_Integer]:=
Module[{},
genEpsVars[numShocks]=
Table[
makeProtectedSymbol["epsVar"<>ToString[ii]],{ii,numShocks}]]/;And[numShocks>=0]
(*end code for genXtm1Vars*)


(*begin code for multiStep*)

multiStep[{XZfunc_Function,numSteps_Integer},numX_Integer,valRange:{_Integer..},numTerms_Integer]:=
With[{funcArgs=XZfunc[[1]]},
With[{xtFunc01=
ReplacePart[
Function[xxxxx,
        Flatten[(Apply[XZfunc, xxxxx])[[Range[numX]]]]],{1->funcArgs}]},
With[{theFunc=
        ReplacePart[
        Function[xxxxx,
 With[{theXVals=NestList[Apply[xtFunc01, Flatten[#]]&,xxxxx,numTerms-1]},(*Print["multiStep:theXVals=",{theXVals,Map[((Apply[XZfunc,Flatten[#]])[[valRange]] )& , theXVals]}];*)
          Map[((Apply[XZfunc,Flatten[#]])[[valRange]] )&, theXVals]]],1->funcArgs]},
With[{xxxxxPos={{2,1,1,2,1,1,1,2,1,1,2},{2,1,1,2,2}}},
ReplacePart[
theFunc,
          {xxxxxPos->funcArgs}]]]]]/;numSteps>0


(*end code for multiStep*)


(*begin code for multiStepZ*)
multiStepZ[{XZfunc_Function,numSteps_Integer},numX_Integer,numZ_Integer,numTerms_Integer]:=
multiStep[{XZfunc,numSteps},numX,numX+Range[numZ],numTerms]

(*end code for multiStepZ*)


(*begin code for multiStepX*)
multiStepX[{XZfunc_Function,numSteps_Integer},numX_Integer,numTerms_Integer]:=
multiStep[{XZfunc,numSteps},numX,Range[numX],numTerms]

(*end code for multiStepX*)


(*begin code for checkLinMod*)

checkLinMod[linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
anX_?MatrixQ,anEps_?MatrixQ,numRegimes_Integer:0]:=
With[{X0Z0=genX0Z0Funcs[linMod],numZ=Length[psiZ[[1]]]},
With[{lilxz=genLilXkZkFunc[linMod, {X0Z0,2}, Join[anX,anEps]]},
        {Eigenvalues[BB]//Abs,Eigenvalues[FF]//Abs,Apply[X0Z0,Flatten[anX]],Apply[lilxz,Flatten[Join[anX,anEps,Table[{0},{numZ}]]]]}]]


(*end code for checkLinMod*)


(*begin code for checkMod*)



checkMod[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},anX_?MatrixQ,anEps_?MatrixQ,ss_?MatrixQ,
eqnsFunc:(_Function|_CompiledFunction)]:=
With[{X0Z0=genX0Z0Funcs[linMod],numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{lilxz=
genLilXkZkFunc[linMod, {X0Z0,1}, Join[anX,anEps]]},
With[{xzFuncNow=theSolver[[1]][{numX,numEps,numZ},lilxz,eqnsFunc,Method->"JenkinsTraub"]},
With[{fp=genFPFunc[theSolver,linMod,{X0Z0,2},eqnsFunc]},
{Apply[lilxz,Flatten[Join[anX,anEps,Table[0,{numZ}]]]],
Apply[xzFuncNow,Flatten[Join[anX,anEps]]],
Apply[fp,Flatten[Join[anX,anEps]]],
Apply[eqnsFunc,Flatten[Join[ss,{{0}}]]]
}]]]]



(*end code for checkMod*)



(*begin code for genFRFunc*)
 
genFRFunc[{numX_Integer,numEps_Integer,numZ_Integer},
xkFunc:(_Function|_CompiledFunction),eqnsFunc:(_Function|_CompiledFunction),opts:OptionsPattern[]]:=
With[{funcArgs=Table[Unique["theFRFuncArgs"],{numX+numEps}],
zArgs=Table[Unique["theFRZArgs"],{numZ}]},
With[{zArgsInit=Map[{#,0}&,zArgs],funcName=Unique["fName"]},
funcName[funcArgsNot:{_?NumberQ..}]:=
Module[{theVars=Join[funcArgsNot]},(*Print["genFRFunc func",theVars,
Flatten[Apply[xkFunc,theVars]]];*)
Apply[eqnsFunc,(Flatten[Apply[xkFunc,theVars]])]];
ReplacePart[
Function[xxxx,With[{zVals=zArgs/.FindRoot[funcName[Join[funcArgs,zArgs]],zArgsInit]},
Join[(Apply[xkFunc,Join[funcArgs,zVals]])[[numX+Range[numX]]],
Transpose[{zVals}]]]],
1->funcArgs]]]
(* input   [function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)
 

(*end code for genFRFunc*)


(*begin code for genFPFunc*)
        
fixedPointLimit=30;
genFPFunc[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),
        linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
XZFuncs:({_Function,_Integer}),eqnsFunc:(_Function|_CompiledFunction)]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{funcArgs=Table[Unique["theFPFuncArgs"],{numX+numEps}]},
ReplacePart[
Function[xxxx,Sow[
myFixedPoint[With[{
        xzFuncNow=theSolver[[1]][{numX,numEps,numZ},genLilXkZkFunc[linMod,XZFuncs,#[[Range[numX]]]],eqnsFunc,{opts}]
},(*Print["infp:",Apply[XZFuncs[[1]],funcArgs]];*)
Apply[xzFuncNow,funcArgs]]&,(Apply[XZFuncs[[1]],funcArgs])[[Range[numX]]],fixedPointLimit]]],
1->funcArgs]]]
(* input   [linMod,XZ, xguess,function (xt,eps,zt)->(xtm1,xt,xtp1,eps), function (xtm1,xt,xtp1,eps)->me]*)
(* output   [function  (xt,eps) ->(xt,zt)] *)


(*end code for genFPFunc*)


(*begin code for myFixedPoint*)

myFixedPoint[firstArg_,secondArg_,thirdArg_]:=
Module[{},
FixedPoint[firstArg,secondArg,thirdArg]]
        

(*end code for myFixedPoint*)


(*begin code for makeInterpFunc*)

makeInterpFunc[aVecFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{interpData=genInterpData[aVecFunc,gSpec],numArgs=getNumVars[gSpec]},
        With[{numFuncs=Length[interpData[[1,2]]],funcArgs=Table[Unique["fArgs"],{numArgs}]},
        With[{longFuncArgs=fillInSymb[{{},toIgnore,funcArgs}]},
                With[{
                interpFuncList=
Map[Function[funcIdx,Interpolation[Map[{#[[1]], #[[2, funcIdx, 1]]} & , 
                interpData],InterpolationOrder -> iOrd]],Range[numFuncs]]},
                With[{applied=Transpose[{Through[Apply[interpFuncList,funcArgs]]}]},
        (*      Print[  Function[xxxxxxx, Transpose[{Through[Apply[interpFuncList,yyyyyyy]]}]]//InputForm];*)
        ReplacePart[
        Function[xxxxxxx, applied],
                {1->longFuncArgs}]
        ]
]]]]




(*end code for makeInterpFunc*)


(*begin code for genInterpData*)

 
genInterpData[aVecFunc:(_Function|P_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0}]:=
With[{thePts=gridPts[getGridPtTrips[gSpec],numRegimes]},
With[{filledPts=ParallelMap[fillIn[{{},toIgnore,#}]&,thePts]},
With[{theVals=ParallelMap[(Apply[aVecFunc,#])&,filledPts]},
With[{interpData=Transpose[{thePts,theVals}]},
interpData]]]]





(*end code for genInterpData*)


(*begin code for gridPts*)
 
gridPts[rngs:{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0]:=
With[{funcForPts=(Function[xx,oneDimGridPts[xx[[1]],xx[[{2,3}]]]][#]) &},
With[{oneDimPts=Map[funcForPts,rngs]},
        With[{maybeRegimes=If[numRegimes==0,oneDimPts,
                Prepend[Append[oneDimPts,Range[0,numRegimes-1]],Range[0,numRegimes-1]]]},
With[{theOuter=Outer[List,Apply[Sequence,#]]&[maybeRegimes]},
Flatten[theOuter,Depth[theOuter]-3]]]]]



(*end code for gridPts*)


(*begin code for oneDimGridPts*)

oneDimGridPts[iPts_Integer,{xLow_?NumberQ,xHigh_?NumberQ}]:=
If[iPts==0,{{(xLow+xHigh)2}},
Table[ii,{ii,xLow,xHigh,N[xHigh-xLow]/iPts}]]/;iPts>=0


(*end code for oneDimGridPts*)


(*begin code for fillIn*)

fillIn[args___]:=Print["wrong args for fillIn",{args}];
fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
        If[MemberQ[toIgnore,Length[theRes]+1],fillIn[{Append[theRes,1],Drop[toIgnore,1],shortVec}],
                fillIn[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]


(*end code for fillIn*)


(*begin code for fillInSymb*)

fillInSymb[{theRes:{___},toIgnore:{_Integer...},shortVec:{___}}]:=
Module[{},
If[toIgnore=={}==shortVec,theRes,
        If[MemberQ[toIgnore,Length[theRes]+1],fillInSymb[{Append[theRes,Unique["ig"]],Drop[toIgnore,1],shortVec}],
                fillInSymb[{Append[theRes,shortVec[[1]]],toIgnore,Drop[shortVec,1]}]]]]/;OrderedQ[toIgnore]

fillInSymb[{theRes:{___},toIgnore:{_Integer...},shortVec:{___}}]:=
fillInSymb[{theRes,Sort[toIgnore],shortVec}]

fillIn[{theRes:{_?NumberQ...},toIgnore:{_Integer...},shortVec:{_?NumberQ...}}]:=
fillIn[{theRes,Sort[toIgnore],shortVec}]


(*end code for fillInSymb*)


(*begin code for doIterREInterp*)
doIterREInterp[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),
        linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
        XZFuncsNow:{(_Function|_InterpolatingFunction|_CompiledFunction),_Integer},
eqnsFunc:(_Function|_CompiledFunction),gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{numX=Length[BB],numEps=Length[psiEps[[1]]],numZ=Length[psiZ[[1]]]},
With[{theFuncs=makeInterpFunc[genFPFunc[theSolver,linMod,XZFuncsNow,eqnsFunc],gSpec]},
{theFuncs,genXZREInterpFunc[{numX,numEps,numZ},theFuncs,gSpec,distribSpec]}]]




(*end code for doIterREInterp*)


(*begin code for nestIterREInterp*)


nestIterREInterp[theSolver:(({genFRFunc,opts:OptionsPattern[]}|{genNSFunc,opts:OptionsPattern[]}|{specialSolver,opts:OptionsPattern[]})),linMod:{theHMat_?MatrixQ,BB_?MatrixQ,phi_?MatrixQ,FF_?MatrixQ,psiEps_?MatrixQ,psiC_?MatrixQ,psiZ_?MatrixQ,psiZPreComp_?MatrixQ},
{XZFuncNow:(_Function|_InterpolatingFunction|_CompiledFunction),numTerms_Integer},eqnsFunc:(_Function|_CompiledFunction),
gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},
distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}},numIters_Integer]:=
NestList[doIterREInterp[theSolver,linMod,{#[[2]],numTerms},eqnsFunc,gSpec,distribSpec]&,{ig,XZFuncNow},numIters]




(*end code for nestIterREInterp*)


(*begin code for genXZREInterpFunc*)
 
genXZREInterpFunc[probDims:{numX_Integer,numEps_Integer,numZ_Integer},
aLilXkZkFunc_Function,gSpec:{toIgnore:{_Integer...},iOrd_Integer,{{_Integer,_?NumberQ,_?NumberQ}..},numRegimes_:0},distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{theFuncNow=genXZFuncRE[{numX,numEps,numZ},aLilXkZkFunc,distribSpec]},
makeInterpFunc[theFuncNow,{toIgnore,gSpec[[2]],Drop[getGridPtTrips[gSpec],-(numEps-If[numRegimes>0,1,0])],numRegimes}]]
  

(*end code for genXZREInterpFunc*)


(*begin code for genXZFuncRE*)

genXZFuncRE[{numX_Integer,ignored_Integer,numZ_Integer},
aLilXkZkFunc_Function,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{intVarRes=genIntVars[numX,distribSpec],
funcName=Unique["fName"],numRegimes=getNumRegimes[distribSpec]},
funcName[fNameArgs:{_?NumberQ..},idx_Integer]:=Module[{},
(Apply[aLilXkZkFunc,fNameArgs])[[idx,1]]];
With[{funcGuts=
Switch[getRegimeTransProbFuncType[distribSpec],
        noTransFunc,Function[xxxx,Module[{},
        Transpose[{Map[myNExpectation[
        (funcName[intVarRes[[2]],#]),intVarRes[[3]]]&,Range[numX+numZ]]}]]],
        transFuncNoShocks,Function[xxxx,Module[{},
        Sum[(Apply[getProbFunc[distribSpec],
                Append[intVarRes[[2]],ii-1]])*
        Transpose[{Map[myNExpectation[
        (funcName[Append[intVarRes[[2]],ii-1],#]),intVarRes[[3]]]&,Range[numX+numZ]]}],{ii,numRegimes}]]],
        transFuncHasShocks,Function[xxxx,Module[{},
        Transpose[{Map[myNExpectation[
        Sum[(Apply[getProbFunc[distribSpec],
                Append[intVarRes[[2]],ii-1]])*(funcName[Append[intVarRes[[2]],ii-1],#]),{ii,numRegimes}],intVarRes[[3]]]&,Range[numX+numZ]]}]]]         
        ]},
        ReplacePart[funcGuts,1->intVarRes[[1]]]]]



(*end code for genXZFuncRE*)


(*begin code for genIntVars*)
 genIntVars[numX_Integer,distribSpec:{expctSpec:{{_Symbol,_}..},regimeTransProbFunc_:{}}]:=
With[{xVars=Table[Unique["xV"],{numX}],
        dists=getDistribs[distribSpec],
        distVars=Table[Unique["epIntV"],{getNumEpsVars[distribSpec]}]},
With[{xEpsVars=If[regimeTransProbFunc=={},
        Join[xVars,distVars],Join[xVars,distVars(*,{Unique["regV"]}*)]],
        intArg=MapThread[#1 \[Distributed] #2&,{distVars,dists}]},
        {xVars,xEpsVars,intArg}]]


(*end code for genIntVars*)


End[]
EndPackage[]


