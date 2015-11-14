(* Wolfram Language Test file *)
Switch[$System,
 "Mac OS X x86 (64-bit)", 
 SetDirectory[
  "/Users/garyanderson/git/ProjectionMethodTools/\
ProjectionMethodToolsJava/code"],
 "Linux x86 (64-bit)", 
 SetDirectory[
  "~/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"],
 "Microsoft Windows (64-bit)", 
 SetDirectory[
  "g:/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"]]; 
$ContextPath=DeleteCases[$ContextPath,"simpleRBCModel`"] 

 Needs["betterRBC`"]


X0Z0=genX0Z0Funcs[linMod];
anXEps={1,.2,1,1.1,0.01}   


thVal=(theta//.ssSolnSubsRE//.(simpParamSubs//N))//N;
kVal = (kk /. Private`kSSSubRE//.(simpParamSubs//N))//N;
cVal = (cc /. Private`cSSSubRE//.(simpParamSubs//N))//N ;
kLow = 1/10*kVal//N;
kHigh = 4*kVal//N;
sigVal = sigma //. (simpParamSubs//N);
sigLow = -3*sigVal;
sigHigh = 3*sigVal;
thLow = 9/10;
thHigh = 11/10;
pdf=NormalDistribution[0, sigVal];


aGSpec={1,{{4,kLow,kHigh},{3,thLow,thHigh},{3,sigLow,3*sigHigh}}};
theDist = {{{ee, NormalDistribution[0, 0.01]}}};

 {x1z1pf, X1Z1PF} = 
  doIterPF[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
   rbcEqnsCompiled];
   {x1z1re, X1Z1RE} = 
  doIterRE[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
   rbcEqnsCompiled, {{{ee, PerfectForesight}}}];
Test[
	Through[X1Z1PF @@ # &@anXEps]==Through[X1Z1RE @@ # &@anXEps]
	,
	True
	,
	TestID->"TestRBCPFRE-20151114-U5V8A1"
]
couplePF = 
 nestIterPF[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
  rbcEqnsCompiled, 2];
coupleRE = 
 nestIterRE[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
  rbcEqnsCompiled, {{{ee, PerfectForesight}}}, 2];
Test[
	Through[coupleRE[[-1, 2]] @@ # &@{1, .18, 1, 1}]==Through[couplePF[[-1, 2]] @@ # &@{1, .18, 1, 1}]
	,
	True
	,
	TestID->"TestRBCPFRE-20151114-O2Q3I3"
]

{x1z1pfInterp, X1Z1PFInterp} = 
  doIterPFInterp[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
   rbcEqnsCompiled, {1, 3}, aGSpec];
{x1z1reInterp, X1Z1REInterp} = 
  doIterREInterp[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
   rbcEqnsCompiled, {1, 3}, aGSpec, {{{ee, PerfectForesight}}}];

Test[
	{x1z1pfInterp @@ anXEps, Through[X1Z1PFInterp @@ # &@anXEps]}=={x1z1reInterp @@ anXEps, Through[X1Z1REInterp @@ # &@anXEps]}
	,
	True
	,
	TestID->"TestRBCPFRE-20151114-C7B0D9"
]

couplePFInterp = 
    nestIterPFInterp[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
     rbcEqnsCompiled, {1, 3}, aGSpec, 3];
coupleREInterp = 
 nestIterREInterp[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
  rbcEqnsCompiled, {1, 3}, aGSpec, {{{ee, PerfectForesight}}}, 3];
  
  Test[
	Through[couplePFInterp[[-1, 2]] @@ # &@{1, .18, 1, 1}]==Through[coupleREInterp[[-1, 2]] @@ # &@{1, .18, 1, 1}]
	,
	True
	,
	TestID->"TestRBCPFRE-20151114-H4S0Q7"
]