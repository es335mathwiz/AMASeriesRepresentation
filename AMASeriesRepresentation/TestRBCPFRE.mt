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
 anXEps={1,.2,1,1.1,0.01}   
 
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