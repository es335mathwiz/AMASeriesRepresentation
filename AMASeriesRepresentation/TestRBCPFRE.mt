(* Wolfram Language Test file *)
Get["prepBetter.m"]

 {x1z1pf, X1Z1PF} = 
  doIterPF[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
   rbcEqnsCompiled];
   {x1z1re, X1Z1RE} = 
  doIterRE[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
   rbcEqnsCompiled, {{{ee, PerfectForesight}}}];
   Print[{Through[X1Z1PF @@ # &@anXEps]-Through[X1Z1RE @@ # &@anXEps]}]
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
   rbcEqnsCompiled,  aGSpec];
{x1z1reInterp, X1Z1REInterp} = 
  doIterREInterp[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
   rbcEqnsCompiled,  aGSpec, {{{ee, PerfectForesight}}}];

Test[
	{x1z1pfInterp @@ anXEps, Through[X1Z1PFInterp @@ # &@anXEps]}=={x1z1reInterp @@ anXEps, Through[X1Z1REInterp @@ # &@anXEps]}
	,
	True
	,
	TestID->"TestRBCPFRE-20151114-C7B0D9"
]

couplePFInterp = 
    nestIterPFInterp[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
     rbcEqnsCompiled,  aGSpec, 3];
coupleREInterp = 
 nestIterREInterp[linMod, {X0Z0}, X0Z0[1, .18, 1, 1][[Range[4]]], 
  rbcEqnsCompiled,  aGSpec, {{{ee, PerfectForesight}}}, 3];
  
  Test[
	Through[couplePFInterp[[-1, 2]] @@ # &@{1, .18, 1, 1}]==Through[coupleREInterp[[-1, 2]] @@ # &@{1, .18, 1, 1}]
	,
	True
	,
	TestID->"TestRBCPFRE-20151114-H4S0Q7"
]