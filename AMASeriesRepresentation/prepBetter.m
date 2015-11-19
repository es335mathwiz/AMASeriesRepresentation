(* Wolfram Language package *)
Print["reading prepBetter.m"]
Switch[$System,
 "Mac OS X x86 (64-bit)", 
 SetDirectory[
  "/Users/garyanderson/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"],
 "Linux x86 (64-bit)", 
 SetDirectory[
  "~/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"],
 "Microsoft Windows (64-bit)", 
 SetDirectory[
  "g:/git/ProjectionMethodTools/ProjectionMethodToolsJava/code"]]; 
$ContextPath=DeleteCases[$ContextPath,"simpleRBCModel`"] 
Needs["betterRBC`"]
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

Print["done reading prepBetter.m"]