(* Wolfram Language package *)
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