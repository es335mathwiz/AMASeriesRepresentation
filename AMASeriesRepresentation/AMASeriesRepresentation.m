(* Mathematica Package *)

(* Created by the Wolfram Workbench Jul 27, 2015 *)
Print["reading AMASeriesRepresentation`"]
BeginPackage["AMASeriesRepresentation`", {"JLink`","ProtectedSymbols`"}]
(* Exported symbols added here with SymbolName::usage *) 
genZVars::usage="genZVars[horizons_Integer,numConstr_Integer,offset_Integer]"
genPath::usage="genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,
numNonZeroZs_Integer,padZeroZs_Integer]"
genZVars::usage="genZVars[horizons_Integer,numConstr_Integer,offset_Integer]"
Begin["`Private`"]
(* Implementation of the package *)
Print["changing MatrixPower to produce Identity Matrix for singular matrices raised to 0th power"]
Unprotect[MatrixPower]
MatrixPower[xx_?MatrixQ,0]:=IdentityMatrix[Length[xx]]/;
Length[xx]===Length[xx[[1]]]
Protect[MatrixPower]

genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,
	psic_?MatrixQ,psiz_?MatrixQ,numNonZeroZs_Integer,padZeroZs_Integer]:=
genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,
numNonZeroZs,padZeroZs]=
With[{startPath=
genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,numNonZeroZs]},
With[{tailPath=NestList[((nonFPart[#,
{{0}},bmat,phimat,fmat,psieps,psic]))&,
startPath[[-Reverse[Range[Length[bmat]]]]],padZeroZs]},
Join[startPath,Join@@Drop[tailPath,1]]]]

Print["genPath assumes only one shock"]
genPath[xtm1_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,
psic_?MatrixQ,psiz_?MatrixQ,
numNonZeroZs_Integer]:=
genPath[xtm1,
bmat,phimat,fmat,psieps,psic,psiz,
numNonZeroZs]=
With[{numCon=Length[psiz[[1]]]},
With[{rawFParts=Reverse[(doFPart[phimat,fmat,psiz,#,numCon,0] &/@Range[0,numNonZeroZs-1])]},
With[{bgn=(nonFPart[xtm1,
{{ProtectedSymbols`eps}},bmat,phimat,fmat,psieps,psic]+rawFParts[[1]])},
Join[xtm1,Join @@ FoldList[(nonFPart[#1,{{0}},bmat,phimat,fmat,psieps,psic]+#2)&,bgn,Drop[rawFParts,1]]]]]]


oldnonFPart[xtm1_?MatrixQ,epsilon_?MatrixQ,
bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psimat_?MatrixQ,psic_?MatrixQ]:=
bmat . xtm1 + phimat . psimat . epsilon + 
Inverse[IdentityMatrix[Length[xtm1]]-fmat] . phimat . psic

nonFPart=Compile[{{xtm1,_Real,2},{epsilon,_Real,2},
{bmat,_Real,2},{phimat,_Real,2},{fmat,_Real,2},{psimat,_Real,2},{psic,_Real,2}},
bmat . xtm1 + phimat . psimat . epsilon + 
Inverse[IdentityMatrix[Length[xtm1]]-fmat] . phimat . psic]
	
	
doFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer]:=
doFPart[phimat,fmat,psiz,horizon,numCon,0]


doFPart[phimat_?MatrixQ,fmat_?MatrixQ,psiz_?MatrixQ,
horizon_Integer,numCon_Integer,offset_Integer]:=
With[{zMats=genZVars[horizon,numCon,offset]},
Plus @@ MapIndexed[ MatrixPower[fmat,(#2[[1]]-1)] . phimat. psiz . #1&,
Reverse[zMats]]]



genZVars[horizons_Integer,numConstr_Integer]:=
genZVars[horizons,numConstr,0]

genZVars[horizons_Integer,numConstr_Integer,offset_Integer]:=
Table[
{makeProtectedSymbol["zzz$"<>ToString[forTime]<>"$"<>ToString[ii]][ProtectedSymbols`t]},
{forTime,0-offset,horizons},{ii,numConstr,1,-1}]/;offset<=0

Print[$ContextPath]
End[]

EndPackage[]
Print["done reading AMASeriesRepresentation`"]
Print[$ContextPath]