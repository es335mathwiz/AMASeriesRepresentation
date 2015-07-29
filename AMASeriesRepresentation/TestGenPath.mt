(* Mathematica Test File *)
Needs["AMASeriesRepresentation`"]
bmat = {{0., 0.6926315789473684}, {0., 0.36}};
phimat = {{-0.04428506256712977, 0.658}, {0.04428506256712977, 0.342}};
fmat={{0.342, 0.}, {-0.342, 0.}};
psieps={{-2.778973604422438}, {-0.5468770327603131}};
psic={{1.7785431068303603}, {0.3500013009666004}};
psiz={{1., 0.}, {0., 1.}};
xtm1={{ctm1},{ktm1}};
Test[
    genPath[xtm1,bmat,phimat,fmat,psieps,psic,psiz,2,1]
    ,
    {{ctm1}, {ktm1}, {0.2303008560360231 - 0.23677806761203624*eps + 
   0.6926315789473684*ktm1 + 0.658*ProtectedSymbols`zzz$0$1[t] - 0.04428506256712977*ProtectedSymbols`zzz$0$2[t]}, 
 {0.11970044493057731 - 0.31009896514827684*eps + 0.36*ktm1 + 0.342*ProtectedSymbols`zzz$0$1[t] + 
   0.04428506256712977*ProtectedSymbols`zzz$0$2[t]}}
    ,
    TestID->"TestGenPath-20150728-A6Z5D2"
]
Test[
	genPath::usage
	,
	"genPath[xtm1_?MatrixQ,bmat_?MatrixQ,phimat_?MatrixQ,fmat_?MatrixQ,psieps_?MatrixQ,psic_?MatrixQ,psiz_?MatrixQ,numCon_Integer,\nnumNonZeroZs_Integer,padZeroZs_Integer]"
	,
	TestID->"TestGenPath-20150728-O0L4E3"
]