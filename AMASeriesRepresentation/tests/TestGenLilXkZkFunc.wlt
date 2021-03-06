(* Mathematica Test File *)
(*reference linear rational expectations model*)
(*taken from simpleRBCModel.m*)
linMod={{{0}},{{0., 0.6926315789473684, 0.34202807765803783}, 
  {0., 0.36, 0.17777143246056068}, {0., 0., 0.9499525011874801}}, 
 {{-0.04442366984873067, 0.658, 0.3600475573573294}, 
  {0.04442366984873067, 0.342, 0.18713718026779125}, {0., 0., 1.}}, 
 {{0.342, 0., -0.12313626461620665}, {-0.342, 0., 0.12313626461620665}, 
  {0., 0., 0.}}, {{0.}, {0.}, {1.0009504513929297}}, 
 {{-0.9988685455324166}, {-0.19718359057668058}, {0.05009757134342517}}, 
 {{1., 0., 0.}, {0., 1., 0.}, {0., 0., 1.}}, {{0}}};
 anXtm1EpsZ={1, .18, 1.1, 0.01, 0.01, -.02, .0001};
 	X0Z0=genX0Z0Funcs[linMod]
BeginTestSection["AutomatedTests"]
VerificationTest[
	X0Z0@@anXtm1EpsZ
	,
	{{0.3891952752266469}, {0.20228690596886514}, {1.0950453226496533}, {0}, {0}, {0}},
	TestID->"TestGenLilXkZkFunc-20151101-J4G6L2"
] 
 
VerificationTest[
	tRes=With[{theFunc=genLilXkZkFunc[linMod, {X0Z0,2},X0Z0@@anXtm1EpsZ]},
		theFunc @@ anXtm1EpsZ];
	Norm[tRes-{{1}, {0.18}, {1.1}, {0.3792309409344926}, {0.19778300683599365}, 
 {1.1051548271635827}, {0.40327544295531254}, {0.20960516943877955}, 
 {1.0999421636068878}, {0.01}}]<10^(-8),
 True
	,
	TestID->"TestGenLilXkZkFunc-20151031-D0D9A9"
]

 (*should produce same result as one X0Z0 since all z's 0*)
VerificationTest[
	tRes=With[{theFunc=genLilXkZkFunc[linMod, {X0Z0,4},X0Z0@@anXtm1EpsZ]},
		theFunc @@ anXtm1EpsZ];
	Norm[tRes-{{1}, {0.18}, {1.1}, {0.3792309409344926}, {0.19778300683599365}, 
 {1.1051548271635827}, {0.40327544295531254}, {0.20960516943877955}, 
 {1.0999421636068878}, {0.01}}]<10^(-8),
 True
	,
	TestID->"TestGenLilXkZkFunc-20151031-D0AAA9"
]
EndTestSection[]


