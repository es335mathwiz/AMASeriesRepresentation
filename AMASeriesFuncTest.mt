(* Wolfram Language Test file *)

Test[
	anX0Z0 = genX0Z0Funcs[linMod];
lilxkzk = genLilXkZkFunc[linMod, {anX0Z0, 3}, anX0Z0 @@ anXEpsFlat];
lilxkzk @@ anXEpsZsFlat
	,
	{{0.2}, {0.18}, {1.1}, {0.62797107309957}, {0.33314357748469003}, {1.4050548271635828}, 
 {0.5996046692205815}, {0.3116486274672324}, {1.384832918713013}, {0.01}}
	,
	TestID->"AMASeriesFuncTest-20160621-U2T2N5"
]

Test[
	anX0Z0 = genX0Z0Funcs[linModBetterQuasi];
lilxkzk = 
 genLilXkZkFunc[linModBetterQuasi, {anX0Z0, 2}, 
  anX0Z0 @@ anXFlatBetterQuasi, {{{1, 2}}, 
   rbcEqnsFunctionBetterQuasi}];
   lilxkzk  @@ anXEpsZsFlatBetterQuasi
	,
	{{0.2}, {0.18}, {0}, {1.}, {1.1}, {0.5181368277192597}, {0.2760564773022188}, {0.5}, 
 {1.8603422701233614}, {1.5050548271635829}, {0.6033499453621338}, {0.31359526035539487}, 
 {0.}, {0.9043206867400233}, {1.4798281688317612}, {0.01}}
	,
	TestID->"AMASeriesFuncTest-20160621-J1G9K7"
]

Test[
	anX0Z0 = genX0Z0Funcs[linModBetter];
lilxkzk = 
  genLilXkZkFunc[linModBetter, {anX0Z0, 3}, 
   anX0Z0 @@ anXEpsFlatBetter];
   lilxkzk @@ anXEpsZsFlatBetter
	,
	{{0.2}, {0.18}, {1.}, {1.1}, {0.5181368277192597}, {0.2760564773022188}, 
 {1.8603422701233614}, {1.5050548271635829}, {0.6033499453621338}, {0.31359526035539487}, 
 {0.9043206867400233}, {1.4798281688317612}, {0.01}}
	,
	TestID->"AMASeriesFuncTest-20160621-B9S8K5"
]

Test[
	anX0Z0 = genX0Z0Funcs[linModBetterCnstrn];
lilxkzk = 
  genLilXkZkFunc[linModBetterCnstrn, {anX0Z0, 3}, 
   anX0Z0 @@ anXEpsFlatBetterCnstrn];
   lilxkzk @@ anXEpsZsFlatBetterCnstrn
	,
	{{0.2}, {0}, {0.18}, {0}, {1.}, {1.1}, {0.7279591594625625}, {0.09764217561585392}, 
 {0.059642175615853915}, {5.338332545161876}, {0.2450043725439719}, {1.5050548271635829}, 
 {0.3862368039977073}, {0.134311903685849}, {0.1879898617401175}, {0.}, 
 {2.5757876006504588}, {1.4798281688317612}, {0.01}}
	,
	TestID->"AMASeriesFuncTest-20160621-M8H1J2"
]

Test[
	test
	,
	result
	,
	TestID->"AMASeriesFuncTest-20160621-C9T5Y6"
]