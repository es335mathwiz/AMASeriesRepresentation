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
	checkMod[{genFRFunc}, linMod, aGSpec, theDist, anX, {{0.7}}, \
rbcEqnsFunctionalNext]
	,
	{{{0.2}, {0.18}, {1.1}, {0.6414681107684652}, {0.33340743751187707}, 
  {1.7957106386247041}, {0.7334026832278214}, {0.381191060279506}, {1.75593738391393}, 
  {0.7}}, {{0.7545568087130929}, {0.4345675897759441}, {2.204596878877305}, 
  {0.6277519526786742}, {-0.009487459882446774}, {0.408886240252601}}, 
 {{0.7545568087130929}, {0.4345675897759441}, {2.204596878877305}, {0.6277519526786742}, 
  {-0.009487459882446774}, {0.408886240252601}}}
	,
	TestID->"AMASeriesFuncTest-20160621-C9T5Y6"
]
Test[
	anX0Z0 = genX0Z0Funcs[linMod];
lilxkzk = genLilXkZkFunc[linMod, {anX0Z0, 1}, anX0Z0 @@ anXEpsFlat];
frf = genFRFunc[{3, 1, 3}, lilxkzk, rbcEqnsFunctionalNext];
frf @@ anXEpsFlat
	,
	{{0.39218871219400964}, {0.2042476292306595}, {1.1057730363825737}, 
 {0.005996813507973366}, {-0.0009158809951081547}, {0.0007182092189910172}}
	,
	TestID->"AMASeriesFuncTest-20160621-K3E3O9"
]

Test[
	anX0Z0 = genX0Z0Funcs[linModBetter];
lilxkzk = 
  genLilXkZkFunc[linModBetter, {anX0Z0, 3}, 
   anX0Z0 @@ anXEpsFlatBetter];
frf = genFRFunc[{4, 1, 4}, lilxkzk, rbcEqnsFunctionalNextBetter];
frf @@ anXEpsFlatBetter 
	,
	{{0.40721295294106075}, {0.18922338848360828}, {2.715466265000223}, {1.1057730363825737}, 
 {-0.3322066892240393}, {0.0022430364031905555}, {0.3011656140559672}, 
 {0.0007182092189910172}}
	,
	TestID->"AMASeriesFuncTest-20160621-U8Y1U5"
]
Test[
	anX0Z0 = genX0Z0Funcs[linModBetterCnstrn];
lilxkzk = 
  genLilXkZkFunc[linModBetterCnstrn, {anX0Z0, 1}, 
   anX0Z0 @@ anXEpsFlatBetterCnstrn];
   nsf = genNSFunc[{6, 1, 6}, lilxkzk, 
   rbcEqnsFunctionalNextBetterCnstrn];
   nsf @@ anXEpsFlatBetterCnstrn
	,
	{{0.5246836762149909}, {0.06863878145085739}, {0.2306387814508574}, {0.}, 
 {1.9059102566595694}, {1.1057730363825737}, {-0.34087189313676974}, 
 {0.16772112258743177}, {0.3959695448307887}, {0.0007182092189910172}, {0}, {0}}
	,
	TestID->"AMASeriesFuncTest-20160621-C0M5I1"
]


Test[
anX0Z0 = genX0Z0Funcs[linMod];
lilxkzk = genLilXkZkFunc[linMod, {anX0Z0, 2}, anX0Z0 @@ anXEpsFlat];
fpf = genFPFunc[{genFRFunc}, linMod, {anX0Z0, 2}, 
   rbcEqnsFunctionalNext];
fpf @@ anXEpsFlat
	,
	{{0.39218871219400964}, {0.2042476292306595}, {1.1057730363825737}, 
 {0.005996813507973366}, {-0.0009158809951081547}, {0.0007182092189910172}}
	,
	TestID->"AMASeriesFuncTest-20160621-V8G0P3"
]


Test[
	{nxtxz, nxtXZ} = 
  doIterREInterp[{genFRFunc}, linMod, {genX0Z0Funcs[linMod], 2}, 
   rbcEqnsFunctionalNext, aGSpec, theDist];
thisOne = genXZFuncREInterp[{3, 1, 3}, nxtxz, aGSpec, theDist];
Through[{thisOne, nxtXZ} @@ # &[anXEpsFlat]]
	,
	{{{0.3777696614582756}, {0.19582662521827646}, {1.0949703972809617}, 
  {-0.007727970837921153}, {-0.01784489650075613}, {-0.00007492535901258141}}, 
 {{0.3777696614582756}, {0.19582662521827646}, {1.0949703972809617}, 
  {-0.007727970837921153}, {-0.01784489650075613}, {-0.00007492535901258141}}}
	,
	TestID->"AMASeriesFuncTest-20160621-I0T8O3"
]
Test[
	{nxtxz, nxtXZ} = 
 doIterREInterp[{genFRFunc}, linModBetter, {anX0Z0, 1}, 
  rbcEqnsFunctionalNextBetter, aGSpecBetter, 
  theDistBetter]; fSum[linModBetter, {nxtXZ, 1}, anXBetter]
	,
	{{0.00015821195377165032}, {-0.020755230293868273}, {0.4677059737143272}, 
{-0.00007492535901259132}}
	,
	TestID->"AMASeriesFuncTest-20160621-Z5D5P3"
]

Test[
	{nxtxz, nxtXZ} = 
  doIterREInterp[{genNSFunc}, linModBetterCnstrn, {anX0Z0, 1}, 
   rbcEqnsFunctionalNextBetterCnstrn, aGSpecBetterCnstrn, 
   theDistBetterCnstrn];
   fSum[linModBetterCnstrn, {nxtXZ, 1}, anXBetterCnstrn]
	,
	{{0.13327212609663572}, {0.03427033593413339}, {0.03427033593413339}, {0.}, 
{-0.6314356203014647}, {-0.00007492535901258141}}
	,
	TestID->"AMASeriesFuncTest-20160621-Q7B3D4"
]