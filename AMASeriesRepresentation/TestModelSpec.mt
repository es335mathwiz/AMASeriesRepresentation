(* Wolfram Language Test file *)
Get["betterLucaMod.m"]
Test[
	N[lucaEqnFuncs @@Range[10]]
	,
	{2.465, -5.515, 1.}

	,
	TestID->"TestModelSpec-20151203-F6V7T0"
]