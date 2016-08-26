(* Wolfram Language Test file *)

TestSuite[
	{"TestRBCPFRE.mt",
		"TestIterDRPF.mt",
		"TestDoIterPF.mt",
		"TestGenFPFunc.mt",
		"TestGenFRFunc.mt",
		"TestGenLilXkZkFunc.mt",(*not okay*)
		"TestGenPath.mt",
		"TestApprox.mt",(*hangs by itself with rbcpfre*)
		"TestRBCCompDoIterPF.mt",(*okay*)
		"TestRBCCompGenFPFunc.mt",
		"TestRBCCompGenFRFunc.mt",
		"TestRBCCompGenLilXkZkFunc.mt",
		"TestRBCCompGenPath.mt",
		"TestRBCCompIterDRPF.mt",(*okay*)
		"TestRBCCompApprox.mt",
				"TestCondExpComposition.mt",
		"TestEmpty.mt",(*,
		"TestRegimes.mt"*)
		"AMASeriesFuncTest.mt"
	}
]