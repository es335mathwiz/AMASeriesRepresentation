(* blown away *)
Print["reading init.m"]
Switch[$System,
  "Mac OS X x86 (64-bit)",
(Print["mac code"];
PrependTo[$Path,"/Users/garyanderson/git/mathSmolyak"];
PrependTo[$Path,"/Users/garyanderson/git/ProtectedSymbolsDir/"]),
_,
(Print["windows code"];
PrependTo[$Path,"g:/git/mathSmolyak"];
PrependTo[$Path,"g:/git/ProtectedSymbolsDir/"])]