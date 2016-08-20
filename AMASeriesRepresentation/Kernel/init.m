(* blown away *)
Print["reading init.m"]
Switch[$System,
  "Mac OS X x86 (64-bit)",
(Print["mac code from init.m in Kernal dir"];
PrependTo[$Path,"/Users/garyanderson/git/mathSmolyak"];
PrependTo[$Path,"/Users/garyanderson/git/ProtectedSymbolsDir/"]),
_,
(Print["windows code"];
PrependTo[$Path,"g:/git/mathSmolyak"];
PrependTo[$Path,"g:/git/ProtectedSymbolsDir/"])]
Get[ "AMASeriesRepresentation`AMASeriesRepresentation`"]