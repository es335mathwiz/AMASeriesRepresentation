parallelNestGenericIterREInterp[genFRExtFunc, 
         {{{0., 0., 0., 0., -7.69859854362188, 9.47963104880131, 0., 0., 0., 
            0., -0.9990004998331669, 0.}, {0., -1.0526315789473686, 0., 0., 
            1., 1., 0., -0.5471847376251848, 0., 0., 0., 0.}, 
           {0., 0., 0., 0., 7.706300992749799, 0., 1., -2.7746348487002535, 
            0., 0., 0., 0.}, {0., 0., 0., -0.9499525011874703, 0., 0., 0., 
            1., 0., 0., 0., 0.}}, {{0., 0.6926315789473685, 0., 
            0.34202807765807436}, {0., 0.36, 0., 0.1777714324605796}, 
           {0., -5.337627424451967, 0., 8.038638938760011*^-17}, 
           {0., 0., 0., 0.9499525011874703}}, {{-0.0444236698487596, 0.658, 
            8.853175874922092*^-18, 0.36004755735737165}, 
           {0.0444236698487596, 0.3419999999999999, -1.6498300305950237*^-18, 
            0.18713718026781317}, {0.3423421710570854, -5.070746053229368, 
            0.9999999999999999, 9.468556211538087*^-17}, {0., 0., 0., 1.}}, 
          {{0., 0., -0.044379268383334426, 0.}, {0., 0., 
            0.044379268383334426, 0.}, {0., 0., 0.34199999999999986, 0.}, 
           {0., 0., 0., 0.}}, {{0.}, {0.}, {0.}, {1.000950451393128}}, 
          {{-3.7735033942323444}, {-0.19718359057674473}, 
          {2.7774108713295114}, {0.050097571343445384}}, {{1., 0., 0., 0.}, 
          {0., 1., 0., 0.}, {0., 0., 1., 0.}, {0., 0., 0., 1.}}, {}}, 
         {{{{-0.1117092944077538 + 0.6926315789473685*#2 + 
               0.34202807765807436*#4}, {-0.058061669737768704 + 0.36*#2 + 
               0.1777714324605796*#4}, {3.777278785008135 - 5.337627424451967*
                #2 + 8.038638938760011*^-17*#4}, {0.050097571343445384 + 
               0.9499525011874703*#4 + 1.000950451393128*#5}, {0}, {0}, {0}, 
             {0}} & , {{-0.1117092944077538 + 0.6926315789473685*#2 + 
               0.34202807765807436*#4}, {-0.058061669737768704 + 0.36*#2 + 
               0.1777714324605796*#4}, {3.777278785008135 - 5.337627424451967*
                #2 + 8.038638938760011*^-17*#4}, {0.050097571343445384 + 
               0.9499525011874703*#4}, {0}, {0}, {0}, {0}} & }, 2}, 
         {{{True & , CompiledFunction[{11, 12., 4446}, {_Real, _Real, _Real, 
              _Real, _Real, _Real, _Real, _Real, _Real, _Real, _Real, _Real, 
              _Real}, {{3, 0, 0}, {3, 0, 1}, {3, 0, 2}, {3, 0, 3}, {3, 0, 4}, 
              {3, 0, 5}, {3, 0, 6}, {3, 0, 7}, {3, 0, 8}, {3, 0, 9}, 
              {3, 0, 10}, {3, 0, 11}, {3, 0, 12}, {3, 1, 0}}, 
             {{0.95, {3, 0, 23}}, {-1., {3, 0, 18}}, {0.36, {3, 0, 19}}, 
              {-0.64, {3, 0, 15}}, {-0.34199999999999997, {3, 0, 14}}, 
              {2.718281828459045, {3, 0, 22}}}, {0, 0, 25, 0, 1}, 
             {{40, 60, 3, 0, 4, 3, 0, 13}, {41, 263, 3, 0, 5, 3, 0, 15, 3, 0, 
               16}, {16, 14, 16, 10, 17}, {13, 13, 17, 16}, {41, 263, 3, 0, 
               1, 3, 0, 19, 3, 0, 17}, {16, 18, 17, 7, 20}, {13, 4, 5, 20, 
               17}, {16, 13, 7, 20}, {19, 20, 21}, {13, 6, 21, 20}, 
              {41, 263, 3, 0, 22, 3, 0, 12, 3, 0, 21}, {41, 263, 3, 0, 3, 3, 
               0, 23, 3, 0, 24}, {16, 21, 24, 21}, {19, 21, 24}, 
              {13, 7, 24, 21}, {34, 1, 4, 16, 17, 20, 21, 3, 0}, {1}}, 
             Function[{betterRBCTrips`Private`cctm1, 
               betterRBCTrips`Private`kktm1, betterRBCTrips`Private`nltm1, 
               betterRBCTrips`Private`thetatm1, betterRBCTrips`Private`cct, 
               betterRBCTrips`Private`kkt, betterRBCTrips`Private`nlt, 
               betterRBCTrips`Private`thetat, betterRBCTrips`Private`cctp1, 
               betterRBCTrips`Private`kktp1, betterRBCTrips`Private`nltp1, 
               betterRBCTrips`Private`thetatp1, 
               betterRBCTrips`Private`epsVal}, Block[{Compile`$1}, 
               Compile`$1 = betterRBCTrips`Private`cct^(-1); 
                {Compile`$1 - (0.34199999999999997*
                    betterRBCTrips`Private`nltp1)/betterRBCTrips`Private`kkt^
                    0.64, betterRBCTrips`Private`cct + 
                  betterRBCTrips`Private`kkt - 1.*
                   betterRBCTrips`Private`kktm1^0.36*
                   betterRBCTrips`Private`thetat, 
                 betterRBCTrips`Private`nlt - Compile`$1*
                   betterRBCTrips`Private`thetat, 
                 betterRBCTrips`Private`thetat - 2.718281828459045^
                    betterRBCTrips`Private`epsVal*
                   betterRBCTrips`Private`thetatm1^0.95}]], $Failed & ], 
            True & }, {True & , CompiledFunction[{11, 12., 4446}, 
             {_Real, _Real, _Real, _Real, _Real, _Real, _Real, _Real, _Real, 
              _Real, _Real, _Real, _Real}, {{3, 0, 0}, {3, 0, 1}, {3, 0, 2}, 
              {3, 0, 3}, {3, 0, 4}, {3, 0, 5}, {3, 0, 6}, {3, 0, 7}, 
              {3, 0, 8}, {3, 0, 9}, {3, 0, 10}, {3, 0, 11}, {3, 0, 12}, 
              {3, 1, 0}}, {{0.95, {3, 0, 23}}, {-1., {3, 0, 18}}, 
              {0.36, {3, 0, 19}}, {-0.64, {3, 0, 15}}, {-0.34199999999999997, 
               {3, 0, 14}}, {2.718281828459045, {3, 0, 22}}}, 
             {0, 0, 25, 0, 1}, {{40, 60, 3, 0, 4, 3, 0, 13}, 
              {41, 263, 3, 0, 5, 3, 0, 15, 3, 0, 16}, {16, 14, 16, 10, 17}, 
              {13, 13, 17, 16}, {41, 263, 3, 0, 1, 3, 0, 19, 3, 0, 17}, 
              {16, 18, 17, 7, 20}, {13, 4, 5, 20, 17}, {16, 13, 7, 20}, 
              {19, 20, 21}, {13, 6, 21, 20}, {41, 263, 3, 0, 22, 3, 0, 12, 3, 
               0, 21}, {41, 263, 3, 0, 3, 3, 0, 23, 3, 0, 24}, 
              {16, 21, 24, 21}, {19, 21, 24}, {13, 7, 24, 21}, 
              {34, 1, 4, 16, 17, 20, 21, 3, 0}, {1}}, Function[
              {betterRBCTrips`Private`cctm1, betterRBCTrips`Private`kktm1, 
               betterRBCTrips`Private`nltm1, betterRBCTrips`Private`thetatm1, 
               betterRBCTrips`Private`cct, betterRBCTrips`Private`kkt, 
               betterRBCTrips`Private`nlt, betterRBCTrips`Private`thetat, 
               betterRBCTrips`Private`cctp1, betterRBCTrips`Private`kktp1, 
               betterRBCTrips`Private`nltp1, betterRBCTrips`Private`thetatp1, 
               betterRBCTrips`Private`epsVal}, Block[{Compile`$2}, 
               Compile`$2 = betterRBCTrips`Private`cct^(-1); 
                {Compile`$2 - (0.34199999999999997*
                    betterRBCTrips`Private`nltp1)/betterRBCTrips`Private`kkt^
                    0.64, betterRBCTrips`Private`cct + 
                  betterRBCTrips`Private`kkt - 1.*
                   betterRBCTrips`Private`kktm1^0.36*
                   betterRBCTrips`Private`thetat, 
                 betterRBCTrips`Private`nlt - Compile`$2*
                   betterRBCTrips`Private`thetat, 
                 betterRBCTrips`Private`thetat - 2.718281828459045^
                    betterRBCTrips`Private`epsVal*
                   betterRBCTrips`Private`thetatm1^0.95}]], $Failed & ], 
            True & }}, Function[{betterRBCTrips`Private`aPt, 
            betterRBCTrips`Private`allRes}, 
           If[betterRBCTrips`Private`allRes[[1]] === $Failed && 
              betterRBCTrips`Private`allRes[[2]] === $Failed, 
             Throw[$Failed, "noSolutionFound"]]; 
            If[betterRBCTrips`Private`allRes[[1]] === $Failed, 
             Flatten[betterRBCTrips`Private`allRes[[2]]], 
             Flatten[betterRBCTrips`Private`allRes[[1]]]]]}, 
         {{1, 3}, {{-2.2495456270812735, 3.4153573162534716}, 
           {-0.24110690297902626, 0.28582091011786126}, {-3, 3}}, 
          {{0.18470046259786932, 0.992290616792759, 0.}, 
           {0.18470046259786932, 0.992290616792759, -0.03}, 
           {0.18470046259786932, 0.992290616792759, 0.03}, 
           {0.1860983282348486, 0.9873476727681342, 0.}, 
           {0.18330259696089005, 0.9972335608173837, 0.}, 
           {0.19972865614637397, 1.0454312889263189, 0.}, 
           {0.16967226904936464, 0.939149944659199, 0.}}, 
          {{1, 0, -1, 0, -1, 0, -1}, {1, -1, 1, 0, -1, 0, -1}, 
           {1, 1, 1, 0, -1, 0, -1}, {1, 0, -1, -1, 1, 0, -1}, 
           {1, 0, -1, 1, 1, 0, -1}, {1, 0, -1, 0, -1, -1, 1}, 
           {1, 0, -1, 0, -1, 1, 1}}, {1, xx[3], -1 + 2*xx[3]^2, xx[2], 
           -1 + 2*xx[2]^2, xx[1], -1 + 2*xx[1]^2}, 
          {1, 0, -0.9999777777777777, -0.0848579369459348 + 
            3.7955863218635133*xx[2], -0.9855982610745595 - 
            1.2883424990941863*xx[2] + 28.812951053434787*xx[2]^2, 
           -0.20579552744921747 + 0.3530510619521161*xx[1], 
           -0.9152964017637968 - 0.29062531804376834*xx[1] + 
            0.24929010469103385*xx[1]^2}, 1, {1, 1, 1}, 
          {{0.18791181760987444, 1.002807281411122, 0}, 
           {0.0075034207458997216, 0.026532584934513576, 0.01}, 
           {-2.2495456270812735, -0.24110690297902626, -3}, 
           {3.4153573162534716, 0.28582091011786126, 3}, 
           {{-0.7071067811865475, -0.7071067811865478, 0}, 
            {-0.7071067811865478, 0.7071067811865475, 0}, {0, 0, 1}}}}, 
         smolyakInterpolation, {}]




genFRExtFunc[{4,1,4},
         {{{0., 0., 0., 0., -7.69859854362188, 9.47963104880131, 0., 0., 0., 
            0., -0.9990004998331669, 0.}, {0., -1.0526315789473686, 0., 0., 
            1., 1., 0., -0.5471847376251848, 0., 0., 0., 0.}, 
           {0., 0., 0., 0., 7.706300992749799, 0., 1., -2.7746348487002535, 
            0., 0., 0., 0.}, {0., 0., 0., -0.9499525011874703, 0., 0., 0., 
            1., 0., 0., 0., 0.}}, {{0., 0.6926315789473685, 0., 
            0.34202807765807436}, {0., 0.36, 0., 0.1777714324605796}, 
           {0., -5.337627424451967, 0., 8.038638938760011*^-17}, 
           {0., 0., 0., 0.9499525011874703}}, {{-0.0444236698487596, 0.658, 
            8.853175874922092*^-18, 0.36004755735737165}, 
           {0.0444236698487596, 0.3419999999999999, -1.6498300305950237*^-18, 
            0.18713718026781317}, {0.3423421710570854, -5.070746053229368, 
            0.9999999999999999, 9.468556211538087*^-17}, {0., 0., 0., 1.}}, 
          {{0., 0., -0.044379268383334426, 0.}, {0., 0., 
            0.044379268383334426, 0.}, {0., 0., 0.34199999999999986, 0.}, 
           {0., 0., 0., 0.}}, {{0.}, {0.}, {0.}, {1.000950451393128}}, 
          {{-3.7735033942323444}, {-0.19718359057674473}, 
          {2.7774108713295114}, {0.050097571343445384}}, {{1., 0., 0., 0.}, 
          {0., 1., 0., 0.}, {0., 0., 1., 0.}, {0., 0., 0., 1.}}, {}}, 
         {{{{-0.1117092944077538 + 0.6926315789473685*#2 + 
               0.34202807765807436*#4}, {-0.058061669737768704 + 0.36*#2 + 
               0.1777714324605796*#4}, {3.777278785008135 - 5.337627424451967*
                #2 + 8.038638938760011*^-17*#4}, {0.050097571343445384 + 
               0.9499525011874703*#4 + 1.000950451393128*#5}, {0}, {0}, {0}, 
             {0}} & , {{-0.1117092944077538 + 0.6926315789473685*#2 + 
               0.34202807765807436*#4}, {-0.058061669737768704 + 0.36*#2 + 
               0.1777714324605796*#4}, {3.777278785008135 - 5.337627424451967*
                #2 + 8.038638938760011*^-17*#4}, {0.050097571343445384 + 
               0.9499525011874703*#4}, {0}, {0}, {0}, {0}} & }, 2}, 
         {{{True & , CompiledFunction[{11, 12., 4446}, {_Real, _Real, _Real, 
              _Real, _Real, _Real, _Real, _Real, _Real, _Real, _Real, _Real, 
              _Real}, {{3, 0, 0}, {3, 0, 1}, {3, 0, 2}, {3, 0, 3}, {3, 0, 4}, 
              {3, 0, 5}, {3, 0, 6}, {3, 0, 7}, {3, 0, 8}, {3, 0, 9}, 
              {3, 0, 10}, {3, 0, 11}, {3, 0, 12}, {3, 1, 0}}, 
             {{0.95, {3, 0, 23}}, {-1., {3, 0, 18}}, {0.36, {3, 0, 19}}, 
              {-0.64, {3, 0, 15}}, {-0.34199999999999997, {3, 0, 14}}, 
              {2.718281828459045, {3, 0, 22}}}, {0, 0, 25, 0, 1}, 
             {{40, 60, 3, 0, 4, 3, 0, 13}, {41, 263, 3, 0, 5, 3, 0, 15, 3, 0, 
               16}, {16, 14, 16, 10, 17}, {13, 13, 17, 16}, {41, 263, 3, 0, 
               1, 3, 0, 19, 3, 0, 17}, {16, 18, 17, 7, 20}, {13, 4, 5, 20, 
               17}, {16, 13, 7, 20}, {19, 20, 21}, {13, 6, 21, 20}, 
              {41, 263, 3, 0, 22, 3, 0, 12, 3, 0, 21}, {41, 263, 3, 0, 3, 3, 
               0, 23, 3, 0, 24}, {16, 21, 24, 21}, {19, 21, 24}, 
              {13, 7, 24, 21}, {34, 1, 4, 16, 17, 20, 21, 3, 0}, {1}}, 
             Function[{betterRBCTrips`Private`cctm1, 
               betterRBCTrips`Private`kktm1, betterRBCTrips`Private`nltm1, 
               betterRBCTrips`Private`thetatm1, betterRBCTrips`Private`cct, 
               betterRBCTrips`Private`kkt, betterRBCTrips`Private`nlt, 
               betterRBCTrips`Private`thetat, betterRBCTrips`Private`cctp1, 
               betterRBCTrips`Private`kktp1, betterRBCTrips`Private`nltp1, 
               betterRBCTrips`Private`thetatp1, 
               betterRBCTrips`Private`epsVal}, Block[{Compile`$1}, 
               Compile`$1 = betterRBCTrips`Private`cct^(-1); 
                {Compile`$1 - (0.34199999999999997*
                    betterRBCTrips`Private`nltp1)/betterRBCTrips`Private`kkt^
                    0.64, betterRBCTrips`Private`cct + 
                  betterRBCTrips`Private`kkt - 1.*
                   betterRBCTrips`Private`kktm1^0.36*
                   betterRBCTrips`Private`thetat, 
                 betterRBCTrips`Private`nlt - Compile`$1*
                   betterRBCTrips`Private`thetat, 
                 betterRBCTrips`Private`thetat - 2.718281828459045^
                    betterRBCTrips`Private`epsVal*
                   betterRBCTrips`Private`thetatm1^0.95}]], $Failed & ], 
            True & }, {True & , CompiledFunction[{11, 12., 4446}, 
             {_Real, _Real, _Real, _Real, _Real, _Real, _Real, _Real, _Real, 
              _Real, _Real, _Real, _Real}, {{3, 0, 0}, {3, 0, 1}, {3, 0, 2}, 
              {3, 0, 3}, {3, 0, 4}, {3, 0, 5}, {3, 0, 6}, {3, 0, 7}, 
              {3, 0, 8}, {3, 0, 9}, {3, 0, 10}, {3, 0, 11}, {3, 0, 12}, 
              {3, 1, 0}}, {{0.95, {3, 0, 23}}, {-1., {3, 0, 18}}, 
              {0.36, {3, 0, 19}}, {-0.64, {3, 0, 15}}, {-0.34199999999999997, 
               {3, 0, 14}}, {2.718281828459045, {3, 0, 22}}}, 
             {0, 0, 25, 0, 1}, {{40, 60, 3, 0, 4, 3, 0, 13}, 
              {41, 263, 3, 0, 5, 3, 0, 15, 3, 0, 16}, {16, 14, 16, 10, 17}, 
              {13, 13, 17, 16}, {41, 263, 3, 0, 1, 3, 0, 19, 3, 0, 17}, 
              {16, 18, 17, 7, 20}, {13, 4, 5, 20, 17}, {16, 13, 7, 20}, 
              {19, 20, 21}, {13, 6, 21, 20}, {41, 263, 3, 0, 22, 3, 0, 12, 3, 
               0, 21}, {41, 263, 3, 0, 3, 3, 0, 23, 3, 0, 24}, 
              {16, 21, 24, 21}, {19, 21, 24}, {13, 7, 24, 21}, 
              {34, 1, 4, 16, 17, 20, 21, 3, 0}, {1}}, Function[
              {betterRBCTrips`Private`cctm1, betterRBCTrips`Private`kktm1, 
               betterRBCTrips`Private`nltm1, betterRBCTrips`Private`thetatm1, 
               betterRBCTrips`Private`cct, betterRBCTrips`Private`kkt, 
               betterRBCTrips`Private`nlt, betterRBCTrips`Private`thetat, 
               betterRBCTrips`Private`cctp1, betterRBCTrips`Private`kktp1, 
               betterRBCTrips`Private`nltp1, betterRBCTrips`Private`thetatp1, 
               betterRBCTrips`Private`epsVal}, Block[{Compile`$2}, 
               Compile`$2 = betterRBCTrips`Private`cct^(-1); 
                {Compile`$2 - (0.34199999999999997*
                    betterRBCTrips`Private`nltp1)/betterRBCTrips`Private`kkt^
                    0.64, betterRBCTrips`Private`cct + 
                  betterRBCTrips`Private`kkt - 1.*
                   betterRBCTrips`Private`kktm1^0.36*
                   betterRBCTrips`Private`thetat, 
                 betterRBCTrips`Private`nlt - Compile`$2*
                   betterRBCTrips`Private`thetat, 
                 betterRBCTrips`Private`thetat - 2.718281828459045^
                    betterRBCTrips`Private`epsVal*
                   betterRBCTrips`Private`thetatm1^0.95}]], $Failed & ], 
            True & }}, Function[{betterRBCTrips`Private`aPt, 
            betterRBCTrips`Private`allRes}, 
           If[betterRBCTrips`Private`allRes[[1]] === $Failed && 
              betterRBCTrips`Private`allRes[[2]] === $Failed, 
             Throw[$Failed, "noSolutionFound"]]; 
            If[betterRBCTrips`Private`allRes[[1]] === $Failed, 
             Flatten[betterRBCTrips`Private`allRes[[2]]], 
             Flatten[betterRBCTrips`Private`allRes[[1]]]]]}]