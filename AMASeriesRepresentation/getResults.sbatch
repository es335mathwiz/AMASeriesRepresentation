#!/bin/bash

#SBATCH --job-name=math-simple
#SBATCH --output=forFinish-%N.%jsimple.out
#SBATCH --error=forFinish-%N.%jsimple.err
#SBATCH --time=2-8:05:00
#SBATCH --nodes=2
#SBATCH --ntasks=28
#SBATCH --exclusive=user
#SBATCH --constraint=infiniband

#module load mathematica
cd /msu/scratch2/m1gsa00/git/AMASeriesRepresentation/AMASeriesRepresentation

for((ii=0;ii<=2;ii++))
do
for((jj=0;jj<=2;jj++))
do
for((kk=0;kk<=2;kk++))
do
for((ll=0;ll<=4;ll++))
do
wolframscript -file getResultsScaffold.mth -args $((ii)) $((jj)) $((kk)) $((ll))
done
done
done
done
