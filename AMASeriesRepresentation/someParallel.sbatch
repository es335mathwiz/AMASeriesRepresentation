#!/bin/bash

#SBATCH --job-name=math-simple
#SBATCH --output=someParallel-%N.%jsimple.out
#SBATCH --error=someParallel-%N.%jsimple.err
#SBATCH --time=8:05:00
#SBATCH --nodes=5
#SBATCH --ntasks=20
#SBATCH --exclusive=user

#module load mathematica

/msu/scratch2/m1gsa00/git/AMASeriesRepresentation/AMASeriesRepresentation/xvfb-run-safe /opt/local/bin/math -run < someParallel.mth
