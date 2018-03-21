#!/bin/bash
#SBATCH --job-name=mex5post
#SBATCH --output=mex5post.%N.%j.out
#SBATCH --error=mex5post.%N.%j.err
#SBATCH --nodes=6
#SBATCH --ntasks-per-node=20



xvfb-run /opt/mathematica/11.0/Executables/math -script ./paperComps.mth

