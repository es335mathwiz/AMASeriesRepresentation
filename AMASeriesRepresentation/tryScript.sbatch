#!/bin/bash

#SBATCH --job-name=math-simple
#SBATCH --output=forFinish-%N.%jsimple.out
#SBATCH --error=forFinish-%N.%jsimple.err
#SBATCH --time=2-8:05:00
#SBATCH --nodes=5
#SBATCH --ntasks=28
#SBATCH --exclusive=user
#SBATCH --constraint=infiniband

#module load mathematica

wolframscript -file tryScript.mth 

