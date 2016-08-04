#!/bin/bash
#SBATCH --job-name=mathematica_example
#SBATCH --output=mathematica_example.out
#SBATCH --error=mathematica_example.err
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8

#module load mathematica

/opt/mathematica/10.3/Executables/math -run < ./sample-parallel.m
