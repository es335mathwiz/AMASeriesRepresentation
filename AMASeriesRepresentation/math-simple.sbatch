#!/bin/bash

#SBATCH --job-name=math-simple
#SBATCH --output=math-simple.out
#SBATCH --error=math-simple.err
#notSBATCH --partition=sandyb
#SBATCH --time=00:05:00
#SBATCH --ntasks=1

#module load mathematica

/opt/mathematica/10.3/Executables/math -run < math-simple.m
