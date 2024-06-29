#!/bin/bash
#
#$ -cwd -b y  
#$ -N ex1
#$ -o ./ex1.out
#$ -e ./ex1.err

source /SFS/product/Modules/default/init/bash 
module purge
module load R

R CMD BATCH --no-save --no-restore ./combine.R ./combine.Rout