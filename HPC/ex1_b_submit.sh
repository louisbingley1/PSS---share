#!/bin/bash
#
#$ -cwd -b y  
#$ -N ex1
#$ -o ./ex1.out
#$ -e ./ex1.err

source /SFS/product/Modules/default/init/bash 
module purge
module load R

R CMD BATCH --no-save --no-restore "--args b=$SGE_TASK_ID" ./ex1_b.R ./ex1_b_${SGE_TASK_ID}.Rout