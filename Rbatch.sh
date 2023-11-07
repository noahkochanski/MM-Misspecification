#!/bin/bash

##################
####  Slurm preamble

#### #### ####  These are the most frequently changing options

####  Job name
#SBATCH --job-name=test_sim

####  Request resources here
####    These are typically, number of processors, amount of memory,
####    an the amount of time a job requires.  May include processor
####    type, too.

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=1000m
#SBATCH --time=60:00
 

####  Slurm account and partition specification here
####    These will change if you work on multiple projects, or need
####    special hardware, like large memory nodes or GPUs.

#SBATCH --account=stats_dept1
#SBATCH --partition=standard

#### #### ####  These are the least frequently changing options

####  Your e-mail address and when you want e-mail

#SBATCH --mail-user=noahkoch@umich.edu
#SBATCH --mail-type=BEGIN,END

# Add a note here to say what software modules should be loaded.
# for this job to run successfully.
# It will be convenient if you give the actual load command(s), e.g.,
#
# module load R

####  End Slurm preamble
##################


####  Commands your job should run follow this line

echo "Running from $(pwd)"

R CMD BATCH --no-restore --no-save --quiet sim.R Rbatch.out

##  If you copied any files to /tmp, make sure you delete them here!
