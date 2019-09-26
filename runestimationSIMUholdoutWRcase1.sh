#!/bin/bash -l
# declare a name for this job to be my_serial_job
# it is recommended that this name be kept to 16 characters or less
#PBS -N metalsimuchao

# request the queue (enter the possible names, if omitted, default is the default)
# this job is going to use the default
#PBS -q default

# request 1 node
#PBS -l nodes=1:ppn=1

# request 0 hours and 15 minutes of wall time
# (Default is 1 hour without this directive)
#PBS -l walltime=56:00:00

# mail is sent to you when the job starts and when it terminates or aborts
#PBS -m bea

# specify your email address
#PBS -M jai.woo.lee.gr@dartmouth.edu

# By default, PBS scripts execute in your home directory, not the
# directory from which they were submitted. The following line
# places the job in the directory from which the job was submitted.
cd $PBS_O_WORKDIR

# run the program using the relative path
# run the program with R >= 3.5.0 and in the right directory
Rscript runestimationSIMUholdoutWRcase1.R

exit 0






