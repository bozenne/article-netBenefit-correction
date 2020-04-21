#!/bin/bash

#$ -N scenario2-1TTE-tau50-exp  # Job name
#$ -t 1:20     # Number of jobs
#$ -q long.q    # Queue. Use long.q for run time >8h and all.q otherwise
#$ -l h_vmem=2G # Memory limit, e.g. reserve 1 GB memory 
#$ -tc 128      # Max concurrent jobs
#$ -cwd         # Run in current directory
#$ -o output/scenario2-1TTE-tau50-exp/   # Direct output to subdirectory
#$ -e output/scenario2-1TTE-tau50-exp/   # Direct output to subdirectory

R CMD BATCH BATCH_scenario2-1TTE-tau50-exp.R output/scenario2-1TTE-tau50-exp/$JOB_NAME-I-$SGE_TASK_ID.Rout --no-restore --no-save

## go to directory    ## cd p:/Cluster/GPC/Article-correction-Julien/
## clean outputs      ## rm -r ./output/scenario2-1TTE-tau50-exp/*
## clean results      ## rm -r ./Results/scenario2-1TTE-tau50-exp/*
## submission command ## qsub SUBM_scenario2-1TTE-tau50-exp.sh

## submission output  ## Your job-array 13258.1-20:1 ("scenario2-1TTE-tau50-exp") has been submitted
## submission time    ## 04/20/20 3:02 

## documentation      ## https://ifsv.sund.ku.dk/biostat/biostatwiki/index.php/IT:Cluster : biostat wiki about the cluster
                      ## http://gridscheduler.sourceforge.net/htmlman/manuals.html : grid engine manual 
                      ## http://bayes/ganglia                                      : current load and history of the cluster

## commands           ## qstat         : view jobs of the user
                      ## qstat -u *   : view jobs of all users (the first column shows the job id)
                      ## qstat -j 1034 : show details of a job (or job array) with job id 1034 type     
                      ## qdel 1034     : delete the job with job id 1034 from the queue type
                      ## finger login  : get the name corresponding to the login

## status             ## qw : quewing
                      ##  r : running
                      ## dr : dual state (r)unning and being (d)eleted
