#!/bin/bash

#$ -N scenario7-1TTE-tau50-weibull  # Job name
#$ -t 1:20     # Number of jobs
#$ -q long.q    # Queue. Use long.q for run time >8h and all.q otherwise
#$ -l h_vmem=2G # Memory limit, e.g. reserve 1 GB memory 
#$ -tc 128      # Max concurrent jobs
#$ -cwd         # Run in current directory
#$ -o output/scenario7-1TTE-tau50-weibull/   # Direct output to subdirectory
#$ -e output/scenario7-1TTE-tau50-weibull/   # Direct output to subdirectory

R CMD BATCH BATCH_scenario7-1TTE-tau50-weibull.R output/scenario7-1TTE-tau50-weibull/$JOB_NAME-I-$SGE_TASK_ID.Rout --no-restore --no-save

## go to directory    ## cd p:/Cluster/GPC/Article-correction-Julien/
## clean outputs      ## rm -r ./output/scenario7-1TTE-tau50-weibull/*
## clean results      ## rm -r ./Results/scenario7-1TTE-tau50-weibull/*
## submission command ## qsub SUBM_scenario7-1TTE-tau50-weibull.sh

## submission output  ## Your job-array 13263.1-20:1 ("scenario7-1TTE-tau50-weibull") has been submitted
## submission time    ## 04/20/20 3:06 

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
