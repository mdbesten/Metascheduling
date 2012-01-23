#!/bin/bash
# $Id: rtd.sh,v 1.1 2003/12/30 15:26:15 mldb Exp $
# generate random run time distribution
# 1: time limit, e.g. 60
# 2: lower bound on solution quality, e.g. 7777
# 3: performance scalar, e.g. 100
# 4: frequency of improvement, e.g. 10

seq $2 $(($2-(($1*$3)))) | random $1 | awk '{print NR, $0}' | grep $(echo \"$(seq $1 | random $(($1/$4)) | sed -e 's/$/\\|/' -e 's/^/\^/' | tr -d '\n')Q\"); 
