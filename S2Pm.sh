#!/bin/bash
# convert single machine instance into parallel machine instance

awk -v m=$1 '
NR==1{
    print $0, m;
}
1<NR&&NR<4{
  print $0
}
NR==4{
  for(i = 1; i < NF+1; i++) { 
    printf "%d ", $i/m
  } 
  print "";
}
'
