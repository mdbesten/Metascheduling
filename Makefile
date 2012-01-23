CXXFLAGS = -Wall -O3 #-ggdb #-pg
OBJS = Flow.o Localsearch.o Problem.o Run.o Solution.o Timer.o
RBIN = nice R BATCH --vanilla --slave
SAMPLE.SZ = 10
SAMPLE.PREFIX = vns.sample
SAMPLE.LOG = vns.sample.Rtimes
.PHONY = all clean ilstest

all: ${SAMPLE.PREFIX}.Rout

%.Rout: %.R 
	${RBIN} $<

${SAMPLE.PREFIX}.R: sample.R vns.out
	echo "source(\"$<\");" > $@
	echo "result <- sample.vns(${SAMPLE.SZ});" >> $@
	echo "save(result, file=\"${SAMPLE.LOG}\");" >> $@

ilstest: neh.out ils.out ils.batch
	${RBIN} ils.batch

ils.batch: ils.R
	echo 'source("ils.R"); ils.batch();' > $@

lstest: localsearch.out s2p.out ls.batch
	${RBIN} ls.batch

ls.batch: localsearch.R
	echo 'source("localsearch.R"); lsall(2);' > $@

%.out: %.cc ${OBJS}
	${CXX} ${CXXFLAGS} -o $@ $^

%.tgz:	
	tar zcf $@ *.cc *.h *.R *.sh Makefile

clean:
	-rm *.o
	-rm *.out
	-rm *~
