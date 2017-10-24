GSC=gsc

bin_PROGRAMS = bug_matrix
bug_matrix_SOURCES = bug_matrix.c \
	bug_matrix_.c

bug_matrix.scm: bug_matrix.bug.scm
	bug-gscpp bug_matrix.bug.scm bug_matrix.scm

bug_matrix.c: bug_matrix.scm
	$(GSC) -c bug_matrix.scm

bug_matrix.o: bug_matrix.scm
	$(GSC) -link -l @LIBBUG_INCLUDE_DIR@libbug bug_matrix.scm
	$(GSC) -obj bug_matrix.c bug_matrix_.c

bug_matrix_.o: bug_matrix.o

