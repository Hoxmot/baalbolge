.PHONY : all build testing clean

all : build testing

build : src
	make -C src build

testing : test
	make -C src test
	make -C test

clean :
	make -C src clean
	make -C test clean

# EOF
