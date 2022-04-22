.PHONY : all build testing clean

all : build testing

build : src
	make -C src

testing : test
	make -C test

clean :
	make -C src clean
	make -C test clean
