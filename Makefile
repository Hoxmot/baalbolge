.PHONY : all build testing

all : build testing

build : src
	make -C src

testing : test
	make -C test
