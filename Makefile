.PHONY : all build doctest testing clean

all : build doctest testing

build : src
	make -C src build

doctest : build
	make -C src test

testing : test
	make -C test

clean :
	make -C src clean
	make -C test clean

# EOF
