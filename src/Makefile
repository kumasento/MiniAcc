CC=gcc
CXX=g++

all:
	$(CXX) oclcommon.cpp BasicHardWare.cpp -o BasicHardWare.o -framework OpenCL
	$(CXX) oclcommon.cpp BasicTwoArrayOpts.cpp -o BasicTwoArrayOpts.o -framework OpenCL
ArrayT:
	$(CXX) oclcommon.cpp ArrayTOpts.cpp -o ArrayTOpts.o -framework OpenCL
Example1:
	$(CXX) oclcommon.cpp Example1.cpp -o Example1.o -framework OpenCL

