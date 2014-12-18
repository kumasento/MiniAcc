CC=gcc
CXX=g++

INCLUDE=include
SRC=src
BIN=bin

Main:
	$(CXX) \
		$(SRC)/oclcommon.cpp \
		main.cpp -o $(BIN)/oclmain.o \
		-I$(INCLUDE) \
		-framework OpenCL

