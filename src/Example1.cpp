#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <assert.h>

#include "OpenCL/cl.h"

#include "oclcommon.h"

using namespace std;
cl_context context;
cl_program program;
cl_command_queue cmdQueue;
cl_int errorCode;

#include "ExampleUtils.h"


int main(int argc, char *argv[]) {
    /* Environment Initialization */
    char *oclfilename = (char*) "Example.cl";

    cl_device_type deviceType = CL_DEVICE_TYPE_GPU;
    cl_device_id * devices = NULL;

    context     = NULL;
    cmdQueue    = NULL;
    program     = NULL;

    assert(initialization(
                deviceType,
                devices,
                &context, 
                &cmdQueue,
                &program,
                oclfilename));

    #include "ExampleSolution.h"

    return 0;
}
