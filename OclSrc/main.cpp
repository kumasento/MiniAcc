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

double sum_of_time = 0.0;

#include "utils.h"
#include "fileio.h"

int main(int argc, char *argv[]) {
    /* Environment Initialization */
    char *oclfilename = (char*) "kernel/kernel.cl";

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

    #include "Solution.h"

    printf("Time Elapsed: %.6fs\n", sum_of_time);

    return 0;
}
