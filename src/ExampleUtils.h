
typedef int     LenType;
typedef float   ElmType;
typedef cl_mem  MemType;

struct ArrayT {
    LenType     len;
    ElmType*    arr;
    MemType     mem;
};

ArrayT * CreateArrayT(LenType len, ElmType* arr);
ArrayT * CreateEmptyArrayT(LenType len);
ArrayT * zipWith(ArrayT* arrT1, ArrayT* arrT2, char ops);

ArrayT * CreateEmptyArrayT(LenType len) {
    ArrayT * arrTuple = (ArrayT*) malloc(sizeof(ArrayT));
    
    arrTuple->len = len;
    arrTuple->arr = (ElmType*) malloc(sizeof(ElmType) * len);

    for (int i = 0; i < len; i++)
        arrTuple->arr[i] = 0;
    
    ALLOCATE_GPU_READ_WRITE_INIT(
                        arrTuple->mem,
                        arrTuple->arr,
                        sizeof(ElmType) * arrTuple->len);

    return arrTuple;
}

ArrayT * CreateArrayT(LenType len, ElmType* arr) {
    ArrayT * arrTuple = (ArrayT*) malloc(sizeof(ArrayT));
    
    arrTuple->len = len;
    arrTuple->arr = (ElmType*) malloc(sizeof(ElmType) * len);

    for (int i = 0; i < len; i++)
        arrTuple->arr[i] = arr[i];
    
    ALLOCATE_GPU_READ(  arrTuple->mem,
                        arrTuple->arr,
                        sizeof(ElmType) * arrTuple->len);

    return arrTuple;
}


ArrayT * zipWith(ArrayT* arrT1, ArrayT* arrT2, char ops) {
    cout << "-- zipWith" << endl;
    ArrayT* ret = CreateEmptyArrayT(arrT1->len);

    cl_kernel kernel = 
        clCreateKernel( program,
                        "zipWith",
                        &errorCode); CHECKERROR;

    errorCode = clSetKernelArg(kernel, 0, sizeof(cl_mem), &arrT1->mem);
    errorCode = clSetKernelArg(kernel, 1, sizeof(cl_mem), &arrT2->mem);
    errorCode = clSetKernelArg(kernel, 2, sizeof(cl_mem), &ret->mem);

    size_t globalsize[] = {arrT1->len};
    size_t localsize[] = {1};
 
    errorCode = clEnqueueNDRangeKernel(
                    cmdQueue,
                    kernel,
                    1,
                    NULL,
                    globalsize,
                    localsize,
                    0,
                    NULL,
                    NULL); CHECKERROR;  

    errorCode = clEnqueueReadBuffer(
                    cmdQueue,
                    ret->mem,
                    CL_TRUE,
                    0,
                    sizeof(ElmType) * ret->len,
                    ret->arr,
                    0,
                    NULL,
                    NULL); CHECKERROR;

    for (int i = 0; i < arrT1->len; i++) 
        cout << "(" 
             << arrT1->arr[i] << "+" 
             << arrT2->arr[i] << ") => " 
             << ret->arr[i] << endl;

    return ret;
}
