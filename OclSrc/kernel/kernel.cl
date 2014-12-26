
__kernel
void zipWith(__global float* A,
             __global float* B,
             __global float* R,
             char ops) {
    int id = get_global_id(0);

    if (ops == '+') 
        R[id] = A[id] + B[id];
    else if (ops == '-')
        R[id] = A[id] - B[id];
    else if (ops == '*')
        R[id] = A[id] * B[id];
    else if (ops == '/')
        R[id] = A[id] / B[id];
}
