
__kernel
void zipWith(__global float* A,
             __global float* B,
             __global float* R) {
    int id = get_global_id(0);
    R[id] = A[id] + B[id];
}
