
#编译实习 MiniAcc-OpenCL项目报告

##引言

MiniAcc是Haskell的嵌入式**领域特定语言**(Domain Specific Language，即DSL)，属于另一个Haskell的DSL-Accelerate[^1]的子集：二者的目标都是在Haskell语言中提供对并行编程，尤其是GPU并行编程的支持。Accelerate语言的实现是基于从Accelerate的**前端语言**到**后端语言**，如CUDA的**翻译**、**即时编译**与**运行**的框架。MiniAcc-OpenCL也遵循大致相同的框架，不过把后端语言从CUDA转化为跨硬件平台的语言：OpenCL。

[^1]: [Accelerate简介](http://hackage.haskell.org/package/accelerate-cuda)
[^2]: [OpenCL简介](http://developer.nvidia.com/opencl)