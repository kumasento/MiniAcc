
#编译实习 MiniAcc-OpenCL项目报告

##引言

MiniAcc是Haskell的嵌入式**领域特定语言**(Domain Specific Language，即DSL)，属于另一个Haskell的DSL-Accelerate的子集：二者的目标都是在Haskell语言中提供对并行编程，尤其是GPU并行编程的支持。Accelerate语言的实现是基于从Accelerate的**前端语言**到**后端语言**，如CUDA的**翻译**、**即时编译**与**运行**的框架。MiniAcc-OpenCL也遵循大致相同的框架，不过把后端语言从CUDA转化为跨硬件平台的语言：OpenCL。

本文针对MiniAcc的语法和编译器设计进行详细的介绍，包括MiniAcc的**AST语法**，**AST设计**与**OpenCL模板设计**。最后