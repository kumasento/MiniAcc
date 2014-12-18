{
int Var_1 = 10000;
float* Var_2 = READ("Var_2");
ArrayT* Var_3 = CreateArrayT(Var_1,Var_2);
int Var_4 = 10000;
float* Var_5 = READ("Var_5");
ArrayT* Var_6 = CreateArrayT(Var_4,Var_5);
int Var_7 = 10000;
float* Var_8 = READ("Var_8");
ArrayT* Var_9 = CreateArrayT(Var_7,Var_8);
ArrayT* Var_10 = zipWith (Var_6,Var_9,'+') ;
ArrayT* Var_11 = zipWith (Var_3,Var_10,'+') ;
}