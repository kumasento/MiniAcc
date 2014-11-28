{
int Var_1 = 10;
float Var_2[10] = {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0};
ArrayT* Var_3 = CreateArrayT(Var_1,Var_2);
int Var_4 = 10;
float Var_5[10] = {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0};
ArrayT* Var_6 = CreateArrayT(Var_4,Var_5);
int Var_7 = 10;
float Var_8[10] = {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0};
ArrayT* Var_9 = CreateArrayT(Var_7,Var_8);
ArrayT* Var_10 = zipWith (Var_6,Var_9,'+') ;
ArrayT* Var_11 = zipWith (Var_3,Var_10,'+') ;}
