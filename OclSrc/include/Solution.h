{
int var_1 = 100000;
float* var_2 = READ("var_2");
ArrayT* var_3 = CreateArrayT(var_1,var_2);
int var_4 = 100000;
float* var_5 = READ("var_5");
ArrayT* var_6 = CreateArrayT(var_4,var_5);
ArrayT* var_7 = zipWith (var_3,var_6,'+') ;
int var_8 = 100000;
float* var_9 = READ("var_9");
ArrayT* var_10 = CreateArrayT(var_8,var_9);
int var_11 = 100000;
float* var_12 = READ("var_12");
ArrayT* var_13 = CreateArrayT(var_11,var_12);
ArrayT* var_14 = zipWith (var_10,var_13,'+') ;
ArrayT* var_15 = zipWith (var_7,var_14,'+') ;
}