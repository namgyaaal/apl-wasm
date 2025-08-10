#include <stdio.h>
#include "libapl.h"

int main() 
{

    init_libapl("foobar", 1);


    apl_exec("1 + 1 + 1 + 1");
    apl_exec("16 ÷ 4");
    apl_exec("a ← 4 + 4");
    apl_exec("a");


    //const char *result = apl_command("11");

    //printf("Result: %s\n", result);

    //free((void*)result); 
    return 0; 
}