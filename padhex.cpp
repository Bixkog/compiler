#include<cstdio>
#include<fstream>

int main(int argc, char const *argv[])
{
    std::fstream file(argv[1]);
    unsigned int i;
    while(file>>i)
    {
        printf("%04x\n", i);
    }
    return 0;
}