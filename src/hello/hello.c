/**
* hello.c -- 'Hello World' basic test.
*/


int main()
{
    puts("compile time: " __DATE__ " -- " __TIME__ "\n");
    puts("gcc version:  " __VERSION__ "\n");
    puts("\n\nHello World!\n\n\n");
}

