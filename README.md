#Compile C code into underscores

Compiles
```
#include <stdio.h>
int main() {
    int i;
    int array[4];
	// print four numbers
    for (i = 0; i <=4 ; i ++) {
        array[i]=0;
        printf( " %d\n " , i);
    }
}
```
into
```
#include <stdio.h>








#define _________ 0
#define __________ 4






#define _________________ array
#define __________________ for
#define ___________________ four
#define ____________________ h
#define _____________________ i
#define ______________________ include
#define _______________________ int
#define ________________________ main
#define _________________________ numbers
#define __________________________ print
#define ___________________________ printf
#define ____________________________ stdio


 _______________________  ________________________ (){
 _______________________  _____________________ ;
 _______________________  _________________ [ __________ ];
// __________________________  ___________________  _________________________ 
 __________________ ( _____________________ = _________ ; _____________________ <= __________ ; _____________________ ++){
 _________________ [ _____________________ ]= _________ ;
 ___________________________ (" %d\n ", _____________________ );
}
}
```

#How to run

Download the binary. compile digests all inputstream and put the content to output stream.

```
$ ./compile <[inputfile] >[outputfile]
```

For example,

```
$ ./compile <a.c > a_compiled.c
```

##Known issue
The code cannot have trailing escapes
