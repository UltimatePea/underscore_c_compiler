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
#define _ i
#define __ int
#define ___ 0
#define ____ 4
#define _____ array
#define ______ for
#define _______ h
#define ________ include
#define _________ main
#define __________ printf
#define ___________ stdio



 __  _________ (){
 __  _ ;
 __  _____ [ ____ ];
/// print four numbers
 ______ ( _ = ___ ; _ <= ____ ; _ ++){
 _____ [ _ ]= ___ ;
 __________ (" %d\n ", _ );
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

