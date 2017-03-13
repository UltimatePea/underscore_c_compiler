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

Download the binary. `compile` digests all input stream(`stdin`) and put the content to output stream(`stdout`).

```
$ ./compile <[inputfile] >[outputfile]
```

For example,

```
$ ./compile <a.c > a_compiled.c
```


###Note that the binary is compiled under macOS 10.12.3, recompilation is needed if you want to run it on other platforms.

To compile this project, download Haskell compiler (`ghc`) from https://www.haskell.org/ghc/download, compile code using

```
$ ghc compile.hs
```
