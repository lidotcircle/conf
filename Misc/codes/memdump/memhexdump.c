#include"memhexdump.h"
#include<math.h>

// macro_cons : MAXBIT, Max bit long of the order number.
#define MAXBIT 5

static char *(ASC[]) = {"NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
                 " BS", " HT", " LF", " VT", " FF", " CR", " SO", " SI",
                 "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
                 "CAN", " EM", "SUB", "ESC", " FS", " GS", " RS", " US",
                 " SP", "DEL", "???"};

//{ func : __memtobase__(), without checking pass argument

// macro_func : stack_overflow(), the handle when <ret_stack -gt ret_top>, !NEXT!
#define stack_overflow() printf("Output memory isn't enough for storing output.");\
    printf("In memtobase(), the output will lose something.");\
    *ret_tp = '\0'; return 1;

// marco_func : pushret(), use the macro to manipulate the ret_sp variable
#define pushret(n) line_length += (n);\
                  ret_sp += (n);

// marco_func : ret_newline(), new line in ret_sp
#define ret_newline() for(;;){\
    x = win_cols - line_length - 5;\
    if(x > 0){\
        memset(ret_sp, ' ', x);\
        pushret(x);\
    }\
    *ret_sp = '*'; pushret(1);\
    *ret_sp = '\n'; pushret(1);\
    line_length = 0;\
    break;\
}

// macro_func : line_begin(), put a asterisk at beginning of line
#define line_begin() for(;;){\
    memset(ret_sp, ' ', 4);\
    pushret(4);\
    *ret_sp = '*'; pushret(1);\
    *ret_sp = ' '; pushret(1);\
    sprintf(numbuf, "%5.5lx | ", linnum);\
    linnum += byteCols_per_winLine;\
    memmove(ret_sp, numbuf, strlen(numbuf));\
    pushret(strlen(numbuf));\
    break;\
}

// macro_func : asterisk(), output a line asterisk
#define asterisk() for(;;){\
    memset(ret_sp, ' ', 4);\
    pushret(4);\
    memset(ret_sp, '*', win_cols - 9);\
    pushret(win_cols - 9);\
    ret_newline();\
    break;\
}


void __memtobase__(const void* mem, void* memstr, size_t n, char base,
        unsigned short win_cols)
{
    // output string
    char wor[] = "0123456789ABCDEF";

    // Duplicate of the <mem>, but the type change to (unsigned char*)
    unsigned char *src_byte = (unsigned char*)mem;

    /* 
     * The vaule of the (*src_byte), 
     * at loop process, val = *src_byte++.
     * use this var to generate the bit order of (*src_byte)
     */
    unsigned char val;

    // allocate alb bytes to per byte of the source
    int alb = ceil((log(256) / log(base)));
    if(win_cols - 14 - MAXBIT - alb < 0){
        fprintf(stderr, "__memtobase__(), error column number.\n");
        return;
    }

    /*
     * stack pointer and base pointer and top pointer, initialized by 
     * the <memstr> argument.
     */
    char *ret_sp = (char*)memstr; // the stack pointer
    /* !NEXT!
    char *ret_bp = (char*)memstr; // the base pointer
    char *ret_tp = (char*)memstr + n - 1; // the top pointer
    */

    // loop Counter
    size_t i, j;

    // Columns per line
    unsigned short byteCols_per_winLine\
        = base < 16 ? (win_cols - 10 + 1 - 2 - 2 - 1- MAXBIT) / (alb + 1) :\
        (win_cols - 10 + 1 - 2 - 2 - 1- MAXBIT) / (alb + 2);
    // remain byte of winLine
    long x = 0;
    // byte order of output
    long linnum = 0;
    char numbuf[MAXBIT + 2 + 2 + 1]; // ' MAXBIT | \0'
    // line length of current line
    unsigned short line_length = 0;

    // push a '\n' at first byte of output
    memset(ret_sp, '\n', 1);
    pushret(1);
    // next, push a line asterisk
    asterisk();
    // hint content
    char BaseHint[] = "    * The base is 00:"; // Hint
    char *insteal = strchr(BaseHint, '0');
    sprintf(insteal, "%2.2d:", base);
    //push hint
    if(memmove(ret_sp, BaseHint, strlen(BaseHint)) == NULL){
        perror("memmove()!");
    }
    pushret(strlen(BaseHint));
    ret_newline();
    asterisk();

    /* The line counter */
    line_begin();
    memset(ret_sp - (MAXBIT + 2 + 2), ' ', (MAXBIT + 2 + 2));
    char line_counter_buf[10];
    short minus;
    short strlen_buf;
    if(base >= 16)
        alb++;
    for(i = 0; i <= (size_t)byteCols_per_winLine - 1; i++){
        sprintf(line_counter_buf, "%lX", i);
        strlen_buf = strlen(line_counter_buf);
        if((minus = alb - strlen_buf) > 0){
            memset(ret_sp, ' ', minus);
            pushret(minus);
            memmove(ret_sp, line_counter_buf, strlen_buf);
            pushret(strlen_buf);
        } else {
            memmove(ret_sp, line_counter_buf, alb);
            pushret(alb);
        }

    }
    ret_newline();
    line_begin();
    memset(ret_sp - (MAXBIT + 2 + 2), '-', win_cols - 10);
    pushret(win_cols - 10 - 4 - MAXBIT);
    ret_newline();
    if(base >= 16)
        alb--;

    linnum = 0;
    line_begin();

    /*
     * Main Loop of this function.
     */
    for(i = 0;i < n; i++){
        val = *src_byte;

        // one byte loop
        for(j = 0;;j++){
            *(ret_sp + alb -1 - j) = wor[val%base];
            if(((val /= base) == 0) && (n - j > 1)){
                memset(ret_sp, '0', alb - j -1);
                break;
            }
        }
        pushret(alb);
        if(i != n - 1){
            if((i%byteCols_per_winLine) == byteCols_per_winLine - 1){
                ret_newline();
                line_begin();
            } else {
                if(base < 16){
                    memset(ret_sp, ' ', 1);
                    pushret(1);
                } else {
                    memset(ret_sp, ' ', 2);
                    pushret(2);
                }
            }
            src_byte++;
        } else {
            ret_newline();
            asterisk();
            *ret_sp = '\0';
            pushret(1);
            break;
        }
    }
}
//}

//{ func : __M_memtobase__(), without checking pass argument

// MACRO
// OFFSET
#define OF(base, offset) (base + offset)

void __M_memtobase__(const void* mem, void* memstr, size_t n, char base,
        unsigned short win_cols)
{
    // output string
    char wor[] = "0123456789ABCDEF";

    // Duplicate of the <mem>, but the type change to (unsigned char*)
    unsigned char *src_byte = (unsigned char*)mem;

    /* 
     * The vaule of the (*src_byte), 
     * at loop process, val = *src_byte++.
     * use this var to generate the bit order of (*src_byte)
     */
    unsigned char val;

    // allocate alb bytes to per byte of the source
    int alb = ceil((log(256) / log(base)));
    if(win_cols - 14 - MAXBIT - alb < 0){
        fprintf(stderr, "__memtobase__(), error column number.\n");
        return;
    }

    // weather easy model output or not.
    char *extra_line;
    char *extra_word;
    char *extra_alb;
    extra_line = (char*)malloc(win_cols - 3); // win_cols - 4 + 1
    extra_word = (char*)malloc(3); extra_word[0] = extra_word[2] = '\'';
    extra_alb = (char*)malloc(base < 16 ? alb + 1 : alb + 2);

    /*
     * stack pointer and base pointer and top pointer, initialized by 
     * the <memstr> argument.
     */
    char *ret_sp = (char*)memstr; // the stack pointer
    /* !NEXT!
    char *ret_bp = (char*)memstr; // the base pointer
    char *ret_tp = (char*)memstr + n - 1; // the top pointer
    */

    // loop Counter
    size_t i, j;

    // Columns per line
    unsigned short byteCols_per_winLine\
        = base < 16 ? (win_cols - 10 + 1 - 2 - 2 - 1- MAXBIT) / (alb + 1) :\
        (win_cols - 10 + 1 - 2 - 2 - 1- MAXBIT) / (alb + 2);
    // remain byte of winLine
    long x = 0;
    // byte order of output
    long linnum = 0;
    char numbuf[MAXBIT + 2 + 2 + 1]; // ' MAXBIT | \0'
    // line length of current line
    unsigned short line_length = 0;

    // push a '\n' at first byte of output
    memset(ret_sp, '\n', 1);
    pushret(1); line_length=0;
    // next, push a line asterisk
    asterisk();
    // hint content
    char BaseHint[] = "    * The base is 00:"; // Hint
    char *insteal = strchr(BaseHint, '0');
    sprintf(insteal, "%2.2d:", base);
    //push hint
    if(memmove(ret_sp, BaseHint, strlen(BaseHint)) == NULL){
        perror("memmove()!");
    }
    pushret(strlen(BaseHint));
    ret_newline();
    asterisk();

    /* The line counter */
    line_begin();
    memset(ret_sp - (MAXBIT + 2 + 2), ' ', (MAXBIT + 2 + 2));
    char line_counter_buf[10];
    short minus;
    short strlen_buf;
    for(i = 0; i <= (size_t)byteCols_per_winLine - 1; i++){
        sprintf(line_counter_buf, "%lX", i);
        strlen_buf = strlen(line_counter_buf);
        if((minus = alb - strlen_buf) > 0){
            memset(ret_sp, ' ', minus);
            pushret(minus);
            memmove(ret_sp, line_counter_buf, strlen_buf);
            pushret(strlen_buf);
        } else {
            memmove(ret_sp, line_counter_buf, alb);
            pushret(alb);
        }
        if(base >= 16){
            memset(ret_sp, ' ', 2); pushret(2);
        } else {
            memset(ret_sp, ' ', 1); pushret(1);
        }
    }
    ret_newline();
    line_begin();
    memset(ret_sp - (MAXBIT + 2 + 2), '-', win_cols - 10);
    pushret(win_cols - 10 - 4 - MAXBIT);
    ret_newline();

    linnum = 0;
    line_begin();
    memmove(extra_line, ret_sp -  6 - strlen(numbuf), 6 + strlen(numbuf));//new
    memset(extra_line + (win_cols -18), ' ', 13);
    *(extra_line + win_cols - 5) = '*'; *(extra_line + win_cols - 4) = '\n';// new
    char *aaa; unsigned short bbb; bbb = 6 + strlen(numbuf);

    /*
     * Main Loop of this function.
     */
    for(i = 0;i < n; i++){
        val = *src_byte;
        if(val < 127 && val > 32){
            memset(extra_word + 1, val, 1);
            aaa = extra_word;
        } else if (val <= 32) {
            aaa = ASC[val];
        } else if (val == 127) {
            aaa = ASC[33];
        } else {
            aaa = ASC[34];
        }
        if(alb > 3){
            memset(OF(extra_line, bbb), ' ', alb -3);
            bbb += alb -3;
        }
        memmove(OF(extra_line, bbb), aaa, 3);
        bbb += 3; *OF(extra_line, bbb) = ' '; bbb++;

        // one byte loop
        for(j = 0;;j++){
            *(ret_sp + alb -1 - j) = wor[val%base];
            if(((val /= base) == 0) && (n - j > 1)){
                memset(ret_sp, '0', alb - j -1);
                break;
            }
        }
        pushret(alb);
        if(i != n - 1){
            if((i%byteCols_per_winLine) == byteCols_per_winLine - 1){
                ret_newline();
                memmove(ret_sp, extra_line, win_cols -3); pushret(win_cols -3);
                line_length=0;
                line_begin();
                memmove(extra_line, ret_sp -  6 - strlen(numbuf), 6 + strlen(numbuf));// new
                memset(extra_line + (win_cols -18), ' ', 13);
                *(extra_line + win_cols - 5) = '*'; *(extra_line + win_cols - 4) = '\n';
                bbb = 6 + strlen(numbuf);// new
            } else {
                if(base < 16){
                    memset(ret_sp, ' ', 1);
                    pushret(1);
                } else {
                    memset(ret_sp, ' ', 2);
                    pushret(2);
                }
            }
            src_byte++;
        } else {
            ret_newline();
            memset(OF(extra_line, bbb), ' ', win_cols - 3 - bbb - 2);
            memmove(ret_sp, extra_line, win_cols -3); pushret(win_cols -3);
            asterisk();
            *ret_sp = '\0';
            pushret(1);
            break;
        }
    }
}
//}

//{ func : memtobase()

int memtobase(const void* mem, void* memstr, size_t n, char base)
{
    if (base > 16){
        printf("WARNING ! base can't greater than 16\n");
        return EXIT_FAILURE;
    }

    /*
     * Change output string through get the size of window.
     * The output will look like:
     *      **************************************
     *      * The base is <base>:                *
     *      **************************************
     *      *      1  2  3  4  5  6  7  8  ...   *
     *      *------------------------------------*
     *      * 1 |                                *
     *      * 2 |                                *
     *      * 3 |                                *
     *      * 4 |                                *
     *      * 5 |                                *
     *      * 6 |                                *
     *      * 7 |                                *
     *      * 8 |                                *
     *      * . |                                *
     *      * . |                                *
     *      * . |                                *
     *      **************************************
     */
    struct winsize win;
    ioctl(0, TIOCGWINSZ, &win);
    short cols;
    int alb = base < 16 ? ceil((log(256) / log(base))) : \
              ceil((log(256) / log(base))) + 1;
    if((cols = (win.ws_col - 10 + 1 - 2 - 2 - 1- MAXBIT) / (alb + 1)) < 5){
        fprintf(stderr, "membase() ! The screen is too narrowed to ");
        fprintf(stderr, "correctly display the output,\n");
        fprintf(stderr, "you should change the console width if you can.\n");
        return EXIT_FAILURE;
    }
    errno=0;
    __memtobase__(mem, memstr, n, base, win.ws_col);

    // errono checking
    if(errno == 0){
        return EXIT_SUCCESS;
    } else {
        fprintf(stderr, "IN memtobase() FAULT! %s.\n", strerror(errno));
        exit(EXIT_FAILURE);
    }
}
//}

//{ func : M_memtobase()

int M_memtobase(const void* mem, void* memstr, size_t n, char base)
{
    if (base > 16){
        printf("WARNING ! base can't greater than 16\n");
        return EXIT_FAILURE;
    }

    /*
     * Change output string through get the size of window.
     * The output will look like:
     *      **************************************
     *      * The base is <base>:                *
     *      **************************************
     *      *      1  2  3  4  5  6  7  8  ...   *
     *      *------------------------------------*
     *      * 1 |                                *
     *      * 2 |                                *
     *      * 3 |                                *
     *      * 4 |                                *
     *      * 5 |                                *
     *      * 6 |                                *
     *      * 7 |                                *
     *      * 8 |                                *
     *      * . |                                *
     *      * . |                                *
     *      * . |                                *
     *      **************************************
     */
    struct winsize win;
    ioctl(0, TIOCGWINSZ, &win);
    short cols;
    int alb = base < 16 ? ceil((log(256) / log(base))) : \
              ceil((log(256) / log(base))) + 1;
    if((cols = (win.ws_col - 10 + 1 - 2 - 2 - 1- MAXBIT) / (alb + 1)) < 5){
        fprintf(stderr, "membase() ! The screen is too narrowed to ");
        fprintf(stderr, "correctly display the output,\n");
        fprintf(stderr, "you should change the console width if you can.\n");
        return EXIT_FAILURE;
    }
    errno=0;
    __M_memtobase__(mem, memstr, n, base, win.ws_col);

    // errono checking
    if(errno == 0){
        return EXIT_SUCCESS;
    } else {
        fprintf(stderr, "IN memtobase() FAULT! %s.\n", strerror(errno));
        exit(EXIT_FAILURE);
    }
}
//}

//{ func : alloc_memtobase()
char* alloc_memtobase(const void* mem, size_t n, char base)
{
    if (base > 16){
        printf("WARNING ! base can't greater than 16\n");
        return NULL;
    }
    struct winsize win;
    ioctl(0, TIOCGWINSZ, &win);
    int alb = ceil((log(256) / log(base)));
    alb += base < 16 ? 0 : 1;
    short byteCols_per_winLine = (win.ws_col - 10 + 1 - 2 -2 - MAXBIT) / (alb+ 1);
    int ll = n / byteCols_per_winLine + 1 + 6;
    size_t alloc_mem = ll * (win.ws_col - 3) + 2;
    char* output = (char*)malloc(alloc_mem);
    memtobase(mem, output, n, base);
    return output;
}
//}

//{ func : alloc_M_memtobase()
char* alloc_M_memtobase(const void* mem, size_t n, char base)
{
    if (base > 16){
        printf("WARNING ! base can't greater than 16\n");
        return NULL;
    }
    struct winsize win;
    ioctl(0, TIOCGWINSZ, &win);
    int alb = ceil((log(256) / log(base)));
    alb += base < 16 ? 0 : 1;
    short byteCols_per_winLine = (win.ws_col - 10 + 1 - 2 -2 - MAXBIT) / (alb+ 1);
    int ll = (n / byteCols_per_winLine + 1) * 2  + 6;
    size_t alloc_mem = ll * (win.ws_col - 3) + 2;
    char* output = (char*)malloc(alloc_mem);
    M_memtobase(mem, output, n, base);
    return output;
}
//}
