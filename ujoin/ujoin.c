#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char **argv) {

    if( argc < 4 ) {
        fprintf(stderr, "usage: ujoin in-file-1 in-file-2 out-file\n");
        exit(-1);
    }

    const char *in_f1 = argv[1];
    const char *in_f2 = argv[2];
    const char *out_f = argv[3];

    struct stat st1;
    stat(in_f1, &st1);

    struct stat st2;
    stat(in_f2, &st2);

    if( st1.st_size != st2.st_size ) {
        fprintf(stderr, "input files size mismatched\n");
        exit(-1);
    }

    FILE *f1 = fopen(in_f1, "r+b");

    if( !f1 ) {
        fprintf(stderr, "can't open %s: %s\n", in_f1, strerror(errno));
        exit(-1);
    }

    FILE *f2 = fopen(in_f2, "r+b");

    if( !f2 ) {
        fprintf(stderr, "can't open %s: %s\n", in_f2, strerror(errno));
        exit(-1);
    }

    FILE *f3 = fopen(out_f, "w+b");

    if( !f3 ) {
        fprintf(stderr, "can't open %s: %s\n", out_f, strerror(errno));
        exit(-1);
    }

    size_t read    = 0;
    size_t written = 0;

    for(;;) {
        char b1[2] = { 0 };
        char b2[2] = { 0 };

        size_t r = 0, w = 0;

        if( (r = fread(b1, sizeof(b1), 1, f1)) != 1 ) {
            break;
        }

        read += r*sizeof(b1);

        if( (r = fread(b2, sizeof(b2), 1, f2)) != 1 ) {
            break;
        }

        read += r*sizeof(b2);

        if( (w = fwrite(b1, sizeof(b1), 1, f3)) != 1 ) {
            fprintf(stderr, "Error writing file %s: %s\n", out_f, strerror(errno));
            exit(-1);
        }

        written += w*sizeof(b1);

        if( (w = fwrite(b2, sizeof(b2), 1, f3)) != 1 ) {
            fprintf(stderr, "Error writing file %s: %s\n", out_f, strerror(errno));
            exit(-1);
        }

        written += w*sizeof(b2);
    }

    size_t total = st1.st_size + st2.st_size;

    fprintf(stdout, "read: %zu, written %zu of %zu, seems %s\n"
                  , read
                  , written
                  , total
                  , (read == total && written == total) ? "good" : "bad"
           );

finished:
    fclose(f1);
    fclose(f2);
    fclose(f3);

    return 0;
}

