#include <limits.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>

// Attempt to write args to file (named pipe) in a non-blocking fashion.
// Args are intepreted in a similar fashion as echo.
// Only up to PIPE_BUF bytes are written to ensure atomicity (man 7 pipe).

int main(int argc, char *argv[]){
    if (argc <= 2) {
        fprintf(stderr, "usage: %s file arg [arg...]\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    int fifo = open(argv[1], O_NONBLOCK | O_WRONLY | O_CLOEXEC);
    if (fifo < 0) {
        fprintf(stderr, "could not open: '%s'\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    ssize_t len = 1; // newline
    for (int i = 2; i < argc; ++i) {
        len += strlen(argv[i]);
        len += 1;
    }

    char *line = calloc(len+1, sizeof(char)); // plus null
    assert(line);
    for (int i = 2; i < argc; ++i) {
        strcat(line, argv[i]);
        strcat(line, " ");
    }
    assert(line[len] == '\0');

    int estatus = EXIT_SUCCESS;
    if (len > PIPE_BUF) {
        fprintf(stderr, "line is too long: %ld > %d\n", len, PIPE_BUF);
        estatus = EXIT_FAILURE;
        len = PIPE_BUF;
        line[len] = '\0';
    }
    assert(len >= 1); // newline must fit
    line[len-1] = '\n';

    ssize_t written = write(fifo, line, len);
    if (written != len) {
        fprintf(stderr, "could not write: '%s'\n", strerror(errno));
        estatus = EXIT_FAILURE;
    }

    close(fifo);

    return estatus;
}
