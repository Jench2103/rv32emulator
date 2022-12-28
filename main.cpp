#include "emulator.h"
#include "translate.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void get_path_prefix(char *argv1, char **path) {
    char *ptr;

    if (!(*path = (char *)malloc(sizeof(char) * strlen(argv1)))) {
        printf("malloc is failed in 'main', copy the argv[1] to char *path.\n");
        exit(2);
    }

    copy_str(*path, argv1);

    ptr = *path + strlen(*path);

    // find the last '/' in argv1
    while (ptr != *path) {
        if (*ptr == '/') {
            *ptr = '\0';
            break;
        }
        ptr--;
    }

    if (ptr == *path)
        **path = '\0';

    if (**path)
        strcat(*path, "/");
}

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("usage: %s asmfile\n", argv[0]);
        exit(1);
    }

    FILE* fin = fopen(argv[1], "r");
    if (!fin) {
        printf("%s: No such file\n", argv[1]);
        exit(2);
    }

    char *path = new char[strlen(argv[2])];

    if (argc < 3) {
        get_path_prefix(argv[1], &path);
    } else {
        strncpy(path, argv[2], strlen(argv[2]));
    }

    bool start_immediate = false;
    if (argc >= 4) {
        start_immediate = true;
    }

    int memoff = 0;
    uint8_t* mem = (uint8_t*)malloc(ADDRESS_SPACE_END);
    instr* imem = (instr*)malloc(DATA_OFFSET * sizeof(instr) / 4);
    label_loc* labels = (label_loc*)malloc(MAX_LABEL_COUNT * sizeof(label_loc));
    int label_count = 0;
    source src;
    src.offset = 0;
    src.src = (char*)malloc(sizeof(char) * MAX_SRC_LEN);

    if (!mem || !labels || !imem || !src.src) {
        printf("Memory allocation failed\n");
        exit(2);
    }

    for (int i = 0; i < DATA_OFFSET / 4; i++) {
        imem[i].op = UNIMPL;
        imem[i].a1.type = OPTYPE_NONE;
        imem[i].a2.type = OPTYPE_NONE;
        imem[i].a3.type = OPTYPE_NONE;
    }

    parse(fin, mem, imem, memoff, labels, label_count, &src);
    normalize_labels(imem, labels, label_count, &src);

    translate_to_machine_code(mem, imem, path);
    printf("translation done!\n");

    // execute(mem, imem, labels, label_count, start_immediate);
    // printf("Execution done!\n");

    delete path;
    return 0;
}
