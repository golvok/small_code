#include <stdlib.h>

struct NumMatrix {
    int* memory;
    int** row_ptrs;
};

/** Initialize your data structure here. */
struct NumMatrix* NumMatrixCreate(int** matrix, int mat_width, int mat_height) {
    struct NumMatrix* nm = malloc(sizeof (struct NumMatrix));
    nm->memory = NULL;
    nm->row_ptrs = NULL;

    const int rstep = +1;
    const int cstep = +1;
    
    const size_t rstart = 0;
    const size_t rstop = mat_height-1;
    const size_t cstart = 0;
    const size_t cstop = mat_width-1;
    
    const size_t true_rstart = rstart+1;
    const size_t true_rstop = rstop+1;
    const size_t true_cstart = cstart+1;
    const size_t true_cstop = cstop+1;
    const size_t true_mat_height = mat_height + 2;
    const size_t true_mat_width = mat_width + 2;

    if (mat_width == 0 || mat_height == 0) {
        return nm;
    }
    
    nm->memory = malloc(sizeof(int)* (true_mat_width*true_mat_height));
    nm->row_ptrs = malloc(sizeof(int*) * true_mat_height);
    for (size_t row = 0; row != true_mat_height; ++row) {
        nm->row_ptrs[row] = nm->memory + row * true_mat_width;
    }

    // init first row to zeroes - corner case
    for (size_t col = 0; col != true_mat_width; ++col) {
        nm->row_ptrs[true_rstart-rstep][col] = 0;
    }

    // init rest
    for (size_t row = true_rstart; row != true_rstop+rstep; row += rstep) {
        nm->row_ptrs[row][true_cstart-cstep] = 0; // init first element - corner case
        for (size_t col = true_cstart; col != true_cstop+cstep; col += cstep) {
            nm->row_ptrs[row][col] = nm->row_ptrs[row-rstep][col] + nm->row_ptrs[row][col-cstep] - nm->row_ptrs[row-rstep][col-cstep] + matrix[row-1][col-1];
        }
    }
    
    return nm;
}

int sumRegion(struct NumMatrix* numMatrix, int rstart, int cstart, int rstop, int cstop) {
    return 0
        + numMatrix->row_ptrs[1+ rstop  ][1+ cstop  ]
        - numMatrix->row_ptrs[1+ rstop  ][1+ cstart-1]
        - numMatrix->row_ptrs[1+ rstart-1][1+ cstop  ]
        + numMatrix->row_ptrs[1+ rstart-1][1+ cstart-1]
    ;
}

/** Deallocates memory previously allocated for the data structure. */
void NumMatrixFree(struct NumMatrix* numMatrix) {
    free(numMatrix->memory);
    free(numMatrix->row_ptrs);
    free(numMatrix);
}

// Your NumMatrix object will be instantiated and called as such:
// struct NumMatrix* numMatrix = NumMatrixCreate(matrix, matrixRowSize, matrixColSize);
// sumRegion(numMatrix, 0, 1, 2, 3);
// sumRegion(numMatrix, 1, 2, 3, 4);
// NumMatrixFree(numMatrix);