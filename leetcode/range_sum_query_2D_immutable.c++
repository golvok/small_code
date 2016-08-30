#include <vector>
#include <numeric>
#include <algorithm>
#include <map>
#include <iostream>
#include <unordered_map>
#include <cmath>

using namespace std;

#define MY_DEBUG










#ifdef MY_DEBUG
    #define DEBUG_STMT(x) x
#else
    #define DEBUG_STMT(x)
#endif

size_t combine_hashes(size_t seed, size_t v) {
    return seed ^ (v + 0x9e3779b9 + (seed << 6) + (seed >> 2));
}

struct Rect {
    size_t t, b, l, r;
    size_t area() const { return width() * height(); };
    size_t width() const { return l - r + 1; }
    size_t height() const { return b - t + 1; }

    bool operator==(const Rect& rhs) const {
        return (t == rhs.t) && (b == rhs.b) && (l == rhs.l) && (r == rhs.r);
    }
};

ostream& operator<<(ostream& os, const Rect& r) {
    os << "{(" << r.t << ',' << r.l << ") , (" << r.b << ',' << r.r << ")}";
    return os;
}

namespace std {
    template<>
    struct hash<Rect> {
        size_t operator()(const Rect& r) const {
            return combine_hashes(
                combine_hashes(std::hash<size_t>()(r.t), std::hash<size_t>()(r.b)),
                combine_hashes(std::hash<size_t>()(r.l), std::hash<size_t>()(r.r))
            );
        }
    };
}

using Matrix = vector<vector<int>>;

int sumUnder(const Rect& r, const Matrix& matrix) {
    int sum = 0;
    for (size_t row = r.t; row != r.b; ++row) {
        const auto& row_vec = matrix[row];
        sum += std::accumulate(begin(row_vec)+r.l, begin(row_vec)+r.r, 0);
    }

    DEBUG_STMT( std::cout << "sum under " << r << " is " << sum << '\n'; )
    return sum;
}

int sumUnderRowSlice(size_t row, size_t cstart, size_t cstop, const Matrix& matrix) {
    const auto& row_vec = matrix[row];
    auto sum = std::accumulate(begin(row_vec)+cstart, begin(row_vec)+cstop+1, 0);
    // DEBUG_STMT((std::cout << "slice sum of " << Rect{row, row, cstart, cstop} << " is " << sum << '\n');)
    return sum;
}

const double SUPERGRID_CONTSANT = 2.0;

template<typename T>
T nearest_supergrid_length_less(T num) {
    return pow(SUPERGRID_CONTSANT, floor(log(num)/log(SUPERGRID_CONTSANT)));
}

template<typename T>
size_t num_supergrid_levels_between(T start, T stop) {
    size_t count = 0;
    while (true) {
        start *= SUPERGRID_CONTSANT;
        if (start >= stop) {
            break;
        }
        ++count;
    }
    return count;
}

Matrix createSumFromMatrixFromMatrix(
    const size_t rstart, const size_t rstop, const ptrdiff_t rstep,
    const size_t cstart, const size_t cstop, const ptrdiff_t cstep,
    const size_t mat_height, const size_t mat_width,
    const Matrix& matrix
) {
    Matrix sum_from_matrix;

    if (mat_height == 0 || mat_width == 0) {
        return sum_from_matrix;
    }

    sum_from_matrix.resize(mat_height);

    // init first row - corner case
    sum_from_matrix[rstart].resize(mat_width);
    sum_from_matrix[rstart][cstart] = matrix[rstart][cstart];
    for (size_t col = (cstart+cstep); col != cstop+cstep; col += cstep) {
        sum_from_matrix[rstart][col] = sum_from_matrix[rstart][col-cstep] + matrix[rstart][col];
    }

    // init rest
    for (size_t row = (rstart+rstep); row != rstop+rstep; row += rstep) {
        sum_from_matrix[row].resize(mat_width);
        sum_from_matrix[row][cstart] = sum_from_matrix[row-rstep][cstart] + matrix[row][cstart]; // init first element - corner case
        for (size_t col = (cstart+cstep); col != cstop+cstep; col += cstep) {
            sum_from_matrix[row][col] = sum_from_matrix[row-rstep][col] + sum_from_matrix[row][col-cstep] - sum_from_matrix[row-rstep][col-cstep] + matrix[row][col];
        }
    }

    return sum_from_matrix;
}

class NumMatrix {
public:
    const size_t mat_height;
    const size_t mat_width;

    size_t max_supergrid_size;

    Matrix matrix;
    const size_t min_supergrid_size_to_sum = 1;
    const int LEVELROWCOL_UNINIT_VALUE = std::numeric_limits<int>::min();

    std::unordered_map<Rect,int> rect2sum;
    std::vector<std::vector<std::vector<int>>> levelrowcol2sum;

    const bool constant_lookup_mode;


    const Matrix sum_from_bottom_right;
    const Matrix sum_from_bottom_left;
    const Matrix sum_from_top_right;
    const Matrix sum_from_top_left;

    const int total_matrix_sum;

    NumMatrix(vector<vector<int>>& _matrix, bool _constant_lookup_mode = true)
        : mat_height(_matrix.size())
        , mat_width(mat_height == 0 ? 0 : _matrix[0].size())
        , max_supergrid_size(0)
        , matrix(std::move(_matrix))
        , rect2sum()
        , levelrowcol2sum()
        , constant_lookup_mode(_constant_lookup_mode)
        , sum_from_bottom_right(createSumFromMatrixFromMatrix(
            mat_height-1, 0, -1,
            mat_width-1,  0, -1,
            mat_height, mat_width,
            matrix
        ))
        , sum_from_bottom_left(createSumFromMatrixFromMatrix(
            mat_height-1, 0,           -1,
            0,            mat_width-1, +1,
            mat_height, mat_width,
            matrix
        ))
        , sum_from_top_right(createSumFromMatrixFromMatrix(
                      0,  mat_height-1, +1,
            mat_width-1,  0,            -1,
            mat_height, mat_width,
            matrix
        ))
        , sum_from_top_left(createSumFromMatrixFromMatrix(
            0, mat_height-1, +1,
            0, mat_width-1,  +1,
            mat_height, mat_width,
            matrix
        ))
        , total_matrix_sum(sumUnder(Rect{0, mat_height, 0, mat_width}, matrix))
    {
        if (constant_lookup_mode) {
            // do nothing?
        } else {
            max_supergrid_size = nearest_supergrid_length_less(mat_width);

            // const auto num_supergrid_levels = 2 + num_supergrid_levels_between(min_supergrid_size_to_sum, max_supergrid_size);
            
            levelrowcol2sum.resize(max_supergrid_size+1);
            for (size_t index = min_supergrid_size_to_sum; index <= max_supergrid_size; index *= SUPERGRID_CONTSANT) {
                levelrowcol2sum[index].resize(mat_height);
                for (auto& rowvec : levelrowcol2sum[index]) {
                    rowvec.resize(mat_width, LEVELROWCOL_UNINIT_VALUE);
                }
            }
        }
    }

    int sumRegion(size_t row1, size_t col1, size_t row2, size_t col2) {
        if (constant_lookup_mode) {
            int sum = total_matrix_sum;
            if (row1 != 0) {
                sum -= sum_from_top_right[row1-1][col1];
            }
            if (col2+1 != mat_width) {
                sum -= sum_from_bottom_right[row1][col2+1];
            }
            if (row2+1 != mat_height) {
                sum -= sum_from_bottom_left[row2+1][col2];
            }
            if (col1 != 0) {
                sum -= sum_from_top_left[row2][col1-1];
            }
            return sum;
        } else {
            int sum = 0;
            for (size_t row = row1; row != row2+1; ++row) {
                sum += sumSlice(row, col1, col2);
            }
            return sum;
        }
    }

    // int sumRegion(const Rect& r) {
    //     if (r.width() > 8) {

    //     }
    //     return sumUnder(r, matrix);
    // }

    int sumSlice(size_t row, size_t cstart, size_t cstop) {
        const size_t row_width = cstop - cstart + 1;
        const Rect slice_rect{row, row, cstart, cstop};
        (void)slice_rect;

        if (row_width < min_supergrid_size_to_sum) {
            int sum = sumUnderRowSlice(row, cstart, cstop, matrix);
            DEBUG_STMT(std::cout << "sumSlice: " << slice_rect << "'s sum is " << sum << '\n';)
            return sum;
        }

        const auto max_supergrid_length = nearest_supergrid_length_less(row_width);
        const auto first_supergridline_after_cstart = (cstart/max_supergrid_length + 1)*max_supergrid_length;

        
        const int sum = [&]() {
            if (cstart % max_supergrid_length == 0 && row_width == max_supergrid_length) {
                // case 0: a slice on a boundary, that is the length of that gridsize
                return sumSliceCached(row, cstart, cstop);
            } else if ((first_supergridline_after_cstart + max_supergrid_length) > cstop) {
                // case 1: doesn't fit, so try next smaller
                if (first_supergridline_after_cstart > cstop || max_supergrid_length == min_supergrid_size_to_sum) {
                    return sumUnderRowSlice(row, cstart, cstop, matrix);
                } else {
                    return 0
                        + sumSlice(row, cstart, first_supergridline_after_cstart-1)
                        + sumSlice(row, first_supergridline_after_cstart, cstop)
                    ;
                }
            } else {
                // case 2: a single grid section fits!
                return 0
                    + sumSlice(row, cstart, first_supergridline_after_cstart-1)
                    + sumSlice(row, first_supergridline_after_cstart, first_supergridline_after_cstart + max_supergrid_length - 1)
                    + sumSlice(row, first_supergridline_after_cstart + max_supergrid_length, cstop)
                ;
            }
        }();

        DEBUG_STMT(std::cout << "sumSlice: " << slice_rect << "'s sum is " << sum << '\n';)
        return sum;
    }

    int sumSliceCached(size_t row, size_t cstart, size_t cstop) {
        const Rect slice_rect{row, row, cstart, cstop};
        (void)slice_rect;

        const auto& width = cstop - cstart + 1;
        auto& cache_results = getCachedValue(width, row, cstart);
        if (cache_results != LEVELROWCOL_UNINIT_VALUE) {
            return cache_results;
        }

        const int sum = [&]() {
            if (width <= min_supergrid_size_to_sum) {
                return sumUnderRowSlice(row, cstart, cstart, matrix);
            } else {
                return 0
                    + sumSliceCached(row, cstart, cstop - width/2)
                    + sumSliceCached(row, cstart + width/2, cstop)
                ;
            }
        }();

        DEBUG_STMT(std::cout << "sumSliceCached: " << slice_rect << "'s sum is " << sum << '\n';)
        cache_results = sum;
        return sum;
    }

    int& getCachedValue(size_t level, size_t row, size_t col) {
        return levelrowcol2sum[level][row][col];
        // Rect r{row, row, col , col+level};
        // auto it = rect2sum.find(r);
        // if (it == rect2sum.end()) {
        //     rect2sum.emplace(r, LEVELROWCOL_UNINIT_VALUE);
        // }

        // return rect2sum[r];
    }
};


// Your NumMatrix object will be instantiated and called as such:
// NumMatrix numMatrix(matrix);
// numMatrix.sumRegion(0, 1, 2, 3);
// numMatrix.sumRegion(1, 2, 3, 4);






NumMatrix* make_num_matrix(std::vector<std::vector<int>>& matrix) {
    return new NumMatrix(matrix, true);
}

int sumRegion(NumMatrix& nm, int rstart, int cstart, int rstop, int cstop) {
    return nm.sumRegion(rstart, cstart, rstop, cstop);
}
