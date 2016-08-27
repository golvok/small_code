#include <vector>
#include <numeric>
#include <algorithm>
#include <map>
#include <iostream>
#include <unordered_map>
#include <cmath>

using namespace std;

#define MY_DEBUG
#define PRINT_NUMS










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
    for (size_t row = r.t; row != r.b+1; ++row) {
        const auto& row_vec = matrix[row];
        sum += std::accumulate(begin(row_vec)+r.l, begin(row_vec)+r.r+1, 0);
    }
    return sum;
}

int sumUnderRowSlice(size_t row, size_t cstart, size_t cstop, const Matrix& matrix) {
    const auto& row_vec = matrix[row];
    auto sum = std::accumulate(begin(row_vec)+cstart, begin(row_vec)+cstop+1, 0);
    // DEBUG_STMT((std::cout << "slice sum of " << Rect{row, row, cstart, cstop} << " is " << sum << '\n');)
    return sum;
}

template<typename T>
T nearest_power_of_two_less(T num) {
    return pow(2, floor(log(num)/log(2)));
}

class NumMatrix {
public:
    size_t max_supergrid_size;
    Matrix matrix;
    const size_t min_supergrid_size_to_sum = 8;
    unordered_map<Rect, int> rect2sum;

    NumMatrix(vector<vector<int>>& matrix)
        : max_supergrid_size(matrix.size() == 0 ? 0 : nearest_power_of_two_less(matrix[0].size()))
        , matrix(std::move(matrix))
        , rect2sum()
    { }

    int sumRegion(int row1, int col1, int row2, int col2) {
        int sum = 0;
        for (int row = row1; row != row2+1; ++row) {
            sum += sumSlice(row, col1, col2);
        }
        return sum;
    }

    // int sumRegion(const Rect& r) {
    //     if (r.width() > 8) {

    //     }
    //     return sumUnder(r, matrix);
    // }

    int sumSlice(size_t row, size_t cstart, size_t cstop) {
        const size_t row_width = cstop - cstart + 1;
        const Rect slice_rect{row, row, cstart, cstop};

        if (row_width < min_supergrid_size_to_sum) {
            int sum = sumUnderRowSlice(row, cstart, cstop, matrix);
            DEBUG_STMT(std::cout << "sumSlice: " << slice_rect << "'s sum is " << sum << '\n';)
            return sum;
        }

        const auto cache_results = rect2sum.find(slice_rect);
        if (cache_results != rect2sum.end()) {
            DEBUG_STMT(std::cout << "sumSlice: " << slice_rect << "'s sum is " << cache_results->second << '\n';)
            return cache_results->second;
        }

        const auto max_supergrid_length = nearest_power_of_two_less(row_width);
        const auto first_supergridline_after_cstart = (cstart/max_supergrid_length + 1)*max_supergrid_length;
        
        const int sum = [&]() {
            if ((first_supergridline_after_cstart + max_supergrid_length) > cstop) {
                // case 1: doesn't fit, so try next smaller
                if (first_supergridline_after_cstart > cstop) {
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

        rect2sum[slice_rect] = sum;

        DEBUG_STMT(std::cout << "sumSlice: " << slice_rect << "'s sum is " << sum << '\n';)
        return sum;
    }
};


// Your NumMatrix object will be instantiated and called as such:
// NumMatrix numMatrix(matrix);
// numMatrix.sumRegion(0, 1, 2, 3);
// numMatrix.sumRegion(1, 2, 3, 4);






NumMatrix* make_num_matrix(std::vector<std::vector<int>>& matrix) {
    return new NumMatrix(matrix);
}

int sumRegion(NumMatrix& nm, int rstart, int cstart, int rstop, int cstop) {
    return nm.sumRegion(rstart, cstart, rstop, cstop);
}
