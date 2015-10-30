#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <tuple>

template<typename CONTAINER>
struct reversed_adaptor {
    CONTAINER& c;
    reversed_adaptor(CONTAINER& c) : c(c) { }

    auto begin() { return c.rbegin(); }
    auto end() { return c.rend(); }
};

template<typename CONTAINER>
auto reversed(CONTAINER& c) {
    return reversed_adaptor<CONTAINER>(c);
}

std::pair<std::vector<bool>,bool> first_number_with_n_bits(size_t num_bits, size_t num_high_bits) {
    std::vector<bool> result(num_bits,false);
    std::fill_n(result.begin(), num_high_bits, true);
    return {result, num_high_bits <= num_bits};
}

std::pair<std::vector<bool>,bool> next_number_with_same_number_of_bits(std::vector<bool>&& gen_number) {

    auto first_one = std::find(gen_number.begin(), gen_number.end(), true);
    auto next_zero = std::find(first_one, gen_number.end(), false);
    size_t run_size = std::distance(first_one,next_zero);

    if (next_zero != gen_number.end()) {
        *next_zero = 1;
    }

    std::fill_n(gen_number.begin(), (run_size-1), true);
    std::fill(gen_number.begin() + (run_size-1), next_zero, false);

    return {gen_number,next_zero != gen_number.end()};
}

int main(int arc, char** arcv) {

    if (arc < 3) {
        std::cerr << "pleas give two arguments: number of bits to be in the numbers, and number of high bits\n";
        return 1;
    }

    const size_t NUM_SIZE = std::strtoull(arcv[1],nullptr,10);
    const size_t NUM_HIGH_BITS = std::strtoull(arcv[2],nullptr,10);

    if (NUM_SIZE < NUM_HIGH_BITS) {
        std::cerr << "number of bits in number must not be less than the number of high bits\n";
        return 2;
    }

    std::vector<std::vector<bool>> gen_numbers;

    for (
        auto gen_number_pair = first_number_with_n_bits(NUM_SIZE, NUM_HIGH_BITS);
        gen_number_pair.second == true;
        gen_number_pair = next_number_with_same_number_of_bits(std::move(gen_number_pair.first))
    ) {
        gen_numbers.push_back(gen_number_pair.first);
    }

    std::vector<bool> number(NUM_SIZE);
    number[0] = true;

    while (true) {

        size_t num_ones = std::count_if(number.begin(),number.end(),[](const auto& bit) {
            return bit;
        });

        if (num_ones == NUM_HIGH_BITS) {
            for (const auto& bit : reversed(number)) {
                std::cout << (bit ? '1' : '0');
            }

            std::cout << " : ";

            for (const auto& bit : reversed(gen_numbers.front())) {
                std::cout << (bit ? '1' : '0');
            }

            if (std::mismatch(number.begin(),number.end(), gen_numbers.front().begin()).first != number.end()) {
                std::cout << " !!BAD!!";
            }
            gen_numbers.erase(gen_numbers.begin());

            std::cout << '\n';
        }

        if (num_ones == number.size()) {
            break;
        }

        auto first_zero = std::find(number.begin(), number.end(), false);
        std::for_each(number.begin(),first_zero,[](auto bit) {
            bit = 0;
        });
        if (first_zero != number.end()) {
            *first_zero = 1;
        }
    }
}
