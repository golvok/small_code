#include <sstream>

/**
 * ex:
 * using namespace stringstream_string_builder
 * const std::string abc1 = "abc"_sb << 1;
 * const std::string abc2 = sb() << "abc" << 1;
 */
namespace stringstream_string_builder {
    struct stringstream_string_builder {
        operator std::string() { return ss.str(); }

        template<typename T>
        stringstream_string_builder& operator<<(const T& t) {
            ss << t;
            return *this;
        }

        auto& underlyingStream() const { return ss; }

    private:
        std::stringstream ss{};
    };

    stringstream_string_builder operator"" _sb(const char* chars, std::size_t) {
        stringstream_string_builder result;
        result << chars;
        return result;
    }

    stringstream_string_builder sb() { return {}; }
}

int main() {
    return 0;
}