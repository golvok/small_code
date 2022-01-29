#include <hunspell/hunspell.hxx>

#include <array>
#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
#include <ranges>
#include <unordered_map>
#include <vector>

namespace {
constexpr auto kletterRange = std::views::iota(0, 5);

struct App {

/* Notes:
- "saaes" is the 'perfect' first guess
- ~ half of words have a vowel as the second letter
*/

struct GuessResult {
	int green = 0;
	int yellow = 0;
	friend std::ostream& operator<<(std::ostream& os, const GuessResult& gr) {
		return os << "{g=" << gr.green << " y=" << gr.yellow << "}";
	}
};

struct FalseInitBool {
	bool b = false;
	operator bool() const { return b; }
	bool operator<=>(const FalseInitBool&) const = default;
	FalseInitBool& operator|=(bool rhs) { b |= rhs; return *this; }
};

struct ZeroInitInt {
	int i = 0;
	operator int() const { return i; }
	auto operator<=>(const ZeroInitInt&) const = default;
	ZeroInitInt& operator+=(int rhs) { i += rhs; return *this; }
	ZeroInitInt& operator++() { ++i; return *this; }
};

static int main() {
	const auto dictionary_path = "/usr/share/hunspell/en_GB-large.dic";
	const auto affix_path = "/usr/share/hunspell/en_GB-large.aff";
	auto speller = Hunspell(affix_path, dictionary_path);

	const auto guesses = std::vector<std::pair<std::string, std::string>>{
		// {"asset", "yybbb"},
	};

	const auto dictionary = readWords(speller, dictionary_path);
	// const auto dictionary = std::vector<std::string>({"paces"});
	std::cout << "dict N=" << dictionary.size() << '\n';
	const auto words = filterWordsOnGuesses(guesses, dictionary);
	const int num_words = words.size();

	const auto word_scorer = [&] (const auto& i_word, const GuessResult& gr) {
		(void)i_word;
		auto g = double(gr.green);
		auto y = double(gr.yellow);
		// std::array<bool, 26> seen;
		// seen.fill(false);
		// for (auto c : words[i_word]) {
		// 	if (seen[c - 'a']) return -1;
		// 	seen[c - 'a'] = true;
		// }
		auto score = g/num_words + std::pow(y/num_words, 1.5);
		return score;
	};

	analyze(words, word_scorer);
	return 0;
}

static void analyze(const auto& words, const auto& word_scorer) {
	const int num_words = words.size();
	std::cout << "N=" << num_words << '\n';

	const auto pipe_percentile = 0.5;
	std::cout << "\n == Frequency Order by Position (pipe at " << pipe_percentile << ") ==\n";
	std::array<std::map<char, int>, 5> counts;
	for (const auto& word : words) {
		for (auto i : kletterRange) {
			counts[i][word[i]]++;
		}
	}
	for (auto i : kletterRange) {
		std::vector<std::pair<char, int>> pos_counts(counts[i].begin(), counts[i].end());
		std::sort(pos_counts.begin(), pos_counts.end(), [](auto&& lhs, auto&& rhs) { return lhs.second > rhs.second; });
		int running_total = 0;
		for (const auto& [c, count] : pos_counts) {
			running_total += count;
			std::cout << c << ' ';
			if (running_total > num_words * pipe_percentile) {
				std::cout << "| ";
				running_total *= -num_words;  // never print the pipe again
			}
		}
		std::cout << '\n';
	}

	std::cout << "\n == Top Scoring Words ==\n";
	std::vector<std::pair<int, GuessResult>> guess_result_totals;
	for (auto& guess : words) {
		GuessResult result;
		for (auto& target : words) {
			std::array<bool, 5> target_letter_hinted;
			target_letter_hinted.fill(false);
			GuessResult target_result;
			for (auto i_g : kletterRange) {
				if (guess[i_g] == target[i_g]) {
					target_result.green++;
					target_letter_hinted[i_g] = true;
					continue;
				}

				for (auto i_t : kletterRange) {
					if (target_letter_hinted[i_t]) continue;
					if (guess[i_g] == target[i_t]) {
						target_result.yellow++;
						target_letter_hinted[i_g] = true;
						break;
					}
				}
			}
			result.green += target_result.green;
			result.yellow += target_result.yellow;
		}
		result.green -= 5; // the self-comparison
		guess_result_totals.emplace_back(guess_result_totals.size(), std::move(result));
	}
	std::sort(
		guess_result_totals.begin(), guess_result_totals.end(),
		[&](auto&& lhs, auto&& rhs) {
			return word_scorer(lhs.first, lhs.second) > word_scorer(rhs.first, rhs.second);
		}
	);
	for (const auto& [i_word, guess_result_total] : guess_result_totals | std::views::take(7)) {
		std::cout << words[i_word] << " with " << guess_result_total << " (score=" << word_scorer(i_word, guess_result_total) << ")\n";
	}
}

static std::vector<std::string> readWords(Hunspell& speller, std::string_view path) {
	std::vector<std::string> words;

	auto dict_in = std::ifstream(std::string(path));
	while (not dict_in.eof()) {
		std::string line;
		std::getline(dict_in, line, '\n');
		auto slash_pos = line.find_first_of('/');
		line.erase(std::min(slash_pos, line.size()), line.npos);
		auto with_suffixes = speller.suffix_suggest(line);
		with_suffixes.push_back(line);
		for (auto& word : with_suffixes) {
			if (word.size() != 5) continue;
			if (std::any_of(word.begin(), word.end(), [](auto c) { return c < 'a' || 'z' < c; })) {
				continue;
			}
			words.push_back(std::move(word));
		}
	}

	std::sort(words.begin(), words.end());
	words.erase(std::unique(words.begin(), words.end()), words.end());

	return words;
}

static std::vector<std::string> filterWordsOnGuesses(const auto& guesses, const auto& dictionary) {
	const auto letter_count_requirements = [](const auto& guesses) {
		std::array<ZeroInitInt, 256> requirements;
		for (const auto& [word, clues] : guesses) {
			std::array<ZeroInitInt, 256> requirements_from_guess;
			for (auto i : kletterRange) {
				auto c = word[i];
				requirements_from_guess[c] += (clues[i] == 'y' || clues[i] == 'g');
			}
			for (size_t i = 0; i < requirements.size(); ++i) {
				requirements[i] = std::max(requirements[i], requirements_from_guess[i]);
			}
		}
		return requirements;
	}(guesses);

	// for (size_t i = 0; i < letter_count_requirements.size(); ++i) {
	// 	if (letter_count_requirements[i] > 0) {
	// 		std::cout << "require " << letter_count_requirements[i] << " of " << (char)i << '\n';
	// 	}
	// }

	const auto banned_letters = [](const auto& guesses, const auto& letter_count_requirements) {
		std::array<FalseInitBool, 256> banned;
		for (const auto& [word, clues] : guesses) {
			for (auto i : kletterRange) {
				auto c = word[i];
				banned[c] |= (clues[i] == 'b') && (letter_count_requirements[c] == 0);
			}
		}
		return banned;
	}(guesses, letter_count_requirements);

	// for (size_t i = 0; i < banned_letters.size(); ++i) {
	// 	if (banned_letters[i]) std::cout << (char)i << " is banned\n";
	// }

	const auto banned_at_position = [](const auto& guesses) {
		std::array<std::array<FalseInitBool, 256>, 5> banned;
		for (const auto& [word, clues] : guesses) {
			for (auto i : kletterRange) {
				auto c = word[i];
				banned[i][c] |= clues[i] == 'y' || clues[i] == 'b';
			}
		}
		return banned;
	}(guesses);

	const auto required_at_position = [](const auto& guesses) {
		std::array<char, 5> required;
		required.fill('\0');
		for (const auto& [word, clues] : guesses) {
			for (auto i : kletterRange) {
				auto c = word[i];
				if (clues[i] == 'g') required[i] = c;
			}
		}
		return required;
	}(guesses);

	const auto word_allowed = [&](const auto& word) {
		std::array<ZeroInitInt, 256> counts;
		for (auto i : kletterRange) {
			auto c = word[i];
			counts[c] += 1;
			if (required_at_position[i] != '\0' && required_at_position[i] != c) return false;
			if (banned_letters[c]) return false;
			if (banned_at_position[i][c]) return false;
		}
		for (size_t i = 0; i < letter_count_requirements.size(); ++i) {
			if ('a' <= i && i <= 'z' && counts[i] > 0) {
				// std::cout << (char)i << ' ' << letter_count_requirements[i] << " > " << counts[i] << '\n';
			}
			if (letter_count_requirements[i] > counts[i]) {
				// std::cout << "skipping " << word << " becase of " << (char)i << '\n';
				return false;
			}
		}
		// std::cout << word << " OK\n";
		return true;
	};

	std::vector<std::string> words;
	std::copy_if(dictionary.begin(), dictionary.end(), std::back_inserter(words), word_allowed);
	return words;
}

};
}

int main() {
	return App::main();
}
