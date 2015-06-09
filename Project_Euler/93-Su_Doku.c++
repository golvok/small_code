void end_func() {}

#include "common.h"
#include <bitset>

const bool FALIURE = false;
const bool SUCCESS = true;

enum class Orientation {
	HORIZONTAL,
	VERTICAL,
};

enum class SudokuOperations {
	SET_TO,
	REMOVE_GUESS
};



class SetOfNine {
public:
	enum class Type {
		ROW,
		COL,
		BOX,
	};

	static const uint DIMENSION_T = 9;
	static const uint DIMENSION; // set to DIMENSION_T below...

private:
	std::bitset<DIMENSION_T> numbers_in_set;
	std::array<uint,DIMENSION_T> candidate_counts; // so you can tell if there is only one place for a number.
	Type type;
	uint index;

public:
	SetOfNine(Type type, uint index)
		: numbers_in_set()
		, candidate_counts()
		, type(type)
		, index(index)
	{
		candidate_counts.fill(SetOfNine::DIMENSION);
	}

	SetOfNine(const SetOfNine&& src)
		: numbers_in_set(std::move(src.numbers_in_set))
		, candidate_counts(std::move(src.candidate_counts))
		, type(src.type)
		, index(src.index)
	{ }

	SetOfNine(const SetOfNine& src)
		: numbers_in_set(src.numbers_in_set)
		, candidate_counts(src.candidate_counts)
		, type(src.type)
		, index(src.index)
	{ }

	bool hasNumber(uint num) const {
		return numbers_in_set[num-1] == true;
	}

	bool setHasNumber(uint num) {
		if (hasNumber(num)) {
			assert("this so9 already has that number!");
			return FALIURE;
		} else {
			numbers_in_set[num-1] = true;
			candidate_counts[num-1] = 0;
			return SUCCESS;
		}
	}

	void decrementCandidateCountOf(uint num) {
		uint& count = candidate_counts[num-1];
		if (count == 0) {
			// do nothing
		} else if (count == 1) {
			assert(0 && "decrement to 0!? - that's a problem");
		} else {
			count -= 1;
		}
	}

	uint getCandidateCount(uint num) const {
		return candidate_counts[num-1];
	}

	Type getType() const {
		return type;
	}

	const std::bitset<DIMENSION_T>& getNumbersInSet() const {
		return numbers_in_set;
	}

	size_t print(std::ostream& os, Orientation o, size_t line) const {
		switch (o) {
			case Orientation::HORIZONTAL:
				os << "{ ";
				for (size_t i = 0; i < numbers_in_set.size(); ++i) {
					if (numbers_in_set[i] == false) {
						os << (i+1) << '@' << getCandidateCount(i+1) << ' ';
					}
				}
				os << '}';
				return 0;
			break;
			case Orientation::VERTICAL:
				os << "line" << line; 
				return DIMENSION - line;
			break;
			default:
				assert(0);
				return 0;
		}
	}

	template<class NodeType>
	class SO9iterator : public std::iterator<std::forward_iterator_tag, NodeType> {
	private:
		typename SetOfNine::Type const type;
		Point current;

	public:
		SO9iterator(typename SetOfNine::Type type_, Point current)
			: type(type_)
			, current(current)
		{ }

		SO9iterator(const SO9iterator& src)
			: type(src.type)
			, current(src.current)
		{ }

		const NodeType& operator*() const {
			return current;
		}
		// NodeType* operator->() const;

		SO9iterator& operator++() {
			switch (type) {
				case SetOfNine::Type::ROW:
					current.first += 1;
				break;
				case SetOfNine::Type::COL:
					current.second += 1;
				break;
				case SetOfNine::Type::BOX:
					current.first += 1;
					if (current.first % 3 == 0) {
						current.first -= 3;
						current.second += 1;
					}
				break;
			}
			return *this;
		}
		SO9iterator operator++(int) {
			SO9iterator copy = *this;
			++(*this);
			return copy;
		}

		bool operator==(SO9iterator& rhs) {
			return this->current == rhs.current;
		}

		bool operator!=(SO9iterator& rhs) {
			return !(*this == rhs);
		}

		// one way conversion: iterator -> const_iterator
		operator SO9iterator<NodeType const>() const;
	};

	typedef SO9iterator<Point const> point_iterator;

	point_iterator begin() const {
		Point start;
		switch (type) {
			case SetOfNine::Type::ROW:
				start = {0,index};
			break;
			case SetOfNine::Type::COL:
				start = {index,0};
			break;
			case SetOfNine::Type::BOX:
				start = {(index%3)*3, (index/3)*3};
			break;
		}
		return point_iterator(type, start);
	}

	point_iterator end() const {
		Point end;
		switch (type) {
			case SetOfNine::Type::ROW:
				end = {9,index};
			break;
			case SetOfNine::Type::COL:
				end = {index,9};
			break;
			case SetOfNine::Type::BOX:
				end = {(index%3)*3, (index/3 + 1)*3};
			break;
		}
		return point_iterator(type, end);
	}
};

const uint SetOfNine::DIMENSION = SetOfNine::DIMENSION_T;

union SudokuOperationData {
	struct {
		Point point;
		uint number;
	} PointAndNumber;
};

using SudokuOpDeque = std::deque<std::pair<SudokuOperations,SudokuOperationData>>;

class Possibilities {
	std::bitset<9> set;

	using BitRef = decltype(set)::reference;
public:
	bool isOnlyOne() const {
		unsigned long long x = set.to_ullong();
		// bit hack - detects powers of 2 (ie. one bit set)
		return x!=0 && (x & (x-1))==0;
	}

	uint getLowest() const {
		for (size_t i = 1; i <= 9; ++i) {
			if (test(i) == true) {
				return i;
			}
		}
		return 0;
	}

	Possibilities(const SetOfNine& r, const SetOfNine& c, const SetOfNine& b)
		: set(~r.getNumbersInSet() & ~c.getNumbersInSet() & ~b.getNumbersInSet())
	{ }

	const BitRef operator[](size_t num) const {
		return const_cast<Possibilities&>(*this)[num];
	}

	BitRef operator[](size_t num) {
		return set[num-1];
	}

	bool test(size_t num) const {
		return (*this)[num];
	}
};

class Sudoku {

	// TODO: use container with constexpr .size() for these.
	std::vector<SetOfNine> rows;
	std::vector<SetOfNine> cols;
	std::vector<SetOfNine> boxes;

	std::array<std::array<uint,9>,9> grid;

	SetOfNine& getBox(Point square_in_box) {
		return boxes[square_in_box.first/3 + (square_in_box.second/3)*3];
	}

	SetOfNine& getRow(Point square_in_row) {
		return rows[square_in_row.second];
	}

	SetOfNine& getCol(Point square_in_col) {
		return cols[square_in_col.first];
	}

	uint& setSquare(Point square) {
		return arrayGet(grid,square);
	}

public:
	uint getNumberAt(Point square) const {
		return arrayGet(grid,square);
	}

	bool hasNumberAt(Point square) const {
		return getNumberAt(square) != 0;
	}

	void setNumber(Point square_to_set, uint num_to_set, SudokuOpDeque& op_deque) {
		// std::cout << "setting " << square_to_set << " to " << num_to_set << '\n';

		getBox(square_to_set).setHasNumber(num_to_set);
		getRow(square_to_set).setHasNumber(num_to_set);
		getCol(square_to_set).setHasNumber(num_to_set);

		setSquare(square_to_set) = num_to_set;

		// std::cout << "updated so9s\n";
		// this->print_with_guesses(std::cout);
		
		// iterate over the row, col, and box to:
		//   remove candidate counts in other so9s, to:
		//     see if this has created something unique that so9

		std::array<SetOfNine*,3> square_to_sets_so9s{
			&getRow(square_to_set),
			&getCol(square_to_set),
			&getBox(square_to_set)
		};
		
		const Possibilities& p_of_sts = getPossibilities(square_to_set);
		for (auto& so9 : square_to_sets_so9s) {
			for (uint i = 1; i <= DIMENSION; ++i) {
				if (p_of_sts.test(i)) {
					so9->decrementCandidateCountOf(i);
				}
			}
			for (auto& squ : *so9) {
				if (squ == square_to_set) { // skip this square
					continue;
				}

				const Possibilities& p = getPossibilities(squ);
				if (p.test(num_to_set)) {

					std::array<SetOfNine*,3> squs_so9s{&getRow(squ), &getCol(squ), &getBox(squ)};

					for(auto& squs_so9 : squs_so9s) {
						// this does nothing if that SO9 has that number already.
						// TODO: optimization - did anything change?
						squs_so9->decrementCandidateCountOf(num_to_set);

						if (squs_so9->getCandidateCount(num_to_set) == 1) {
							// find the candidate
							for (auto& point : *squs_so9) {
								if (getPossibilities(point).test(num_to_set)) {
									op_deque.emplace_back(std::make_pair(
										SudokuOperations::SET_TO,
										SudokuOperationData{point, num_to_set}
									));
								}
							}
						}
					}
				}
			}
		}

		// std::cout << "grid is now:\n";
		// this->print_with_guesses(std::cout);

	}

	const Possibilities getPossibilities(Point square) {
		return Possibilities(getRow(square),getCol(square),getBox(square));
	}

	void print_to_stream(std::ostream& os) const {
		os << "╔═══════╦═══════╦═══════╗\n";
		for (size_t row_num = 0; row_num < DIMENSION; ++row_num) {
			if (row_num % 3 == 0 && row_num != 0) {
				os << "╠═══════╬═══════╬═══════╣\n";
			}
			for (size_t col_num = 0; col_num < DIMENSION; ++col_num) {
				if (col_num % 3 == 0) {os << "║ ";}
				uint this_num = getNumberAt(Point{row_num,col_num});
				if (this_num == 0) {
					os << "▫";
				} else {
					os << this_num;
				}
				os << " ";
			}
			os << "║";
			os << "   ";

			rows[row_num].print(os, Orientation::HORIZONTAL, 1);

			os << '\n';
		}
		
		os << "╚═══════╩═══════╩═══════╝\n";
		os << "\n";

		for (size_t col_num = 0; col_num < DIMENSION; ++col_num) {
			for (size_t i = 0; i < col_num; ++i) {
				os << "  ";
				if (i % 3 == 2) {
					os << "  ";
				}
			}
			cols[col_num].print(os, Orientation::HORIZONTAL, 1);
			os << "\n";
		}
	}

	void print_with_guesses(std::ostream& os) {
		os << "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗\n";
		for (size_t row_num = 0; row_num < DIMENSION; ++row_num) {
			if (row_num % 3 == 0 && row_num != 0) {
				os << "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣\n";
			}
			for (size_t i = 0; i < DIMENSION_SQRT; ++i) {
				for (size_t col_num = 0; col_num < DIMENSION; ++col_num) {
					Point current_square{row_num,col_num};

					if (col_num % 3 == 0) {
						os << "║";
					}
				
					for (size_t j = 0; j < DIMENSION_SQRT; ++j) {
						uint test_possibility_num = i*3+j+1;
						if (hasNumberAt(current_square)) {
							// if this square has been set
							os << getNumberAt(current_square);
						} else if (getPossibilities(current_square).test(test_possibility_num)) {
							os << test_possibility_num;
						} else {
							os << ' ';
						}
					}

					if (col_num % 3 != 2) {
						os << "│";
					}
				}
				os << "║";
				os << "   ";

				os << '\n';
			}
			if (row_num % 3 != 2) {
				os << "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n";
			}
		}
		
		os << "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝\n";
		os << "\n";		
	}

	Sudoku()
		: rows()
		, cols()
		, boxes()
		, grid()
	{
		rows.reserve(DIMENSION);
		for (uint i = 0; i < DIMENSION; ++i) {
			rows.emplace_back(SetOfNine::Type::ROW, i);
		}
		cols.reserve(DIMENSION);
		for (uint i = 0; i < DIMENSION; ++i) {
			cols.emplace_back(SetOfNine::Type::COL, i);
		}
		boxes.reserve(DIMENSION);
		for (uint i = 0; i < DIMENSION; ++i) {
			boxes.emplace_back(SetOfNine::Type::BOX, i);
		}
	}

	class iterator : public std::iterator<std::forward_iterator_tag, Point> {
	private:
		Point current;
	public:
		iterator(const Point& start)
			: current(start)
		{}

		iterator(const iterator& src)
			: current(src.current)
		{}

		bool operator==(const iterator& rhs) const {
			return current == rhs.current;
		}

		bool operator!=(const iterator& rhs) const {
			return !(*this == rhs);
		}

		iterator& operator++() {
			current.first++;
			if (current.first == DIMENSION) {
				current.first = 0;
				current.second++;
			}
			return *this;
		}

		const Point& operator*() const {
			return current;
		}
	};

	Sudoku::iterator begin() const {
		return Sudoku::iterator(Point{0,0});
	}

	Sudoku::iterator end() const {
		return Sudoku::iterator(Point{0,9});
	}

	static const uint DIMENSION = 9;
	static const uint DIMENSION_SQRT = 3;

};

std::ostream& operator<<(std::ostream& os, const Sudoku& s) {
	s.print_to_stream(os);
	return os;
}

std::vector<uint> solve(std::vector<uint>& grid) {

	Sudoku sudoku;

	SudokuOpDeque op_deque;
	
	for (size_t i = 0; i < 9; ++i) {
		for (size_t j = 0; j < 9; ++j) {
			uint grid_number = grid[i*9 + j];
			if (grid_number != 0) {
				op_deque.emplace_back(std::make_pair(
					SudokuOperations::SET_TO,
					SudokuOperationData{Point{i,j},grid_number}
				));
			}
		}
	}

	while (op_deque.empty() == false) {
		// update guesses.
		while (op_deque.empty() == false) {
			auto& op_and_data = op_deque.front();
			SudokuOperations& op = op_and_data.first;
			SudokuOperationData& data = op_and_data.second;
			switch (op) {
				case SudokuOperations::SET_TO:
					sudoku.setNumber(data.PointAndNumber.point, data.PointAndNumber.number, op_deque);
				break;
				case SudokuOperations::REMOVE_GUESS:
					assert(0 && "to do?");
				break;
			}
			op_deque.pop_front();
		}

		// do logic
		// eg. only places for number in one SO9 are aso all in one other SO9
			// boxes in particular
		// hidden twin, naked twin, triplets...
		for (Point squ : sudoku) {
			// std::cout << "looking at " << squ << "\n";
			const Possibilities& p = sudoku.getPossibilities(squ);
			if (p.isOnlyOne() && !sudoku.hasNumberAt(squ)) {
				uint the_one = p.getLowest();
				// std::cout << squ << " can only be " << the_one << "\n";
				op_deque.emplace_back(std::make_pair(
					SudokuOperations::SET_TO,
					SudokuOperationData{squ,the_one}
				));
			}
		}

		std::cout << "grid is now:\n";
		sudoku.print_with_guesses(std::cout);
	}

	end_func();

	std::cout << sudoku;

	return {};
}

void get_input_and_solve() {
	std::string word;
	uint game_num;
	std::cin >> word;
	std::cin >> game_num;

	std::string grid_string;
	for (size_t i = 0; i < 9; ++i) {
		std::string line;
		std::cin >> line;
		grid_string += line;
	}

	std::vector<uint> grid;

	for (char c : grid_string) {
		grid.push_back(std::stoul(&c));
	}

	std::vector<uint> solved_grid = solve(grid);

	end_func();
}

int main() {

	for (uint i = 0; i < 3; ++i) {
		get_input_and_solve();
	}

	return 0;
}
