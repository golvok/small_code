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
	REMOVE_GUESS,
	SEARCH_FOR_ONLY_ONE
};

class SetOfNine;

union SudokuOperationData {
	struct {
		Point point;
		uint number;
	} PointAndNumber;

	struct {
		SetOfNine* so9;
		uint search_for;
	} so9AndValue;

	SudokuOperationData(Point point, uint number)
		: PointAndNumber{point,number}
	{}

	SudokuOperationData(SetOfNine* so9, uint search_for)
		: so9AndValue{so9, search_for}
	{}
};

class SudokuOpDeque {
	struct OpHash;
	struct OpEquals;
public:
	class Operation {
		SudokuOperations op;
		SudokuOperationData data;
	public:
		Operation(const SudokuOperations& op, const SudokuOperationData& data) : op(op), data(data) {}
		const SudokuOperations& getOp() const { return op; }
		const SudokuOperationData& getData() const { return data; }
		friend struct SudokuOpDeque::OpHash;
		friend struct SudokuOpDeque::OpEquals;
	};
private:
	struct OpHash { size_t operator()(const SudokuOpDeque::Operation& op) const {
		switch (op.op) {
			case SudokuOperations::SET_TO:
			case SudokuOperations::REMOVE_GUESS:
				return
					  std::hash<Point>()(op.data.PointAndNumber.point)
					^ std::hash<uint>()(op.data.PointAndNumber.number);
			case SudokuOperations::SEARCH_FOR_ONLY_ONE:
				return
					  std::hash<SetOfNine*>()(op.data.so9AndValue.so9)
					^ std::hash<uint>()(op.data.so9AndValue.search_for);
		}
		assert(0 && "operation type not set: hashing failed");
		return 0;
	}};

	struct OpEquals { size_t operator()(const SudokuOpDeque::Operation& lhs, const SudokuOpDeque::Operation& rhs) const {
		if (lhs.op != rhs.op) { return false; }
		switch (lhs.op) {
			case SudokuOperations::SET_TO:
			case SudokuOperations::REMOVE_GUESS:
				return
					   (lhs.data.PointAndNumber.point  == rhs.data.PointAndNumber.point )
					&& (lhs.data.PointAndNumber.number == rhs.data.PointAndNumber.number);
			case SudokuOperations::SEARCH_FOR_ONLY_ONE:
				return
					   (lhs.data.so9AndValue.so9        == rhs.data.so9AndValue.so9 )
					&& (lhs.data.so9AndValue.search_for == rhs.data.so9AndValue.search_for);
		}
		assert(0 && "operation type not set: equivalence test failed");
		return false;
	}};

	std::deque<Operation> op_deque;
	std::unordered_set<Operation, OpHash, OpEquals> previously_done;
public:
	SudokuOpDeque()
		: op_deque()
		, previously_done()
	{}

	void emplace_back(SudokuOperations&& op, SudokuOperationData&& data) {
		Operation new_op(op, data);
		const auto& find_results = previously_done.find(new_op);
		if (find_results == previously_done.end()) {
			previously_done.insert(new_op);
			op_deque.push_back(new_op);
		}
	}

	void emplace_front(SudokuOperations&& op, SudokuOperationData&& data) {
		Operation new_op(op, data);
		const auto& find_results = previously_done.find(new_op);
		if (find_results == previously_done.end()) {
			previously_done.insert(new_op);
			op_deque.push_front(new_op);
		}
	}

	bool empty() { return op_deque.empty(); }
	void pop_front() { op_deque.pop_front(); }

	const Operation& front() { return op_deque.front(); }
};

class Sudoku;

class SetOfNine {
public:
	enum class Type {
		ROW,
		COL,
		BOX,
	};

	static const char* getTypeAsStr(Type t) {
		switch (t) {
			case Type::ROW:	return "row";
			case Type::COL:	return "column";
			case Type::BOX:	return "box";
			default: return "";
		}
	}

	static const uint DIMENSION_T = 9;
	static const uint DIMENSION; // set to DIMENSION_T below...

private:
	std::bitset<DIMENSION_T> numbers_in_set;
	std::array<uint,DIMENSION_T> candidate_counts; // so you can tell if there is only one place for a number.
	Type type;
	uint index;

	uint getCandidateCount(uint num) const {
		return candidate_counts[num-1];
	}

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

	bool decrementCandidateCountOf(uint num, SudokuOpDeque& op_deque) {
		uint& count = candidate_counts[num-1];
		if (count == 0) {
			return SUCCESS;
		} else if (count == 1) {
			// this is bad.
			return FALIURE;
		} else {
			count -= 1;
			if (count == 1) {
				// std::cout << "only one " << num << " in " << getTypeAsStr(getType()) << ' ' << getIndex() << "!\n";
				op_deque.emplace_back(
					SudokuOperations::SEARCH_FOR_ONLY_ONE,
					SudokuOperationData(this, num)
				);
			}
			return SUCCESS;
		}
	}

	bool operator==(const SetOfNine& rhs) const {
		return (getType() == rhs.getType()) && (getIndex() == rhs.getIndex());
	}

	bool operator!=(const SetOfNine& rhs) const { return !(*this == rhs); }

	Type getType() const { return type; }
	uint getIndex() const { return index; }

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

std::ostream& operator<<(std::ostream& os, SetOfNine::Type t) {
	os << SetOfNine::getTypeAsStr(t);
	return os;
}

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

	uint getNumPossibilities() const {
		uint count = 0;
		for (size_t i = 1; i <= 9; ++i) {
			if (test(i) == true) {
				count++;
			}
		}
		return count;
	}

	Possibilities(const SetOfNine& r, const SetOfNine& c, const SetOfNine& b, const std::bitset<9>& local_mask)
		: set(~r.getNumbersInSet() & ~c.getNumbersInSet() & ~b.getNumbersInSet() & local_mask)
	{ }

	const BitRef operator[](size_t num) const {
		return const_cast<Possibilities&>(*this)[num];
	}

	BitRef operator[](size_t num) {
		return set[num-1];
	}

	bool operator==(const Possibilities& rhs) const {
		return this->set == rhs.set;
	}

	bool operator!=(const Possibilities& rhs) const {
		return !(*this == rhs);
	}

	bool test(size_t num) const {
		return (*this)[num];
	}

	class iterator : std::iterator<std::forward_iterator_tag,uint> {
		const Possibilities& ref;
		uint index;
	public:
		iterator(const Possibilities& ref, uint start)
			: ref(ref)
			, index(start)
		{}

		iterator& operator++() {
			index++;
			while (index <= 9 && ref.test(index) == false) {
				index++;
			}
			return *this;
		}

		uint operator*() {
			return index;
		}

		bool operator==(const iterator& rhs) const {
			return this->index == rhs.index;
		}

		bool operator!=(const iterator& rhs) const {
			return !(*this == rhs);
		}
	};

	iterator begin() const {
		iterator retval(*this,0);
		++retval;
		return retval;
	}

	iterator end() const {
		return iterator(*this,10);
	}

	void print(std::ostream& os = std::cout) const {
		for (uint num : *this) {
			os << num;
		}
	}

	friend struct std::hash<Possibilities>;
};

std::ostream& operator<<(std::ostream& os, const Possibilities& p) {
	p.print(os);
	return os;
}

namespace std {
	template<>
	struct hash<Possibilities> {
		size_t operator()(const Possibilities& p) const {
			return static_cast<size_t>(p.set.to_ullong());
		}
	};
}

class Sudoku {

	// TODO: use container with constexpr .size() for these.
	std::vector<SetOfNine> rows;
	std::vector<SetOfNine> cols;
	std::vector<SetOfNine> boxes;

	std::array<std::array<uint,9>,9> grid;

	std::array<std::array<std::bitset<9>,9>,9> local_masks;

	SetOfNine& getBox(Point square_in_box) { return boxes[square_in_box.first/3 + (square_in_box.second/3)*3]; }
	SetOfNine& getRow(Point square_in_row) { return rows[square_in_row.second]; }
	SetOfNine& getCol(Point square_in_col) { return cols[square_in_col.first]; }

public:
	const SetOfNine& getBox(Point square_in_box) const { return boxes[square_in_box.first/3 + (square_in_box.second/3)*3]; }
	const SetOfNine& getRow(Point square_in_row) const { return rows[square_in_row.second]; }
	const SetOfNine& getCol(Point square_in_col) const { return cols[square_in_col.first]; }

	const std::vector<SetOfNine>& getBoxes() const { return boxes; }
	const std::vector<SetOfNine>& getRows() const { return rows; }
	const std::vector<SetOfNine>& getCols() const { return cols; }

	uint getNumberAt(Point square) const { return arrayGet(grid,square); }
	bool hasNumberAt(Point square) const { return getNumberAt(square) != 0; }

	std::array<SetOfNine*,3> getSetsOfNine(Point squ) {
		return {
			&getRow(squ),
			&getCol(squ),
			&getBox(squ)
		};
	}

	bool decrementAllCandidateCountsOf(Point squ, uint num, SudokuOpDeque& op_deque) {
		for (SetOfNine* so9 : getSetsOfNine(squ)) {
			if (so9->decrementCandidateCountOf(num,op_deque) == FALIURE) {
				return FALIURE;
			}
		}

		return SUCCESS;
	}

	bool setNumber(Point square_to_set, uint num_to_set, SudokuOpDeque& op_deque) {
		assert(num_to_set <= 9 && "a number set must be 9 or less");
		// std::cout << "setting " << square_to_set << " to " << num_to_set << '\n';

		const Possibilities& p_of_sts_before_set = getPossibilities(square_to_set);
		arrayGet(grid,square_to_set) = num_to_set;
		arrayGet(local_masks,square_to_set).reset();

		// iterate over the row, col, and box to:
		//   remove candidate counts in other so9s, to:
		//     see if this has created something unique that so9

		std::unordered_set<Point> already_decremented { square_to_set };

		for (auto& so9 : getSetsOfNine(square_to_set)) {
			// remove guess count from so9s of the square we set
			for (uint guess : p_of_sts_before_set) {
				if (guess != num_to_set) {
					if(so9->decrementCandidateCountOf(guess, op_deque) == FALIURE) {
						return FALIURE;
					}
				}
			}
			for (auto& squ : *so9) {
				if (already_decremented.find(squ) != already_decremented.end()) { // skip squares we've done already
					continue;
				}

				if (getPossibilities(squ).test(num_to_set)) {
					for(auto& squs_so9 : getSetsOfNine(squ)) {
						if (squs_so9 != so9) {
							if (squs_so9->decrementCandidateCountOf(num_to_set, op_deque) == FALIURE) {
								return FALIURE;
							}
						}
					}
					already_decremented.insert(squ);
				}
			}
		}

		getBox(square_to_set).setHasNumber(num_to_set);
		getRow(square_to_set).setHasNumber(num_to_set);
		getCol(square_to_set).setHasNumber(num_to_set);

		// std::cout << "grid is now:\n";
		// this->print_with_guesses(std::cout);
		// this->print_to_stream(std::cout);

		return SUCCESS;
	}

	bool removeGuess(Point square_to_change, uint guess_to_remove, SudokuOpDeque& op_deque) {
		// std::cout << "removing " << guess_to_remove << " from " << square_to_change << "\n";
		if (getPossibilities(square_to_change).test(guess_to_remove)) {
			if (decrementAllCandidateCountsOf(square_to_change,guess_to_remove,op_deque) == FALIURE) {
				return FALIURE;
			}
			arrayGet(local_masks,square_to_change)[guess_to_remove-1] = 0;
			Possibilities p = getPossibilities(square_to_change);
			if (p.isOnlyOne() && hasNumberAt(square_to_change) == false) {
				op_deque.emplace_back(
					SudokuOperations::SET_TO,
					SudokuOperationData{square_to_change, p.getLowest()}
				);
			}
		}

		return SUCCESS;
	}

	const Possibilities getPossibilities(Point square) {
		return Possibilities(getRow(square),getCol(square),getBox(square),arrayGet(local_masks,square));
	}

	void print_to_stream(std::ostream& os, bool show_guess_counts = false) const {
		os << "╔═══════╦═══════╦═══════╗\n";
		for (size_t row_num = 0; row_num < DIMENSION; ++row_num) {
			if (row_num % 3 == 0 && row_num != 0) {
				os << "╠═══════╬═══════╬═══════╣\n";
			}
			for (size_t col_num = 0; col_num < DIMENSION; ++col_num) {
				if (col_num % 3 == 0) {os << "║ ";}
				uint this_num = getNumberAt(Point{col_num,row_num});
				if (this_num == 0) {
					os << "▫";
				} else {
					os << this_num;
				}
				os << " ";
			}
			os << "║";

			if (show_guess_counts) {
				os << "   ";
				rows[row_num].print(os, Orientation::HORIZONTAL, 1);
			}

			os << '\n';
		}

		os << "╚═══════╩═══════╩═══════╝";

		if (show_guess_counts) {
			os << '\n';
			for (size_t col_num = 0; col_num < DIMENSION; ++col_num) {
				os << '\n';
				for (size_t i = 0; i < col_num; ++i) {
					os << "  ";
					if (i % 3 == 2) {
						os << "  ";
					}
				}

				cols[col_num].print(os, Orientation::HORIZONTAL, 1);
			}
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
					Point current_square{col_num,row_num};

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

		os << "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝";
	}

	Sudoku()
		: rows()
		, cols()
		, boxes()
		, grid()
		, local_masks()
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
		for (auto& row : local_masks) {
			for (auto& mask : row) {
				mask.set();
			}
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

using SudokuState = std::pair<Sudoku,SudokuOpDeque>;
using SudokuStateList = std::vector<SudokuState>;

std::pair<SudokuStateList,bool> attempt(SudokuState& state);

std::vector<uint> solve(std::vector<uint>& grid) {

	std::stack<SudokuStateList> state_stack;

	{ // new scope
		SudokuOpDeque first_op_deque;

		for (size_t i = 0; i < 9; ++i) {
			for (size_t j = 0; j < 9; ++j) {
				uint grid_number = grid[i*9 + j];
				if (grid_number != 0) {
					first_op_deque.emplace_back(
						SudokuOperations::SET_TO,
						SudokuOperationData{Point{j,i},grid_number}
					);
				}
			}
		}

		state_stack.emplace(SudokuStateList{{Sudoku(), std::move(first_op_deque)}});
	}

	while (state_stack.empty() == false) {
		SudokuStateList state_list = state_stack.top();
		state_stack.pop();

		for (SudokuState& state : state_list) {
			// std::cout << ">>> stack level=" << state_stack.size() << ", going with grid:\n" << state.first << '\n';
			SudokuStateList new_states;
			bool success;
			std::tie(new_states,success) = attempt(state);

			if (new_states.empty()) {
				if (success) {
					// std::cout << "Solved?\n";
					std::cout << state.first << '\n';
					return {};
				} else {// else: try the next thing.
					// std::cout << "!!! things went illegal! reverting. !!!\n";
				}
			} else {
				state_stack.emplace(new_states);
			}
		}
	}

	std::cout << "failed?\n";
	return {};
}

std::pair<SudokuStateList,bool> attempt(SudokuState& state) {

	Sudoku& sudoku = state.first;
	SudokuOpDeque& op_deque = state.second;

	size_t iteration_number = 0;

	while (op_deque.empty() == false) {
		iteration_number++;

		if (iteration_number > 1000) {
			// std::cout << "GIVING UP!!!!\n";
			break;
		}

		// update guesses.
		while (op_deque.empty() == false) {
			auto& op_and_data = op_deque.front();
			const SudokuOperations& op = op_and_data.getOp();
			const SudokuOperationData& data = op_and_data.getData();
			switch (op) {
				case SudokuOperations::SET_TO:
					if (sudoku.setNumber(data.PointAndNumber.point, data.PointAndNumber.number, op_deque) == FALIURE) {
						return {{},false};
					}
				break;
				case SudokuOperations::REMOVE_GUESS:
					if (sudoku.removeGuess(data.PointAndNumber.point, data.PointAndNumber.number, op_deque) == FALIURE) {
						return {{},false};
					}
				break;
				case SudokuOperations::SEARCH_FOR_ONLY_ONE:
					for (const Point& squ : *data.so9AndValue.so9) {
						if (sudoku.getPossibilities(squ).test(data.so9AndValue.search_for)) {
							op_deque.emplace_back(
								SudokuOperations::SET_TO,
								SudokuOperationData(squ, data.so9AndValue.search_for)
							);
							break;
						}
					}
				break;
			}
			op_deque.pop_front();
		}

		// std::cout << "before iteration " << iteration_number << ", grid is:\n";
		// sudoku.print_with_guesses(std::cout);

		// do logic
		// eg. only places for number in one SO9 are also all in one other SO9
			// boxes in particular
		// hidden twin, naked twin, triplets...
		for (Point squ : sudoku) {
			const Possibilities& p = sudoku.getPossibilities(squ);
			if (p.isOnlyOne()) {
				uint the_one = p.getLowest();
				// std::cout << squ << " can only be " << the_one << "\n";
				op_deque.emplace_back(
					SudokuOperations::SET_TO,
					SudokuOperationData{squ,the_one}
				);
			}
		}

		std::array<const std::vector<SetOfNine>*, 3> so9_sets {
			&sudoku.getRows(),
			&sudoku.getCols(),
			&sudoku.getBoxes(),
		};

		for (auto& so9_set : so9_sets) {
			for (const SetOfNine& so9 : *so9_set) {
				std::unordered_multiset<Possibilities> previously_seen;
				for (Point squ : so9) {
					Possibilities p = sudoku.getPossibilities(squ);
					uint num_poss = p.getNumPossibilities();
					previously_seen.insert(p);

					// naked tuples - naked pair & triplet & etc.
					if (previously_seen.count(p) == num_poss && num_poss > 1) {
						// std::cout << "found a naked tuple " << p << " in " << so9.getType() << ' ' << so9.getIndex() << " consisting of ";
						for (Point squ2 : so9) {
							if (p != sudoku.getPossibilities(squ2)) {
								for (uint candidate_in_tuple : p) {
									op_deque.emplace_front(
										SudokuOperations::REMOVE_GUESS,
										SudokuOperationData{squ2,candidate_in_tuple}
									);
								}
							} else {
								// std::cout << squ2 << ", ";
							}
						}
						// std::cout << "\n";
					}
				}
			}
		}
	}


	Point has_fewest_possibilities = {-1,-1};
	uint fewest_possibilities = (uint)-1;
	for (const Point& squ : sudoku) {
		uint num_possibilities = sudoku.getPossibilities(squ).getNumPossibilities();
		if (num_possibilities > 0 && num_possibilities < fewest_possibilities) {
			has_fewest_possibilities = squ;
			fewest_possibilities = num_possibilities;
		}
	}

	if (has_fewest_possibilities == Point{-1,-1}) {
		// everything is set, and the puzzle is solved!
		return {{},true};
	}

	// if we got here, then logic has failed us, so lets push some guesses on on the stack!
	// std::cout << "logic has failed!\n";
	SudokuStateList retval;

	for (uint poss : sudoku.getPossibilities(has_fewest_possibilities)) {
		// std::cout << "trying " << has_fewest_possibilities << " as " << poss << "\n";
		SudokuState state_dup = state;
		state_dup.second.emplace_back(
			SudokuOperations::SET_TO,
			SudokuOperationData(has_fewest_possibilities, poss)
		);
		retval.emplace_back(state_dup);
	}

	return {retval,false};
}

std::vector<uint> get_input_and_solve() {
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
		grid.push_back(c - '0');
	}

	return grid;
}

int main() {

	for (uint i = 1; i <= 50; ++i) {
		std::vector<uint> grid = get_input_and_solve();
		// if (i <= 5) {
		// 	continue; // skip first so many
		// }
		std::vector<uint> solved_grid = solve(grid);
		end_func();
	}

	return 0;
}
