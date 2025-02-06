#include "solitaire-solver.h++"

int main(int argc, char** argv) {
	std::vector<std::string_view> args;
	for (int i = 0; i < argc; ++i) {
		args.push_back(argv[i]);
	}
	return golvok::solitaire::App::main(args);
}