build: eq_towns

eq_towns: main.o
	g++ -Wall -Weffc++ -Wextra -Werror -fkeep-inline-functions -pedantic -std=c++11 $< -o $@ -ggdb -D_GLIBCXX_DEBUG

%.o: %.c++
	g++ -c -Wall -Weffc++ -Wextra -Werror -fkeep-inline-functions -pedantic -std=c++11 $< -o $@ -ggdb -D_GLIBCXX_DEBUG
