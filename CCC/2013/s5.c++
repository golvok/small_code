#include <iostream>
#include <limits>

// #define USE_SMALL_FACTOR_OPT

using namespace std;

int findLowestScore(int, int, int*);
void recurse(int, int, int*, int*);

int main() {
	// ifstream infile("1.in");
	// if(!infile.is_open()){
	// 	cout << "problem with file"
	// 	return 1;
	// }
	cout << "enter number: ";
	int n;
	cin >> n;
	int bestScoreSofar = numeric_limits<int>::max();
	cout << "Lowest score: " << findLowestScore(n,0,&bestScoreSofar) << endl;
	return 0;
}

int findLowestScore(int n, int scoreToHere, int* bestScoreSofar){

	if (n == 1){
		return 0;
	}

	int lowestScoreTo1 = numeric_limits<int>::max();
	for(int i=2;i <= n;++i){
		if (n % i == 0){
			recurse(n/i,i-1, &lowestScoreTo1, bestScoreSofar);
			#ifdef USE_SMALL_FACTOR_OPT
			return lowestScoreTo1;
			#endif
		}
	}

	recurse(1,n-1,&lowestScoreTo1, bestScoreSofar);
	return lowestScoreTo1;
}

void recurse(int a, int b, int* lowestScoreTo1, int* bestScoreSofar){
	if (*lowestScoreTo1 > *bestScoreSofar) {
		cout << "returning immediately" << endl;
		return;
	}
	cout << "recursing on " << a << "," << b << endl;
	int scoreTo1 = b + findLowestScore(a*b, bestScoreSofar);
	cout << "got " << scoreTo1 << endl; 
	if (scoreTo1 < *lowestScoreTo1){
		*lowestScoreTo1 = scoreTo1;
	}
	if (scoreTo1 == b && scoreTo1 < *bestScoreSofar) { // ie. b hasn't changed; n (=a*b) == 1
		*bestScoreSofar = scoreTo1;
	}
}