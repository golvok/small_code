#include <vector>
#include <iostream>

using namespace std;

extern "C" {
    struct NumMatrix;
    struct NumMatrix* NumMatrixCreate(int** matrix, int mat_width, int mat_height);
    void NumMatrixFree(struct NumMatrix* numMatrix);
    int sumRegion(NumMatrix* nm, int rstart, int cstart, int rstop, int cstop);
}

int sumRegion_ref(NumMatrix& nm, int rstart, int cstart, int rstop, int cstop) {
    return sumRegion(&nm, rstart, cstart, rstop, cstop);
}

#define PRINT_NUMS

#ifdef PRINT_NUMS
    #define PRINT_WITH_NEWLINE(x) std::cout << x << '\n';
#else
    #define PRINT_WITH_NEWLINE(x) x
#endif

int main() {
    {
        int matrix[][2]{{-4,-5}};

        int* row_ptrs[] {
            &matrix[0][0]
        };

        NumMatrix* num_matrix = NumMatrixCreate(row_ptrs, 2, 1);

        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 0,0,0,0) ) ;
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 0,0,0,1) ) ;
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 0,1,0,1) ) ;

        NumMatrixFree(num_matrix);

        auto& dummy = PRINT_WITH_NEWLINE( "\n=========\n" );
        (void)dummy;
    }

    {
        int matrix[][4] {
            {-1,-2,-9,6},
            {8,-9,-3,-6},
            {2,9,-7,-6},
        };

        int* row_ptrs[] {
            &matrix[0][0],
            &matrix[1][0],
            &matrix[2][0],
        };

        NumMatrix* num_matrix = NumMatrixCreate(row_ptrs, 4, 3);

        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 2,1,2,1) ) ;
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 2,1,2,2) ) ;
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 2,2,2,2) ) ;
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 1,3,2,3) ) ;
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 1,3,2,3) ) ;

        NumMatrixFree(num_matrix);

        auto& dummy = PRINT_WITH_NEWLINE( "\n=========\n" );
        (void)dummy;
    }

    {
        int matrix[5][5]{
            {3,0,1,4,2},
            {5,6,3,2,1},
            {1,2,0,1,5},
            {4,1,0,1,7},
            {1,0,3,0,5}
        };

        int* row_ptrs[] {
            &matrix[0][0],
            &matrix[1][0],
            &matrix[2][0],
            &matrix[3][0],
            &matrix[4][0],
        };

        NumMatrix* num_matrix = NumMatrixCreate(row_ptrs, 5, 5);
        (void)num_matrix;

        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 2,1,4,3) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 1,1,2,2) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 1,2,2,4) );

        NumMatrixFree(num_matrix);

        auto& dummy = PRINT_WITH_NEWLINE( "\n=========\n" );
        (void)dummy;
    }

    {
        int matrix[19][19]{
            {-56570,17068,-26223,-53913,-26086,26309,65796,-56742,-74513,73420,-23726,89259,64856,91259,47122,-66481,-94327,-88320,56730},
            {33743,-36980,81122,-85032,-48099,-27038,-24047,20088,-49668,-12309,-98620,-42131,-17465,-77191,-12069,-55025,30930,-13511,-29547},
            {14158,5507,-41888,3365,-91278,96920,-56782,-32186,-65272,62981,41805,-7528,-73474,3629,-44215,-65540,-1981,27258,-74770},
            {-8702,-92572,54539,96395,72556,60669,-28710,-94980,83297,-42422,-89126,82469,51899,93744,28431,-20201,-63915,-59869,43312},
            {69158,82292,47760,-31933,90132,-74310,-42687,53188,35584,-69991,46298,-28390,70719,51836,82117,12317,38761,11842,23796},
            {17324,24452,-31241,758,97290,10060,69805,-79162,-37783,55410,45655,-4825,1017,-65994,-49031,24472,-63212,8956,-70537},
            {63348,45795,88171,75258,34508,13717,-33942,-52198,-53865,-63664,85433,26800,48092,66134,-9447,-79184,-27590,67266,29303},
            {-4011,36294,25114,-88447,60713,95735,-92318,-26388,-15340,-69908,36494,-12124,-80147,64061,-71894,96592,25162,-10161,-87430},
            {-15063,43304,95414,-25131,50128,75262,-81787,54557,82462,81686,74449,34335,14004,23291,2620,27136,51578,-85305,3892},
            {94870,-65332,72583,-55998,81397,53393,83481,99904,-52986,-280,-68669,45738,-42432,90551,59108,47884,-39340,17159,-86639},
            {-23135,-70930,81597,-532,9335,20685,52596,-53254,32053,-53354,52590,43345,-66665,28621,-80104,-20845,-7418,-50772,-70672},
            {-69404,40301,94014,71491,-73991,-56132,76055,83407,48114,-82721,-66694,56034,-21963,2241,37354,-35603,-47612,-85248,64119},
            {40124,80078,29463,-11428,52013,83736,92144,-1333,-15051,-59257,48069,-61085,82672,72223,79715,-3223,-24987,12584,83967},
            {62705,-42135,47165,-43813,59845,-6109,-38539,37186,4212,-33928,-14255,-55359,39308,-93299,12445,-3380,-66974,56257,24023},
            {-45527,-65573,-52842,-15870,67178,16295,-72572,67334,96378,41115,49732,-17154,-33564,-69573,8699,81566,-83018,-2439,39295},
            {-32683,-34571,-66655,-45955,71197,-50480,-89028,-86487,-52331,87712,35861,18730,-77129,97055,-85782,98585,57664,-68950,-53293},
            {-69515,38298,54376,30130,10053,-30059,53743,96353,72060,-28189,8591,13219,29842,-50825,27859,-1358,-83799,-55531,82467},
            {-2071,-30303,-96822,18712,3846,40446,-89727,-8760,59484,51598,-85594,32030,-91514,-15794,30915,99002,-60131,-91207,92924}
        };

        int* row_ptrs[] {
            &matrix[0][0],
            &matrix[1][0],
            &matrix[2][0],
            &matrix[3][0],
            &matrix[4][0],
            &matrix[5][0],
            &matrix[6][0],
            &matrix[7][0],
            &matrix[8][0],
            &matrix[9][0],
            &matrix[10][0],
            &matrix[11][0],
            &matrix[12][0],
            &matrix[13][0],
            &matrix[14][0],
            &matrix[15][0],
            &matrix[16][0],
            &matrix[17][0],
            &matrix[18][0],
        };

        NumMatrix* num_matrix = NumMatrixCreate(row_ptrs, 18, 19);

        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 14,15,14,16) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 4,10,4,17) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 16,10,16,17) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 11,18,15,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 9,7,17,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 13,1,14,2) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 9,15,9,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 7,6,11,16) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 16,16,17,16) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 15,2,17,11) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 14,17,16,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 10,9,14,14) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 7,4,15,15) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 6,12,12,13) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 1,8,12,14) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 1,16,6,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 3,5,11,10) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 12,13,17,16) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 11,18,17,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 7,11,9,14) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 6,16,12,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 5,3,12,9) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 8,12,10,16) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 0,18,1,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 17,10,17,10) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 14,12,14,18) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 12,12,15,12) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 9,7,9,16) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 11,15,14,17) );
        PRINT_WITH_NEWLINE( sumRegion_ref(*num_matrix, 12,0,17,12) );

        NumMatrixFree(num_matrix);

        auto& dummy = PRINT_WITH_NEWLINE( "\n=========\n" );
        (void)dummy;
    }
}
