// #include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>

#include <iostream>

using namespace cv;
using namespace std;

static void help()
{
	cout << "\nThis program demonstrates line finding with the Hough transform.\n"
			"Usage:\n"
			"./houghlines <image_name>, Default is ../data/pic1.png\n" << endl;
}

int main(int argc, char** argv)
{
	cv::CommandLineParser parser(argc, argv,
		"{ h | help  | false            | help       }"
		"{ i | image | ../data/pic1.png | image name }"
	);
	if (parser.get<bool>("help"))
	{
		help();
		return 0;
	}
	string filename = parser.get<string>("image");
	if (filename.empty())
	{
		help();
		cout << "no image_name provided" << endl;
		return -1;
	}
	Mat src = imread(filename, 0);
	if(src.empty())
	{
		help();
		cout << "can not open " << filename << endl;
		return -1;
	}

	Mat dst, cdst;
	Canny(src, dst, 50, 200, 3);
	cvtColor(dst, cdst, COLOR_GRAY2BGR);

#if 0
	vector<Vec2f> lines;
	HoughLines(dst, lines, 1, CV_PI/180, 75, 0, 0 );
	std::cout << "found " << lines.size() << " lines\n";
	for( size_t i = 0; i < lines.size(); i++ )
	{
		float rho = lines[i][0], theta = lines[i][1];
		Point pt1, pt2;
		double a = cos(theta), b = sin(theta);
		double x0 = a*rho, y0 = b*rho;
		pt1.x = cvRound(x0 + 1000*(-b));
		pt1.y = cvRound(y0 + 1000*(a));
		pt2.x = cvRound(x0 - 1000*(-b));
		pt2.y = cvRound(y0 - 1000*(a));
		line( cdst, pt1, pt2, Scalar(0,0,255), 3, CV_AA);
	}
#else
	vector<Vec4i> lines;
	HoughLinesP(dst, lines, 1, CV_PI/180, 30, 20, 10 );
	for( size_t i = 0; i < lines.size(); i++ )
	{
		Vec4i l = lines[i];
		line( cdst, Point(l[0], l[1]), Point(l[2], l[3]), Scalar(0,0,255), 3, CV_AA);
	}
#endif
	imshow("source", src);
	imshow("detected lines", cdst);

	waitKey();
	waitKey();

	return 0;
}
