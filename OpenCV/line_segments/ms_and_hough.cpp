
#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>

#include <iostream>

int main(int argc, char** argv) {
	if (argc != 2) {
		return -1;
	}

	cv::Mat img = cv::imread(argv[1], 0);

	cv::imshow("original", img);
	cv::waitKey();

	cv::threshold(img, img, 127, 255, cv::THRESH_BINARY);
	cv::Mat skel(img.size(), CV_8UC1, cv::Scalar(0));
	cv::Mat temp;
	cv::Mat eroded;

	cv::Mat element = cv::getStructuringElement(cv::MORPH_CROSS, cv::Size(3, 3));

	while (true) {
		cv::erode(img, eroded, element);
		cv::dilate(eroded, temp, element); // temp = open(img)
		cv::subtract(img, temp, temp);
		cv::bitwise_or(skel, temp, skel);
		eroded.copyTo(img);

		if (cv::countNonZero(img) == 0) {
			break;
		}

	}

	cv::imshow("Skeleton", skel);
	cv::waitKey();

	cv::Mat dst, cdst;
	cv::Canny(skel, dst, 50, 200, 3);
	cv::cvtColor(dst, cdst, cv::COLOR_GRAY2BGR);

	// cv::imshow("Canny", dst);
	// cv::waitKey();

	std::vector<cv::Vec4i> lines;
	cv::HoughLinesP(skel, lines, 1, CV_PI/180, 5, 0, 10 );
	std::cout << "found " << lines.size() << " lines\n";
	for( size_t i = 0; i < lines.size(); i++ )
	{
		cv::Vec4i l = lines[i];
		auto colour = cv::Scalar((i%3==0)*255,(i%3==1)*255,(i%3==2)*255);
		cv::line( cdst, cv::Point(l[0], l[1]), cv::Point(l[2], l[3]), colour, 3, CV_AA);
	}

	cv::imshow("Lines", cdst);
	cv::waitKey();

}