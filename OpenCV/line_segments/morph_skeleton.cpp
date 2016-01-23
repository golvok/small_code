
#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>

// from http://felix.abecassis.me/2011/09/opencv-morphological-skeleton

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

}