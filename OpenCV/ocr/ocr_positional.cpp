// #include <opencv2/text/ocr.hpp>
#include <tesseract/baseapi.h>
#include <leptonica/allheaders.h>

#include <iostream>
#include <memory>

int main(int argc, char** argv) {
	(void) argv;
	if (argc != 2) {
		return -1;
	}

	tesseract::TessBaseAPI tess;

	if (tess.Init(nullptr, "eng")) {
		throw std::runtime_error("could not init tesseract");
	}

	auto pix_up = std::unique_ptr<Pix>(pixRead(argv[1]));
	tess.SetVariable("save_blob_choices", "T");
	// tess.SetVariable("tessedit_char_whitelist", "1234567890RVA"); //makes it worse somehow...
	tess.SetImage(pix_up.get());
	tess.Recognize(NULL);

	tesseract::ResultIterator* ri = tess.GetIterator();
	// tesseract::PageIteratorLevel level = tesseract::RIL_WORD;
	tesseract::PageIteratorLevel level = tesseract::RIL_SYMBOL;
	if (ri) {
	  do {
	    const char* word = ri->GetUTF8Text(level);
	    float conf = ri->Confidence(level);
	    int x1, y1, x2, y2;
	    ri->BoundingBox(level, &x1, &y1, &x2, &y2);
	    printf("word: '%s';  \tconf: %.2f; BoundingBox: %d,%d,%d,%d;\n",
	           word, conf, x1, y1, x2, y2);
	    delete[] word;
	  } while (ri->Next(level));
	}

	// cv::Mat img = cv::imread(argv[1], 0);
	// cv::imshow("original", img);
	// cv::waitKey();

	// cv::threshold(img, img, 127, 255, cv::THRESH_BINARY);

	// cv::imshow("threshold", img);
	// cv::waitKey();

	// auto tess = cv::text::OCRTesseract::create(nullptr, nullptr, "0123456789RVA");

	// std::string output_text;
	// std::vector<cv::Rect> component_rects;
	// std::vector<std::string> compenent_texts;
	// std::vector<float> component_confidences;
	// tess->run(img, output_text, component_rects, compenent_texts, component_confidences, OCR_LEVEL_WORD);

	// for (size_t i = 0; i < component_rects.size(); ++i) {
	// 	const auto& rect = component_rects.at(i);
	// 	const auto& text = compenent_texts.at(i);
	// 	std::cout << "found " << text << " at " << rect << '\n';
	// }
}
