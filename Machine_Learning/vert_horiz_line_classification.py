#!/usr/bin/env python

print(__doc__)

import os
import glob
import pdb
import math

import matplotlib.pyplot as plt
import numpy
from PIL import Image
from sklearn import svm, linear_model, metrics

from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis
from sklearn.neural_network import BernoulliRBM
from sklearn.pipeline import Pipeline

# for use with data/generate_lines.py
# cd to data & run it first
data_prefix = "data/lines/"
data_file_glob = "line_img*-[hv].png"

files = glob.glob(data_prefix + data_file_glob)

line_images = numpy.empty([len(files), 10, 10])
line_labels = numpy.empty(len(files), dtype=int)
for ifile, file in enumerate(files):
	line_images[ifile] = numpy.array(Image.open(file))
	line_labels[ifile] = 0 if (file[-5] == "h") else 1

# To apply a classifier on this data, we need to flatten the image, to
# turn the data in a (samples, feature) matrix:
n_samples = len(line_images)
data = line_images.reshape((n_samples, -1))
is_rmb = False

# Create a classifier

# classifier = svm.SVC()
# classifier = neural_network.MLPClassifier()
# classifier = RandomForestClassifier()

logistic = linear_model.LogisticRegression()
rbm = BernoulliRBM(random_state=0, verbose=True)
classifier = Pipeline(steps=[('rbm', rbm), ('logistic', logistic)])
rbm.learning_rate = 0.00000001
rbm.n_iter = 30
rbm.n_components = 50
logistic.C = 6000.0 # regularization - smaller means more
is_rmb = True

# We learn the lines on the first half of the lines
classifier.fit(data[:n_samples / 2], line_labels[:n_samples / 2])

# Now predict the value of the digit on the second half:
expected = line_labels[n_samples / 2:]
predicted = classifier.predict(data[n_samples / 2:])

print("Classification report for classifier %s:\n%s\n"
      % (classifier, metrics.classification_report(expected, predicted)))
print("Confusion matrix:\n%s" % metrics.confusion_matrix(expected, predicted))

plt.figure()
images_and_labels = list(zip(line_images, line_labels))
for index, (image, label) in enumerate(images_and_labels[:4]):
    plt.subplot(2, 4, index + 1)
    plt.axis('off')
    plt.imshow(image, cmap=plt.cm.gray_r, interpolation='nearest')
    plt.title('Training: %i' % label)

images_and_predictions = list(zip(line_images[n_samples / 2:], predicted))
for index, (image, prediction) in enumerate(images_and_predictions[:4]):
    plt.subplot(2, 4, index + 5)
    plt.axis('off')
    plt.imshow(image, cmap=plt.cm.gray_r, interpolation='nearest')
    plt.title('Prediction: %i' % prediction)

plt.show()

if (is_rmb):
	plt.figure(figsize=(4.2, 4))
	sqrt_len = math.sqrt(len(rbm.components_))
	ratio_bias = 16/9 # screen ratio - horiz/vert
	numx = math.ceil(sqrt_len/ratio_bias)
	numy = math.ceil(sqrt_len*ratio_bias)
	for i, comp in enumerate(rbm.components_):
	    plt.subplot(numx, numy, i + 1)
	    plt.imshow(comp.reshape((10, 10)), cmap=plt.cm.gray_r, interpolation='nearest')
	    plt.xticks(())
	    plt.yticks(())
	plt.suptitle('Components Extracted by RBM', fontsize=16)

	plt.show()
	plt.subplots_adjust(0.08, 0.02, 0.92, 0.85, 0.08, 0.23)
