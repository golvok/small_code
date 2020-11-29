#!/usr/bin/env python

import os
import glob
import pdb
import math

import numpy
from PIL import Image

import pybrain.structure as pbstruct
from pybrain.datasets            import ClassificationDataSet
from pybrain.utilities           import percentError
from pybrain.tools.shortcuts     import buildNetwork
from pybrain.supervised.trainers import BackpropTrainer
from pybrain.structure.modules   import SoftmaxLayer

import pylab
import scipy
from numpy.random import multivariate_normal

# for use with data/generate_lines.py
# cd to data & run it first
data_prefix = "data/lines/"
data_file_glob = "line_img*-[hv].png"
img_size = 10*10

files = glob.glob(data_prefix + data_file_glob)

line_images = numpy.empty([len(files), img_size])
line_labels = numpy.empty(len(files), dtype=int)
for ifile, file in enumerate(files):
	line_images[ifile] = numpy.array(Image.open(file)).flatten()
	line_labels[ifile] = 0 if (file[-5] == "h") else 1


# nn = pbstruct.FeedForwardNetwork()

# image_in_layer = pbstruct.LinerLayer(100)
# hlayer_1 = pbstruct.SigmoidLayer(400)
# out_layer = pbstruct.LinerLayer(2);

# nn.addInputModule(image_in_layer);
# nn.addModule(hlayer_1);
# nn.addOutputModule(out_layer); 

# nn.addConnection(pbstruct.FullConnection(image_in_layer, hlayer_1));
# nn.addConnection(pbstruct.FullConnection(hlayer_1, out_layer));

# nn.sortModules()

n_classes = 2;
my_class_labels=['vertical', 'horizontal']

# means = [(-1,0),(2,4),(3,1)]
# cov = [scipy.diag([1,1]), scipy.diag([0.5,1.2]), scipy.diag([1.5,0.7])]
alldata = ClassificationDataSet(img_size, 1, nb_classes=n_classes, class_labels=my_class_labels)
# for n in range(400):
# 	for klass in range(3):
# 		mvn_input = multivariate_normal(means[klass],cov[klass])
# 		alldata.addSample(mvn_input, [klass])

for image, label in zip(line_images, line_labels):
	alldata.addSample(image, label)

tstdata_temp, trndata_temp = alldata.splitWithProportion(0.25)

tstdata = ClassificationDataSet(img_size, 1, nb_classes=n_classes, class_labels=my_class_labels)
for n in range(0, tstdata_temp.getLength()):
	tstdata.addSample( tstdata_temp.getSample(n)[0], tstdata_temp.getSample(n)[1] )

trndata = ClassificationDataSet(img_size, 1, nb_classes=n_classes, class_labels=my_class_labels)
for n in range(0, trndata_temp.getLength()):
	trndata.addSample( trndata_temp.getSample(n)[0], trndata_temp.getSample(n)[1] )

print ("Number of training patterns: ", len(trndata))
print ("Input and output dimensions: ", trndata.indim, trndata.outdim)
print ("First sample (input, target, class):")
print (trndata['input'][0], trndata['target'][0], trndata['class'] )

fnn = buildNetwork( trndata.indim, 20, 20, trndata.outdim, outclass=SoftmaxLayer )

trainer = BackpropTrainer( fnn, dataset=trndata, momentum=0.1, verbose=True, weightdecay=0.01)

# ticks = scipy.arange(-3.,6.,0.2)
# X, Y = scipy.meshgrid(ticks, ticks)
# # need column vectors in dataset, not arrays
# griddata = ClassificationDataSet(2,1, nb_classes=n_classes, class_labels=my_class_labels)
# for i in range(X.size):
# 	griddata.addSample([X.ravel()[i],Y.ravel()[i]], [0])
# griddata._convertToOneOfMany()  # this is still needed to make the fnn feel comfy


for i in range(20):
	trainer.trainEpochs( 1 )

	trnresult = percentError( trainer.testOnClassData(), trndata['target'] )
	tstresult = percentError( trainer.testOnClassData( dataset=tstdata ), tstdata['target'] )

	print ("epoch: %4d" % trainer.totalepochs,
		"  train error: %5.2f%%" % trnresult,
		"  test error: %5.2f%%" % tstresult
	)

	# out = fnn.activateOnDataset(griddata)
	# out = out.argmax(axis=1)  # the highest output activation gives the class
	# out = out.reshape(X.shape) 
	
pylab.ioff()
pylab.show()
