#!/usr/bin/env python

import random
import os
from PIL import Image, ImageDraw, ImageFilter

## config ##
# meta
num_images = 1000
output_dir = "lines/"
output_prefix = output_dir

# dimensions
end_width = 10
working_padding = 5; # on all sides

# line related
acceptable_deviation_factor = 1/4
line_fill_colour = 255

# noise related
min_noise_points = 2
max_noise_points = 7
random_point_fill_colour = 191

## derived - do not modify ##
working_width = end_width + 2*working_padding
working_height = working_width # square image
acceptable_deviation_from_straight = end_width*acceptable_deviation_factor

os.mkdir(output_dir);

for iimage in range(num_images):
	# create 8-bit greyscale image, with the padding
	im = Image.new('L', (working_width, working_height))
	draw = ImageDraw.Draw(im)

	horiz_not_vert = random.choice([True, False])

	# decide where the line will start
	start_position = random.randint(0, end_width-1)
	start_perturbation = start_position - end_width/2
	start_dist_to_right = (end_width-1) - start_position
	start_dist_to_left = start_position

	# decide where the line will end, but make sure it's not too far left or right
	end_perturbation = start_perturbation + random.randint(
		int( -min(acceptable_deviation_from_straight, start_dist_to_left ) ),
		int(  min(acceptable_deviation_from_straight, start_dist_to_right) )
	)

	start = (working_width/2 + start_perturbation, working_padding);
	end = (working_width/2 + end_perturbation, working_width - working_padding);

	# swap x & y if is horizontal
	if (horiz_not_vert):
		start = (start[1], start[0])
		end = (end[1], end[0])

	draw.line([start, end], fill=line_fill_colour, width=1);

	# now add some random noise
	num_noise_points = random.randint(min_noise_points, max_noise_points)
	for ipoint in range(num_noise_points):
		draw.point((working_padding + random.randint(0,end_width), working_padding + random.randint(0,end_width)), fill=random_point_fill_colour)

	# smooth the image
	im = im.filter(ImageFilter.SMOOTH)
	# cut out the part we care about
	im = im.crop((working_padding, working_padding, working_width - working_padding, working_height - working_padding ))

	label_string = 'h' if horiz_not_vert else 'v'
	im.save( output_prefix + "line_img%06d-%s.png" % (iimage, label_string), 'PNG')
