#!/usr/bin/python3

### pngtowav v0.9
### Convert a list of png images to pseudo composite video in wav file form

### This is Python code not intended for running on a microcontroller board

### MIT License

### Copyright (c) 2019 Kevin J. Walters

### Permission is hereby granted, free of charge, to any person obtaining a copy
### of this software and associated documentation files (the "Software"), to deal
### in the Software without restriction, including without limitation the rights
### to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
### copies of the Software, and to permit persons to whom the Software is
### furnished to do so, subject to the following conditions:

### The above copyright notice and this permission notice shall be included in all
### copies or substantial portions of the Software.

### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
### IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
### FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
### AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
### LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
### OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
### SOFTWARE.


### TODO - PAD IMAGE TO SCOPE RATIO which is 10 x 8 but 10 x 6 maps nicely to 0.5V per square
### TODO - quantize to 10bit or just leave it as is for high res DACs?
### TODO - work out how flyback time affects things - is this why there's an angle on image

### Out of focus looks good!

import getopt
import sys
import array
import struct

import imageio
import wave

### globals
### start_offset of 1 can help if triggering on oscilloscope
### is missing alternate lines
debug = 0
verbose = False
movie_file = False
output = "dacanim.wav"
fps = 50
threshold = 128
replaceforsync = False
start_offset = 1

max_dac_v = 3.3
### 16 bit wav files always use signed representation for data
offtopscreen = 32767      ### 3.30V
syncvalue    = -32768     ### 0.00V
### image from 3.00V to 0.30V
topvalue     = round(3.00 / max_dac_v * 65535) - 32768
bottomvalue  = round(0.30 / max_dac_v * 65535) - 32768


def usage(exit_code):
    print("pngtowav: [-f fps] [-h] [-m] [-o outputfilename] [-r] [-s lineoffset] [-v]",
          file=sys.stderr)
    if exit_code is not None:
        sys.exit(exit_code)

def main(cmdlineargs):
    global debug, fps, movie_file, output, replaceforsync
    global threshold, start_offset, verbose

    try:
        opts, args = getopt.getopt(cmdlineargs,
                                   "f:hmo:rs:v", ["help", "output="])
    except getopt.GetoptError as err:
        print(err,
              file=sys.stderr)
        usage(2)
    for opt, arg in opts:
        if opt == "-f":
            fps = int(arg)
        elif opt in ("-h", "--help"):
            usage()
            sys.exit()
        elif opt == "-m":
             movie_file = True
        elif opt in ("-o", "--output"):
            output = arg
        elif opt == "-r":
            replaceforsync = True
        elif opt == "-s":
            start_offset = int(arg)
        elif opt == "-v":
            verbose = True
        else:
            print("BAD OPTION",
                  file=sys.stderr)
            sys.exit(2)

    raw_output = array.array("h", [])

    ### Decide whether to replace first column with sync pulse
    ### or add it as an additional column
    if replaceforsync:
        firstpix = 1
    else:
        firstpix = 0

    ### Reach each frame, either
    ### many single image filenames in args or
    ### one or more video (animated gifs) (needs -m on command line)
    screenyrange = topvalue - bottomvalue
    row_offset = 0
    for arg in args:
        if verbose: print("PROCESSING", arg)
        if movie_file:
            images = imageio.mimread(arg)
        else:
            images = [imageio.imread(arg)]

        for img in images:
            img_height, img_width = img.shape
            if verbose: print("W,H", img_width, img_height)
            for row_o in range(img_height):
                row = (row_o + row_offset) % img_height
                ### Currently using 0 to n-1/n range
                ypos = round(topvalue - row / (img_height - 1) * screenyrange)
                if verbose: print("Adding row", row, "at ypos", ypos)
                raw_output.extend(array.array("h",
                                  [syncvalue]
                                  + [ypos if x >= threshold else offtopscreen
                                     for x in img[row, firstpix:]]))
            row_offset += start_offset

    ### Write wav file
    wav_filename = output
    wav_file = wave.open(wav_filename, "w")
    nchannels = 1
    sampwidth = 2
    framerate = (img_width + (1 - firstpix)) * img_height * fps
    nframes = len(raw_output)
    comptype = "NONE"
    compname = "not compressed"
    if verbose: print("Writing wav file", wav_filename, "at rate", framerate, "with", nframes, "samples")
    wav_file.setparams((nchannels, sampwidth, framerate, nframes,
                        comptype, compname))
    wav_file.writeframes(raw_output)
    wav_file.close()


if __name__ == "__main__":
    main(sys.argv[1:])
