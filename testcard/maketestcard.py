#!/usr/bin/python3

### Produces png test cards with a transparent middle

### Testing with
### test=NNN ; for res in 120x120 160x120 240x240 320x240 1280x720 ; do python maketestcard.py ${res} test-${test}-${res}.png ; done

### TODO - think about some c0 desaturated values just under colour bars at top
###        maybe bottom third at 40,0,0  and ff,c0,c0 for red
### TODO - adjust so the 120x120 and 160x120 doesn't over write top/bottom colours bars by 1px
### TODO - work out how to add optional logo and composite with background image
### TODO - look at how subprocess works - can it do pipe in? pipe in and out at same time?


### The MIT License (MIT)
###
### Copyright (c) 2020 Kevin J. Walters
###
### Permission is hereby granted, free of charge, to any person obtaining a copy
### of this software and associated documentation files (the "Software"), to deal
### in the Software without restriction, including without limitation the rights
### to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
### copies of the Software, and to permit persons to whom the Software is
### furnished to do so, subject to the following conditions:
###
### The above copyright notice and this permission notice shall be included in
### all copies or substantial portions of the Software.
###
### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
### IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
### FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
### AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
### LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
### OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
### THE SOFTWARE.


import getopt
import sys
import struct
import math

import numpy as np
import imageio


TESTCARD="A"
VERSION="1"

### globals
debug = 1
composite_filename = None
verbose = False
output = "testcard.png"

TRANSPARENT = 0
OPAQUE = 255


BLACK = (0, 0, 0, OPAQUE)
WHITE = (255, 255, 255, OPAQUE)


BLUE = (0, 0, 255, OPAQUE)
DIMBLUE = (0, 0, 128, OPAQUE)
PALEBLUE = (128, 128, 192, OPAQUE)

RED = (255, 0, 0, OPAQUE)
DIMRED = (128, 0, 0, OPAQUE)
PALERED = (192, 128, 128, OPAQUE)

MAGNETA = (255, 0, 255, OPAQUE)
DIMMAGENTA = (128, 0, 128, OPAQUE)
PALEMAGENTA = (192, 128, 192, OPAQUE)

GREEN = (0, 255, 0, OPAQUE)
DIMGREEN = (0, 128, 0, OPAQUE)
PALEGREEN = (128, 192, 128, OPAQUE)

CYAN = (0, 255, 255, OPAQUE)
DIMCYAN = (0, 128, 128, OPAQUE)
PALECYAN = (128, 192, 192, OPAQUE)

YELLOW = (255, 255, 0, OPAQUE)
DIMYELLOW = (128, 128, 0, OPAQUE)
PALEYELLOW = (192, 192, 128, OPAQUE)

GREY = (128, 128, 128, OPAQUE)
GREY247 = (247, 247, 247, OPAQUE)


def d_print(level, *args, **kwargs):
    """A simple conditional print for debugging based on global debug level."""
    if not isinstance(level, int):
        print(level, *args, **kwargs)
    elif debug >= level:
        print(*args, **kwargs)


def usage(exit_code):
    print("maketestcard: [-v] widthxheight filename",
          file=sys.stderr)
    if exit_code is not None:
        sys.exit(exit_code)


def background_grey_grid(im, thickness=None, spacing=None):
    wid, hei, _ = im.shape

    if spacing is None:
       pixhun = max(wid, hei) / 100.0
       if pixhun > 4.0:
           spacing = 32
       elif pixhun > 2.0:
           spacing = 16
       else:
           spacing = 8

    if thickness is None:
        thickness = spacing // 8

    ### grey fill
    for x in range(wid):
        for y in range(hei):
            im[x, y] = GREY

    ### horizontals
    y_pos = []
    for start in range(thickness):
        y_pos.extend(list(range(start, hei, spacing)))

    for y in y_pos:
        for x in range(wid):
            im[x, y] = WHITE

    ### verticals
    x_pos = []
    for start in range(thickness):
        x_pos.extend(list(range(start, wid, spacing)))

    for x in range(0, wid, spacing):
        for y in range(hei):
            im[x, y] = WHITE


def fine_border_lines(im, size=8):
    wid, hei, _ = im.shape

    ### black first to allow white to over-write
    ### horizontals
    for x in range(wid):
        for offset in range(1, size, 2):
            im[x, offset] = BLACK
            im[x, hei - 1 - offset] = BLACK

    ### verticals
    for y in range(hei):
        for offset in range(1, size, 2):
            im[offset, y] = BLACK
            im[wid - 1 - offset, y] = BLACK

    ### white lines get reduced in length to combine to form boxes
    ### horizontals
    for offset in range(0, size, 2):
        for x in range(offset, wid - offset):
            im[x, offset] = WHITE
            im[x, hei - 1 - offset] = WHITE

    ### verticals
    for offset in range(0, size, 2):
        for y in range(offset, hei - offset):
            im[offset, y] = WHITE
            im[wid - 1 - offset, y] = WHITE


def mark_quarterpoints(im, size=8):
    wid, hei, _ = im.shape

    ### mark top and bottom
    halfwidth = 1
    for y_off in range(size):
        for h_quarter_r in (wid // 4, wid // 2, wid * 3 // 4):
            for x in range(h_quarter_r - halfwidth, h_quarter_r + halfwidth):
                im[x, y_off] = WHITE
                im[x, hei - y_off - 1] = WHITE
        halfwidth += 1

    ### mark left and right
    halfwidth = 1
    for x_off in range(size):
        for v_quarter_r in (hei // 4, hei // 2, hei * 3 // 4):
            for y in range(v_quarter_r - halfwidth, v_quarter_r + halfwidth):
                im[x_off, y] = WHITE
                im[wid - x_off -1, y] = WHITE
        halfwidth += 1


### bar_width here is the width of each colour bar in the larger "bar"
### this is inconsistent with much of the rest of the code
def colour_bars(im, gap=8, top=8, bottom=None,
                    bar_width=None, bar_height=None,
                    centre=True):
    """Colour bars intended for the top of the screen.
       These all have the same pixel width."""
    wid, hei, _ = im.shape
    bars = (YELLOW, CYAN, GREEN, MAGNETA, RED, BLUE)
    subbars = ((DIMYELLOW, PALEYELLOW),
               (DIMCYAN, PALECYAN),
               (DIMGREEN, PALEGREEN),
               (DIMMAGENTA, PALEMAGENTA),
               (DIMRED, PALERED),
               (DIMBLUE, PALEBLUE))

    if bar_width is None:
        bar_width = int((wid - 2 * gap) / len(bars))
        if centre:
            gap = int((wid - bar_width * len(bars)) // 2)

    if bar_height is None:
        bar_height = hei // 8

    if gap is None:
        gap = (wid - bar_width * len(bars)) // 2

    half_bar_width = bar_width // 2
    top_section = range(top, top + bar_height * 2 // 3)
    bottom_section = range(top + bar_height * 2 // 3, top + bar_height)
    for bar_idx, bar_col in enumerate(bars):
        left_x = gap + bar_idx * bar_width
        for x in range(left_x, left_x + bar_width):
            subsubbar_idx = 1 if x - left_x >= half_bar_width else 0
            for y in top_section:
                im[x, y] = bar_col
            for y in bottom_section:
                im[x, y] = subbars[bar_idx][subsubbar_idx]

    return bar_width * len(bars)


def grey_vbars(im,
               top=None, bottom_ex=None,
               left=None, right=None,
               bar_width=None,
               bar_height=None):
    wid, hei, _ = im.shape

    ### 9 linear levels
    lin_levels = list(range(0, 8 * 32, 32))
    lin_levels.append(255)

    log_levels = [255]
    ### Add another 17 levels to make pairs for each linear level
    log_levels.extend([round(2**(p / 2.0 / 17.0 * 16.0 )) for p in range(16, -1, -1)])

    if bar_width is None:
        bar_width = wid // 8

    if right is not None:
        left = right - bar_width
    if left is None:
        ##left = round(wid * 13 / 16)
        left = wid - bar_width - 10

    if top is not None and bottom_ex is not None:
        bar_height = bottom_ex - top
    else:
        if bar_height is None:
            uneven_bar_height = round(hei * 10 / 16)
            ### make each linear cell in the bar the same size - this looks a lot
            ### better on small screens, e.g. 64 height
            bar_height = int(uneven_bar_height // len(lin_levels) * len(lin_levels))
        top = (hei - bar_height) // 2
        bottom_ex = top + bar_height

    lin_cell_height = bar_height / len(lin_levels)
    half_bar_width = bar_width // 2
    middle_r = left + bar_width // 4
    for y in range(top, bottom_ex):
        idx_real = (y - top) / (bar_height - 1) * 0.9998 * len(lin_levels) - 0.49999
        idx = round(idx_real)
        for x in range(left, left + half_bar_width):
           im[x, y] = (lin_levels[idx], lin_levels[idx], lin_levels[idx], OPAQUE)

        ### this is not a good way of looking for middle but it works
        ymid = round(top + (idx + 0.5) * lin_cell_height)
        if y == ymid or y == ymid - 1:
            for x in range(middle_r - 1, middle_r + 1):
               im[x, y] = (lin_levels[-1-idx], lin_levels[-1-idx], lin_levels[-1-idx], OPAQUE)

    for y in range(top, bottom_ex):
        idx_real = (y - top) / (bar_height - 1) * 0.9998 * len(log_levels) - 0.49999
        idx = round(idx_real)
        for x in range(left, left + half_bar_width):
           im[x + half_bar_width, y] = (log_levels[idx], log_levels[idx], log_levels[idx], OPAQUE)

    return bar_width, bar_height


def wheel(pos, alpha=OPAQUE):
    """Input a value 0 to 255 to get a color value.
       The colours are a transition r - g - b - back to r."""
    if (pos < 0) or (pos > 255):
        return (0, 0, 0)
    tripos = int(pos*3)
    if (pos < 85):
        return (255 - tripos, tripos, 0, OPAQUE)
    elif (pos < 170):
        tripos -= 255
        return (0, 255 - tripos, tripos, OPAQUE)
    else:
        tripos -= 510
        return (tripos, 0, 255 - tripos, OPAQUE)


def rgb_vwheel(im,
               top=None, bottom_ex=None,
               left=None, right=None,
               bar_width=None,
               bar_height=None):
    wid, hei, _ = im.shape

    if bar_width is None:
        bar_width = wid // 16

    if right is not None:
        left = right - bar_width
    if left is None:
        left = wid - bar_width - 10

    if top is not None and bottom_ex is not None:
        bar_height = bottom_ex - top
    else:
        if bar_height is None:
            top = round(hei * 3 / 16)
            bottom_ex = round(hei * 13 / 16)
            bar_height = bottom_ex - top
        else:
            top = (hei - bar_height) // 2
            bottom_ex = top + bar_height

    for y in range(top, bottom_ex):
        ### Convert to 0.0 to 255.0
        wheel_pos = (y - top) / (bar_height - 1) * 255.0

        colour = wheel(wheel_pos)
        for x in range(left, left + bar_width):
           im[x, y] = colour


def bw_vstripes(im,
                top=None, bottom_ex=None,
                left=None, bar_width=None,
                bar_height=None):
    """Blocks of black and white lines and dots.
       Vertical lines, horizontal lines and a chequered pattern."""
    wid, hei, _ = im.shape

    if bar_width is None:
        bar_width = wid // 8

    if left is None:
        ##left = round(wid * 1 / 16)
        left = 10

    if top is not None and bottom_ex is not None:
        bar_height = bottom_ex - top
    else:
        top = round(hei * 3 / 16)
        bottom_ex = round(hei * 13 / 16)
        bar_height = bottom_ex - top

    half_bar_width = bar_width // 2

    line_spc = (1, 2, 3, 4, 5)
    last_idx = None
    for y in range(top, bottom_ex):
        idx_real = (y - top) / (bar_height - 1) * 0.9998 * len(line_spc) - 0.49999
        idx = round(idx_real)
        if idx != last_idx:
            cell_top = y
            last_idx = idx
        d_print(3, "INDEX", idx_real, idx, "at y", y, top, bottom_ex)
        selectPeakV = (y - cell_top) // line_spc[idx] % 2 == 0
        for x in range(left, left + half_bar_width):
           im[x, y] = WHITE if selectPeakV else BLACK

        for x in range(left, left + half_bar_width):
           selectPeakH = (x - left) // line_spc[idx] % 2 == 0
           im[x + half_bar_width, y] = WHITE if selectPeakH else BLACK
           ### draw a grey dot pattern too
           ### TODO review positioning/etc
           im[x + half_bar_width * 2, y] = WHITE if selectPeakH == selectPeakV else BLACK


def colour_linear_graduated(im, col,
                            top=None, bottom_ex=None,
                            bar_width=None,
                            gap=8):
    wid, hei, _ = im.shape

    if bottom_ex is None:
        bottom_ex = hei - gap

    if top is None:
        top = bottom_ex - hei // 16

    if bar_width is None:
        bar_width = wid - gap * 2

    grad_col = [OPAQUE] * 4
    for x in range(gap, gap + bar_width):
        grad = (x - gap) / (bar_width - 1)
        grad_col[0] = round(col[0] * grad)
        grad_col[1] = round(col[1] * grad)
        grad_col[2] = round(col[2] * grad)
        d_print(3, "GRAD", grad_col)
        for y in range(top, bottom_ex):
            im[x, y] = grad_col  ### this copies the data


def rgb_colour_linear_graduated(im, gap=None, bottom_gap=8, bottom=None,
                                bar_width=None, bar_height=None,
                                centre=True):
    wid, hei, _ = im.shape
    bars = (RED, GREEN, BLUE)

    if bar_height is None:
        bar_height = hei // 24

    if bar_width is None and gap is not None:
        bar_width = wid - gap * 2
    elif gap is None:
        gap = (wid - bar_width) // 2

    start_y = hei - bottom_gap - len(bars) * bar_height
    for bar_idx, bar_col in enumerate(bars):
        top_y = start_y + bar_idx * bar_height
        colour_linear_graduated(im, bar_col,
                                top=top_y,
                                bottom_ex=top_y + bar_height,
                                bar_width=bar_width,
                                gap=gap)


def image_cutout(im, radius=None):
    """A circular cut-out achieved with modifying transparency."""
    wid, hei, _ = im.shape

    if radius is None:
       radius = min(wid, hei) // 4 + 3.5

    white_border = radius / 30.0

    filled_circle(im, WHITE,
                  radius=radius)
    filled_circle(im, (None, None, None, TRANSPARENT),
                  radius=radius - white_border)


def filled_circle(im, colour, radius=None):
    wid, hei, _ = im.shape

    if radius is None:
       intradius = min(wid, hei) // 4
       radius = intradius - 0.5
    else:
       intradius = math.ceil(radius)

    ### Images will normally have even number of rows so centre
    ### is really the middle two rows, this is the lower of those two
    intcentre_x = hei // 2
    intcentre_y = hei // 2
    centre_x = (wid - 1) / 2.0
    centre_y = (hei - 1) / 2.0
    top = intcentre_y - intradius
    bottom_ex = intcentre_y + intradius

    ### Cutout a transparent circle by drawing horizontal slices of that
    ### circle - no anti-aliasing
    radius_sqrd = radius * radius
    for row in range(top, bottom_ex):
        y_off = centre_y - row
        slice_half_len_sqrd = radius_sqrd - y_off * y_off
        if slice_half_len_sqrd > 0.0:
            slice_half_len = math.sqrt(slice_half_len_sqrd)
            begin_x = centre_x - slice_half_len
            end_x = centre_x + slice_half_len
            d_print(2, "Drawing slice at", row, "half width", slice_half_len)
            if end_x - begin_x > 0.5:
                for x in range(round(begin_x), round(end_x) + 1):
                    for ch_idx, ch_value in enumerate(colour):
                        if ch_value is not None:
                            im[x, row][ch_idx] = ch_value


def binary_write(im, text, x=None, y=None,
                 colours = (DIMRED, DIMGREEN, DIMBLUE)):
    wid, hei, _ = im.shape

    if x is None:
       x = 3
    if y is None:
       y = hei - 2
    
    ### Make a binary representation of all the digits as a text string
    binary_text = "".join(["{0:{fill}8b}".format(ord(b), fill='0') for b in text])
    
    xpos = x
    for bit_idx, bit in enumerate(binary_text):
        if bit == "1" and xpos < wid:
            im[xpos, y] = colours[bit_idx // 8 % len(colours)]
        xpos += 1


def main(cmdlineargs):
    global debug
    global composite_filename, verbose

    try:
        opts, args = getopt.getopt(cmdlineargs,
                                   "c:hv", ["help"])
    except getopt.GetoptError as err:
        print(err,
              file=sys.stderr)
        usage(2)
    for opt, arg in opts:
        if opt == "-c":
            composite_filename = arg
        elif opt in ("-h", "--help"):
            usage(0)
        elif opt == "-v":
            verbose = True
        else:
            print("BAD OPTION",
                  file=sys.stderr)
            usage(2)

    if len(args) != 2:
       print("Incorrect number of arguments",
             file=sys.stderr)
       usage(2)

    width, height = map(int, args[0].split("x"))
    output = args[1]

    ### Four eight bit channels for in-memory image
    ### RGB plus Alpha
    channels = 4
    image = np.zeros((width, height, channels), dtype=np.uint8)

    ### order is very important here as there's a lot of over-writing
    background_grey_grid(image)
    fine_border_lines(image)
    mark_quarterpoints(image)
    cbar_width = colour_bars(image)
    rgb_colour_linear_graduated(image, bar_width=cbar_width)
    border = (width - cbar_width) // 2

    ### These two have matched heights
    grey_width, grey_height = grey_vbars(image, right=width - border)
    rgb_vwheel(image, bar_height=grey_height, right=width - border - grey_width)

    ### Keep this height as is - this may overlap the top bottom colours for small images
    bw_vstripes(image, left=border)

    image_cutout(image)
    binary_write(image, TESTCARD + " v" + VERSION)
    binary_write(image, "AYBABTU", y=2, colours=(GREY247,))

    ### not clear to me why I need to swap axes here
    imageio.imwrite(output, np.swapaxes(image, 0, 1))

    if composite_filename:
        print("TODO", "WRITE THIS!!!", "AND WORK OUT HOW TO DO INPUT AND OUTPUT FILENAMES")

    ### convert \( lf-ab-1-1080.jpg -splice 160x0 -resize x360 -gravity center -extent 1280x720 +repage \) test-24-1280x720.png -composite ctest42.png


if __name__ == "__main__":
    main(sys.argv[1:])
