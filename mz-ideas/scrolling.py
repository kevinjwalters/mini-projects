# Scrolling for 8x8 rgb tuples - ideas for Mission Zero

def scroll_up(image, rows=8, cols=8, fill=(0,0,0)):
    if isinstance(fill, str) and fill == "rotate":
        new_col = image[0:cols]
    else:
        new_col = [fill] * cols
    new_image = image[cols:] + new_col
    return new_image

# Avoiding use of negative values in slices intentionally
def scroll_down(image, rows=8, cols=8, fill=(0,0,0)):
    if isinstance(fill, str) and fill == "rotate":
        new_col = image[len(image)-cols:]
    else:
        new_col = [fill] * cols
    new_image = new_col + image[:len(image)-cols]
    return new_image

def scroll_left(image, rows=8, cols=8, fill=(0,0,0)):
    new_image = []
    col_start = 1
    if isinstance(fill, str) and fill == "rotate":
        for _ in range(rows):
            new_image.extend(image[col_start:col_start+cols-1] + [image[col_start-1]])
            col_start += cols
    else:
        for _ in range(rows):
            new_image.extend(image[col_start:col_start+cols-1] + [fill])
            col_start += cols 
    return new_image

def scroll_right(image, rows=8, cols=8, fill=(0,0,0)):
    new_image = []
    col_start = 0
    if isinstance(fill, str) and fill == "rotate":
        for _ in range(rows):
            new_image.extend([image[col_start+cols-1]] + image[col_start:col_start+cols-1])
            col_start += cols
    else:
        for _ in range(rows):
            new_image.extend([fill] + image[col_start:col_start+cols-1])
            col_start += cols 
    return new_image


# a = [10] * 8 + [20] * 8 + [30] * 8 + [40] * 8 + [50] * 8 + [60] * 8 + [70] * 8 + [80] * 8
a = list(range(1, 64 + 1))

print("image     ", a)
print("")
print("scrolled U", scroll_up(a))
print("rotated  U", scroll_up(a, fill='rotate'))
print("")
print("scrolled D", scroll_down(a))
print("rotated  D", scroll_down(a, fill='rotate'))
print("")
print("scrolled L", scroll_left(a))
print("rotated  L", scroll_left(a, fill='rotate'))
print("")
print("scrolled R", scroll_right(a))
print("rotated  R", scroll_right(a, fill='rotate'))
