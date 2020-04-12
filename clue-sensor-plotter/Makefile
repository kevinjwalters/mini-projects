
all: animations

animations: clue-magnetometer-comparison.gif clue-magnetometer-comparison-faster-smaller.gif

clue-magnetometer-comparison.gif: frames
	convert -delay 75 -resize 800x600 \
	        `ls $</*.png` \
	        `ls frames/*.png | head -n -1 | tail -n +2 | sort -r` \
	         -loop 4 -layers optimize $@

### Aiming for forum 1MB limit
clue-magnetometer-comparison-faster-smaller.gif: frames
	convert -delay 8 -resize 584x438 \
	        `ls $</*.png` \
	        `ls frames/*.png | head -n -1 | tail -n +2 | sort -r` \
	         -loop 0 -layers optimize $@

