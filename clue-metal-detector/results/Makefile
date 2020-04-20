
all: sample4-fitted-line-residuals-histogram.gif

sample4-fitted-line-residuals-histogram.gif:
	convert -delay 400 `ls sample4-fitted-line-residuals-histogram-*.png` `ls sample4-fitted-line-residuals-histogram-*.png | head -n -1 | tail -n +2 | sort -r` -loop 6 -layers optimize $@
