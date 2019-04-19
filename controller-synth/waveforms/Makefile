
animations: waveform-construction-sawtooth.v2.n10.gif waveform-construction-square.v2.n10.gif

%.n10.gif:
	@convert -delay 32 -resize 480x360 $$(ls frames/$$(echo $* | sed -e 's!\(.*\)\.\(v[0-9][0-9]*\)\(.*\)!\2/\1\3-*.png!')) -loop 0 $@

