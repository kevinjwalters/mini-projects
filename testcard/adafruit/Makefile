
MAKETESTCARD=../maketestcard.py

LCD_RESOLUTIONS=96x64 160x80 160x128 240x135 240x240 320x240 480x320 800x480 1024x600 1280x800 1366x768 
EXTRA_RESOLUTIONS=1152x900 1280x1024 1600x1200 1920x1080 2400x1600 2560x1440 2560x1600 2880x1880 4096x2160
RESOLUTIONS=$(LCD_RESOLUTIONS) $(EXTRA_RESOLUTIONS)

PNGFILES=$(foreach res,$(RESOLUTIONS),images/adafruit-testcard-a-$(res).png)
BMP16FILES=$(PNGFILES:png=16.bmp)
BMP24FILES=$(PNGFILES:png=24.bmp)
FILES=$(PNGFILES) $(BMP16FILES) $(BMP24FILES)

all: images $(FILES)

lf-ab-1-1080.jpg:
	echo YouTube video grab from end of https://www.youtube.com/watch?v=4lGRgO40UhM

logo_2x.png:
	wget https://learn.adafruit.com/logos/logo_2x.png

images:
	mkdir -p images

$(PNGFILES): logo_2x.png lf-ab-1-1080.jpg
	@echo Crafting $@
	@filename=$@ ; \
	res=`basename $${filename##*-} .png` ; \
	height=$${res#*x} ; \
        halfheight=$$((height / 2)) ; \
        logopos=$$((halfheight * 55 / 100)) ; \
        logoheight=$$((height * (30+15) / 240)) ; \
	$(MAKETESTCARD) \
	  -c 'lf-ab-1-1080.jpg -splice 155x0 -resize x'$${halfheight}' -gravity center -extent '$${res}' +repage' \
	  -e 'logo_2x.png -bordercolor none -border 30 -channel RGBA -morphology Edge Disk:4 +channel -negate -resize x'$$logoheight' -gravity Center -geometry +0+'$${logopos} \
	  -e 'logo_2x.png -bordercolor none -border 30 -resize x'$${logoheight}' -gravity Center -geometry +0+'$${logopos} \
	  $${res} $${filename}

### oddly ffmpeg can make 565 bmps and it makes them with minimal header
### the -sws_flags neighbor turns off dithering but throwing in
### -sws_dither none just in case
$(BMP16FILES): %.16.bmp: %.png
	@echo Transmogrifying $< $@
	@ffmpeg -loglevel warning -i $< -sws_flags neighbor -sws_dither none -pix_fmt rgb565 -y $@

$(BMP24FILES): %.24.bmp: %.png
	##convert $< $@
	@echo Transmogrifying $< $@
	@ffmpeg -loglevel warning -i $< -y $@
