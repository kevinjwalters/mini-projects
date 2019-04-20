### cpx-basic-synth-waveform-analysis.R v0.5
### replicate and analyse the waveform(s)
### use by cpx-basic-synth
### https://github.com/kevinjwalters/circuitpython-examples/blob/master/cpx/cpx-basic-synth.py

library(tuneR)  ### is this the one that can write wav files
library(ggplot2)
library(gridExtra)
library(grid)   ### for textGrob
library(data.table)
library(reshape2)

### 
### Adafruit CircuitPython 4.0.0-beta.7 on 2019-04-13; Adafruit CircuitPlayground Ex press with samd21g18
### >>>
### >>> import math
### >>> def sawtooth(angle):
### ...     return 1.0 - angle % (2 * math.pi) / (2 * math.pi) * 2
### ...
### ...
### ...
### >>> length = 12
### >>> vol = 32767
### >>> [round(vol * sawtooth((idx + 0.5) / length * 2 * math.pi + math.pi)) for idx in list(range(length))]
### [-2731, -8192, -13653, -19114, -24575, -30036, 30036, 24575, 19114, 13653, 8192,  2731]
###
### >>> 440*12
### 5280

### Not exactly sure how the timers work for sample playback so this will
### only be approximate for x axis (time)
### current approach is to upsample it to 350000 but this is probably
### going to be very slightly different to how it actually works

# Create at 2400:1800 (4:3 ratio)
gdpi <- 100
gheight <- 18
gwidth <- 24
#gheight <- 12
#gwidth <- 16


# I think this is based on 7 cycles
maxsamplerate <- 350000
arefvolt <- 3.3

osc_freq <- 440

### This could be made a bit more general purpose...
quantize_10bit <- function (x) {
  unsigned_x <- x + 2^15
  u10bit_x <- unsigned_x - unsigned_x %% 2^(16-10)
  u10bit_x - 2^15
}

sawtooth_fn <- function (angle) {
  1 - angle %% (2 * pi) / (2 * pi) * 2
}

# upsample with no interpolation just repetition of the numbers
# not sure of approx() gives a good even distribution
# seems better if rep() does most of the work first
cupsample <- function (samples, len) {
  approx(x=rep(samples,each=len%/%length(samples)), n=len, method="constant")$y
}

### These are the "raw" values not yet quantised by the SAMD21 10 bit DAC
### as this is not visible from CircuitPython
wavesamples <- c(-2731, -8192, -13653, -19114, -24575, -30036,
                 30036, 24575, 19114, 13653, 8192,  2731)

print(paste("mean:", mean(wavesamples),
            "max:", max(wavesamples),
            "min:", min(wavesamples)))

print(paste("quanitized",
            "mean:", mean(quantize_10bit(wavesamples)),
            "max:", max(quantize_10bit(wavesamples)),
            "min:", min(quantize_10bit(wavesamples))))

# upsample to increase resolution of everything            
upsample <- floor(maxsamplerate / osc_freq / length(wavesamples))

### This is not a real fixed playback rate it's just for calculations
playbackrate <- osc_freq * length(wavesamples) * upsample  # 63360

# Make one second of upsampled samples to emulate a zero hold DAC
# upsampled (i.e. stretched out) to the maxsamplerate
# and normalise amplitude to +/- 1
max_sample <- max(abs(quantize_10bit(wavesamples)))
one_second_of_samples <- cupsample(rep(rep(quantize_10bit(wavesamples) / max_sample,
                                           each=upsample),
                                       osc_freq),
                                   maxsamplerate)

# addition of pi is to phase shift it to match the python version
hires_wavesamples <- round(sawtooth_fn(seq(0, upsample * length(wavesamples) - 1)
                                       * 2 * pi / (upsample * length(wavesamples))
                                       + pi) * 32767)
                             
max_hires_sample <- max(abs(quantize_10bit(hires_wavesamples)))
one_second_of_hires_samples <- cupsample(rep(quantize_10bit(hires_wavesamples) / max_hires_sample,
                                             osc_freq),
                                         maxsamplerate)
                             
fft_magn_onesec <- abs(fft(one_second_of_samples))
power_spectrum_db <- log10(fft_magn_onesec / max(fft_magn_onesec)) * 20
power_spectrum_db_pos60 <- ifelse(power_spectrum_db > -60,
                                  power_spectrum_db + 60, NA)

fft_magn_onesec_hires <- abs(fft(one_second_of_hires_samples))
power_spectrum_hires_db <- log10(fft_magn_onesec_hires / max(fft_magn_onesec_hires)) * 20
power_spectrum_hires_db_pos60 <- ifelse(power_spectrum_hires_db > -60,
                                        power_spectrum_hires_db + 60, NA)
                                  
dat1 <- data.frame(x=seq(0, length(one_second_of_samples)-1),
                   xtimeus=seq(0, length(one_second_of_samples)-1) / length(one_second_of_samples) * 1e6,
                   samples=one_second_of_samples,
                   hires_samples=one_second_of_hires_samples,
                   power_spectrum_db=power_spectrum_db,
                   power_spectrum_hires_db=power_spectrum_hires_db,
                   power_spectrum_db_pos60=power_spectrum_db_pos60,
                   power_spectrum_hires_db_pos60=power_spectrum_hires_db_pos60
                  )


#startwave <- len(wavesamples) * upsample / 2
#endwave <- startwave + len(wavesamples) * upsample - 1

### two samples
starttimeus <- 1e6 / osc_freq / 2
endtimeus <- starttimeus + 2 * 1e6 / osc_freq

# spaces on label names are hack to get them spread out a bit more for top position
bothwaves <- ggplot(aes(x=xtimeus),data=dat1) +
             ggtitle("Two simulated samples - actual vs maximum resolution") +
             theme_light(base_size=42) +
             theme(legend.position="top", legend.title=element_blank(),
                   legend.spacing.x = unit(0.5, 'char'),
                   plot.title = element_text(hjust = 0.5)) +
             geom_step(aes(y=samples,color="actual"), size=2, alpha=0.85) +
             geom_step(aes(y=hires_samples, color="maxres"), alpha=0.85) +
             guides(color = guide_legend(override.aes = list(size=2))) +
             scale_y_continuous("amplitude",
                                sec.axis=sec_axis(~ (. + 1 ) / 2 * arefvolt,
                                name = "DAC output on A0 (V)")) +
             scale_color_manual(name="Samples",
                                values=c("actual"="red", "maxres"="dodgerblue"),
                                labels=c("actual"="actual  ",
                                         "maxres"="maxres  ")) + 
             scale_x_continuous(expression(paste("time (", mu, "s)", sep="")),
                                limits=c(starttimeus, endtimeus))

### TODO - plot hires one in a useful way - perhaps on separate graphs is best
### up and under then the frequencies line up?

### creates a df with vectors (x,variable,value) using melt with just the
### frequencies we are interested in
minfreq = 1
maxfreq = 20000
dat2 <- melt(dat1[(minfreq+1):(maxfreq+1),
                  c("x",
                    "power_spectrum_db_pos60",
                    "power_spectrum_hires_db_pos60")], id.vars = 1)

# note huge width to get visibility of bars on 1600x1200 / 2400x1800 output
# 20-60dB to show bit more detail
# lot more useful if breaks starts at osc_freq
powerspec   <- ggplot(dat2, aes(x=factor(x), y=value)) +
               ggtitle("Power spectrum - actual vs maximum resolution sample") +
               theme_light(base_size=42) +
               theme(legend.position="top",
                     legend.title=element_blank(),
                     legend.spacing.x = unit(0.5, 'char'),
                     plot.title = element_text(hjust = 0.5),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.text.x=element_text(hjust=0.5, vjust=0.5, angle=90)) +
               labs(x="frequency (Hz)", y="power (dB)") +
               geom_col(aes(fill=variable), position="dodge", width=300) +
               scale_x_discrete(breaks=seq(osc_freq, maxfreq, by=osc_freq*2)) +
               scale_fill_manual(values=c("power_spectrum_db_pos60"="red",
                                          "power_spectrum_hires_db_pos60"="dodgerblue"),
                                 labels=c("power_spectrum_db_pos60"="actual  ",
                                          "power_spectrum_hires_db_pos60"="maxres  ")) +
               scale_y_continuous(breaks=seq(20, 60, by=10)) +
               coord_cartesian(xlim=c(minfreq, maxfreq), ylim = c(20, 60))

ggsave(paste("cpx-basic-synth-sawtooth-waves-1.png"),
       bothwaves,
       dpi=gdpi, height=gheight, width=gwidth, units="in")

ggsave(paste("cpx-basic-synth-sawtooth-spectra-1.png"),
       powerspec,
       dpi=gdpi, height=gheight, width=gwidth, units="in")

#bothwaves
#powerspec
