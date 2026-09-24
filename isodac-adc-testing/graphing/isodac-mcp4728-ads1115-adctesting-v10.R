### isodac-mcp4728-ads1115-adctesting v10

### MIT License

### Copyright (c) 2026 Kevin J. Walters

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

### SPDX-FileCopyrightText: 2026 Kevin J. Walters

### This plotting code is untidy with a lot of cut and paste
### and in need of a review to check the correctness of the 
### processing/calculation/statistics code
### Some efficiency tweaks and perhaps parallelism would also help


library("ggplot2")
library("stringr")
library("dplyr")
library("tidyr")
library("tidyverse")
library("reshape2")
##library("grid")
##library("gridExtra")
library("ggforce")
library('ggnewscale')
library("ggrepel")
library("ggtext")
library("litedown")
##library("colorspace")
library("scales")
##library('cowplot')
library('bit64')
library('RColorBrewer')



options(dplyr.summarise.inform=FALSE)  ### don't show the "`summarise()` has grouped output by ..."


myversion <- "v10"
chart_filebase <- "isodac-mcp4728-ads1115-adctesting-"
csv_filename <- paste0("adcdata-", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC"), ".csv")
### Empty csv_df
e_csv_df <- data.frame("Board_Manufacturer"=character(0),
                       "Board_Name"=character(0),
                       "Board_Instance"=character(0),
                       "Language"=character(0),
                       "Power"=character(0),
                       "Nominal_V"=numeric(0),
                       "Actual_V"=numeric(0),
                       "Bits"=integer(0),
                       "ENOB_(t+g+vl)"=numeric(0),
                       "ENOB_(n)"=numeric(0),
                       "Noise_(n)_mV"=numeric(0),
                       "Distortion (t+g+vl)_mV"=numeric(0),
                       "Read_Function"=character(0),
                       "Min_V"=numeric(0),
                       "Max_V"=numeric(0),
                       "Coverage_pct"=numeric(0),
                       "Min_code"=integer(0),
                       "Max_code"=integer(0),
                       "Absent_codes"=integer(0),
                       "Absent_code_groups"=integer(0),
                       "Noise_model"=character(0),
                       "Noise_voltage_constant1"=numeric(0),
                       "Noise_voltage_constant2"=numeric(0),
                       "Noise_voltage_coefficient"=numeric(0),
                       "Ambient_temperature"=numeric(0),
                       "Software_version"=character(0),
                       "Plot_name"=character(0),
                       "Sample_counts"=character(0),
                       "Notes"=character(0))
### The warning line version
all_csv_df <- data.frame("Board_Manufacturer"="Please read article on",
                         "Board_Name"="https://www.instructables.com/member/kevinjwalters",
                         "Board_Instance"="to understand the limitations of this data",
                         "Language"=NA,
                         "Power"=NA,
                         "Nominal_V"=NA,
                         "Actual_V"=NA,
                         "Bits"=NA,
                         "ENOB_(t+g+vl)"=NA,
                         "ENOB_(n)"=NA,
                         "Noise_(n)_mV"=NA,
                         "Distortion (t+g+vl)_mV"=NA,
                         "Read_Function"=NA,
                         "Min_V"=NA,
                         "Max_V"=NA,
                         "Coverage_pct"=NA,
                         "Min_code"=NA,
                         "Max_code"=NA,
                         "Absent_codes"=NA,
                         "Absent_code_groups"=NA,
                         "Noise_model"=NA,
                         "Noise_voltage_constant1"=NA,
                         "Noise_voltage_constant2"=NA,
                         "Noise_voltage_coefficient"=NA,
                         "Ambient_temperature"=NA,
                         "Software_version"=NA,
                         "Plot_name"=NA,
                         "Sample_counts"=NA,
                         "Notes"=NA)

##shape_point_noborder <- 16  ### The borderless point, borders don't alpha.

dac_bits <- 12
dac_code_num <- 2**12

READ_RAW = "raw"
READ_CALIBRATEDMV = "calibratedmv"

READ_MAP = c("analogRead()",
             "analogReadMilliVolts()")
names(READ_MAP) <- c(READ_RAW, READ_CALIBRATEDMV)


### Output example
#
# 16430454650886,"dac","B",0,"S","max",0,"[-1]",-0.0001875,NA
# 16430466491708,"dac","B",0,"S","max",0,"[-1]",0.0,NA
# 16430488403322,"dac","B",0,"S","max",0,"[0]",0.001125,NA
# 16430501373296,"dac","B",0,"S","max",0,"[1]",0.00225,NA
# 16430514038091,"dac","B",0,"S","max",0,"[2]",0.0035625,NA
# 16430526550295,"dac","B",0,"S","max",0,"[3]",0.004875,NA
# 
# 17067400299084,"triangle","B",0,"S","max",0,"[-1]",0.0,"[27,30,28,25,28,30,30,30,26,24]"
# 17067440521241,"triangle","B",0,"S","max",0,"[0]",0.001125,"[17,17,17,10,17,15,10,17,15,12]"
# 17067472656250,"triangle","B",0,"S","max",0,"[1]",0.00225,"[21,25,20,16,23,21,20,21,19,14]"
# 17067504699718,"triangle","B",0,"S","max",0,"[2]",0.0035625,"[22,24,25,22,24,26,27,24,22,22]"
# 17067536682136,"triangle","B",0,"S","max",0,"[3]",0.004875,"[30,30,28,28,28,31,27,29,28,30]"
# 
# 19041505584726,"gaps","B;C",0,"S","max",0,"[0,3]",0.003375,"[24,22,25,20,25,22,24,26,18,21]"
# 19041538604743,"gaps","B;C",0,"S","max",0,"[1,4]",0.0043125,"[26,28,23,25,30,24,28,30,21,25]"
# 19041571502688,"gaps","B;C",0,"S","max",0,"[1,6]",0.005624999,"[33,31,30,28,33,33,32,33,30,27]"
# 19041604705813,"gaps","B;C",0,"S","max",0,"[5,7]",0.0088125,"[44,46,20,43,44,46,42,44,43,42]"
# 19041637512218,"gaps","B;C",0,"S","max",0,"[0,16]",0.012749996,"[58,59,56,54,59,59,59,58,55,65]"
# 19041670715343,"gaps","B;C",0,"S","max",0,"[4,13]",0.0121875,"[56,55,56,53,51,54,56,54,50,52]"
# 
# 23496132080081,"verylow","B",0,"S","max",0,"[0]",0.0009375,"[18,16,12,14,17]"
# 23496155883797,"verylow","B",0,"S","max",0,"[1]",0.0020625,"[18,22,23,19,20]"
# 23496179534923,"verylow","B",0,"S","max",0,"[2]",0.003375,"[25,27,25,22,21]"
# 23496203308108,"verylow","B",0,"S","max",0,"[3]",0.0045,"[24,26,30,23,24]"
# 23496226928716,"verylow","B",0,"S","max",0,"[4]",0.005624999,"[34,46,33,30,34]"
# 23496250671396,"verylow","B",0,"S","max",0,"[5]",0.00675,"[37,34,38,35,36]"
# 23496274292004,"verylow","B",0,"S","max",0,"[6]",0.0080625,"[42,40,43,38,38]"
# 23496297851562,"verylow","B",0,"S","max",0,"[7]",0.0091875,"[48,43,42,38,46]"
# 
# 23599203735360,"noise","B",0,"C","2048",0,"[1397]",0.6999374,"[2490,2495,2490,2487,2488,2490,2493,2488,2486,2481,2486,2486,2486,2485,2481,2483,2486,2482,2484,2484,2483,2484,2490,2484,2488,2489,2487,2486,2486,2486,2484,2485,2485,2484,2484,2485,2482,2483,2482,2479,2478,2483,2480,2485,2483,2479,2484,2485,2480,2486,2479,2482,2482,2479,2485,2484,2481,2482,2485,2480,2482,2486,2484,2487,2486,2482,2483,2484,2482,2481,2478,2482,2478,2480,2483,2479,2482,2481,2478,2480,2484,2480,2484,2482,2480,2479,2482,2485,2487,2482,2486,2489,2484,2488,2487,2484,2487,2485,2483,2491,2487,2484,2482,2481,2483,2480,2483,2484,2480,2484,2485,2479,2483,2485,2482,2483,2481,2480,2482,2484,2483,2484,2485,2485,2485,2480,2483,2487,2490,2485,2487,2483,2488,2487,2488,2488,2492,2488,2489,2489,2487,2484,2486,2489,2476,2486,2484,2480,2486,2491,2480,2484,2484,2480,2482,2483,2482,2484,2485,2482,2483,2478,2483,2483,2480,2483,2485,2479,2481,2475,2480,2482,2482,2484,2488,2484,2484,2482,2483,2483,2479,2483,2479,2478,2480,2482,2479,2479,2481,2480,2486,2484,2480,2479,2483,2480,2482,2482,2480,2480]"
# 23599586853040,"noise","B",0,"C","2048",0,"[1995]",0.99975,"[3552,3554,3555,3551,3554,3559,3558,3556,3555,3559,3556,3556,3556,3552,3552,3553,3549,3552,3550,3556,3563,3555,3553,3556,3552,3552,3557,3553,3556,3552,3553,3552,3550,3549,3553,3545,3551,3553,3550,3550,3548,3549,3548,3548,3552,3545,3547,3551,3546,3547,3549,3544,3548,3551,3544,3545,3549,3551,3551,3550,3546,3546,3551,3551,3554,3554,3549,3547,3550,3550,3545,3547,3549,3549,3552,3546,3547,3546,3545,3546,3550,3546,3546,3548,3544,3545,3547,3544,3546,3546,3542,3549,3547,3546,3560,3557,3555,3555,3555,3554,3552,3553,3552,3548,3556,3550,3546,3550,3550,3548,3549,3553,3547,3549,3546,3547,3547,3551,3548,3552,3555,3550,3553,3550,3552,3554,3555,3553,3554,3557,3556,3561,3559,3558,3558,3554,3556,3552,3556,3557,3553,3554,3555,3556,3555,3558,3556,3553,3550,3554,3553,3550,3553,3552,3548,3548,3550,3549,3551,3550,3549,3547,3548,3546,3546,3546,3546,3545,3554,3551,3545,3546,3546,3542,3548,3550,3546,3548,3547,3546,3542,3547,3543,3546,3544,3544,3548,3544,3547,3546,3547,3545,3546,3544,3551,3544,3547,3541,3546,3544]"


### This should be 0.25 and 0.75 for IQR
### but I've tweaked the values for a wider centre cut!
iqrmean <- function(x) { mean(x[x >= quantile(x, 0.20, type=2) & x <= quantile(x, 0.80, type=2)]) }


read_data_file <- function(filename, but_v, dac_vdd) {
  all_text_lines <- readLines(filename)

  ### This really needs nested parsing rather than this hacky regex mess...
  but_info_line <- na.omit(str_match(all_text_lines, '### Board: \\{(.*)\\} ###')[,2])[1]
  but_info_crude <- gsub("'data': \\{|\\}", "",  str_split(but_info_line, ",")[[1]])
  but_data <- gsub("^[ '\"]*", "", gsub("[ '\"]*$", "", sapply(str_split(but_info_crude, ":", 2), '[', 2)))
  names(but_data) <- gsub("^[ '\"]*", "", gsub("[ '\"]*$", "", sapply(str_split(but_info_crude, ":", 2), '[', 1)))

  aref <- as.numeric(but_data["aref"])
  adc_bits <- as.integer(but_data["adc_bits"])
  if (is.na(but_data["read"])) {
    but_read <- "raw"  ### Assume raw if not present
    but_data["read"] = but_read
  } else {
    but_read <- but_data["read"]
  }

  ### Filter out anything which doesn't have expected number of fields
  ### with primitive comma count
  good_selector <- !grepl("^\\s*#", all_text_lines) & str_count(all_text_lines, ",") >= 9
  data <- read.csv(text=all_text_lines[good_selector],
                   header=FALSE,
                   col.names=c("time_ns",
                               "test_name",
                               "dac_chans",
                               "adc_chan",
                               "adc_mode",
                               "dac_vref_mode",
                               "test_idx",
                               "dac_codes",
                               "adc_v",
                               "but_adc_codes"),
                   colClasses=c("integer64",
                                "character",
                                "character",
                                "integer",
                                "character",
                                "character",
                                "integer",
                                "character",
                                "numeric",
                                "character"))

  data$test_name <- as.factor(data$test_name)
  data$dac_chans <- as.factor(data$dac_chans)
  data$adc_chan <- as.factor(data$adc_chan)
  data$adc_mode <- as.factor(data$adc_mode)
  data$dac_vref_mode <- as.factor(data$dac_vref_mode)

  data$dac_code_1 <- as.integer(sapply(strsplit(gsub("\\[|\\]", "", data$dac_codes), ","), '[', 1))
  data$dac_code_2 <- as.integer(sapply(strsplit(gsub("\\[|\\]", "", data$dac_codes), ","), '[', 2))

  data$but_adc_ref_nominal_v <- aref
  data$but_adc_ref_actual_v <- but_v
  data$but_adc_bits <- adc_bits

  ### as.numeric(as.character()) is needed - always get caught by that
  ### direction will only work properly for single channel tests
  data <- data %>%
    group_by(test_name, dac_chans, adc_chan, adc_mode, dac_vref_mode, test_idx) %>%
    mutate(idx=row_number(),
           reltime_s=(time_ns - min(time_ns)) / 1e9,
           direction=as.factor(replace_na(if_else(lag(dac_code_1) <= dac_code_1, "rising", "falling"), "rising")),
           vref=as.factor(round(ifelse(dac_vref_mode == "max",
                                dac_vdd,
#                                 max(adc_v) * dac_code_num / (dac_code_num - 1), ## only works if tests go full DAC range
                                 as.numeric(as.character(dac_vref_mode)) / 1000.0), 3))) %>%
    ungroup()

  if (but_read == READ_CALIBRATEDMV) {
    ### Convert the ESP32 calibrated values from analogReadMilliVolts()
    ### leave it as floating point value and for now don't limit range to normal N bit range
    adc_processor <- function(dc) { return(as.integer(dc) / 1000 / but_v * 2**adc_bits) }
    #adc_processor <- function(dc) { return(pmin(2**adc_bits - 1,
    #                                            pmax(0,
    #                                                 round(as.integer(dc) / 1000 / but_v * 2**adc_bits)))) }
  } else {
    adc_processor <- as.integer 
  }

  
  
  expanded_data <- data %>%
    mutate(but_adc_code=strsplit(gsub("\\[|\\]", "", but_adc_codes), ",")) %>%
    unnest(but_adc_code) %>%
    mutate(but_adc_codesize=2**adc_bits,
           but_adc_unproc_code=as.integer(but_adc_code),
           but_adc_code=adc_processor(but_adc_code),
           but_nominal_v=but_adc_code / but_adc_codesize * but_adc_ref_nominal_v,
           but_actual_v=but_adc_code / but_adc_codesize * but_adc_ref_actual_v) %>%
    select(-but_adc_codes)  ### delete this column now it's redundant

  return(list(data_df=data, expanded_data_df=expanded_data, but_data=but_data))
}

usb_desktop_power = "Desktop USB"
usb_laptop_power = "Laptop USB"
pb_power = "PB USB"
battreg_power_bypass = "reg. 6xNiMH to 3.3V"
battreg_power_bypass50 = "reg. 6xNiMH to 5.0V"
battreg_power = "reg. 6xNiMH"
alk2bat_power = "2xAlkaline"
alk3bat_power = "3xAlkaline"
lipo_power = "LiPo"



# Analogue pin wasn't connected for these ones :(
# putty-adctestsg-v1.1-pipico-usb-20260828-i.txt
# putty-adctestsg-v1.1-pipico-nimh33-20260828-i.txt
# putty-adctestsg-v1.1-pipico-nimh-20260828-i.txt
# putty-adctestsg-v1.1-pipico2w-usb-20260828-i.txt
# putty-adctestsg-v1.1-pipico2w-nimh-20260828-i.txt
#
# Something wrong with this one
# putty-adctestsg-v1.1-feathers2-lipo-20260830-i.txt
#
# short aborted file
# putty-adctestsg-v1.1-esp32clone-nimh-20260831-i.txt 
#
# premature termination due to me looknig at laptop which caused a serial disconnect!
# putty-adctestsg-v1.1-pipicowmcp3208-nimh33-20260910-i.txt
# 


extra_data <- list(
                   ### These first few are from the development of the programs - these are included
                   ### because it turns out the desktop USB is noisy as hell
                   "UNO R4 Minima : USB t0"=list(but_v=4.704,
                                          dac_vdd=5.229,
                                          power=usb_desktop_power,
                                          temp=27,
                                          plotname="unor4minima-noisyusb-0",
                                          filename="data/adc-test-prelim/putty-adctestsg-v0.13-unor4minima-usb-20260820-i.txt",
                                          notes="Noisy desktop USB test from development stage",
                                          csv_notes="from development testing"),
                   "Leonardo : USB t0"=list(but_v=5.011,
                                            dac_vdd=5.229,
                                            power=usb_desktop_power,
                                            temp=27,
                                            plotname="leonardo-noisyusb-0",
                                            filename="data/adc-test-prelim/putty-adctestsg-v0.13-leonardo-usb-20260821-i.txt",
                                            notes="Noisy desktop USB test from development stage",
                                            csv_notes="from development testing"),
                   "Xiao ESP32C6 : USB t0"=list(but_v=3.297,
                                                dac_vdd=5.229,
                                                power=usb_desktop_power,
                                                temp=26,
                                                plotname="xiaoesp32c6-noisyusb-0",
                                                filename="data/adc-test-prelim/putty-adctestsg-v0.13-xiaoesp32c6-usb-20260822-i.txt",
                                                notes="Noisy desktop USB test from development stage",
                                                csv_notes="from development testing"),
                   "Xiao ESP32C5 : USB t0"=list(but_v=3.293,
                                                dac_vdd=5.206,
                                                power=usb_desktop_power,
                                                temp=27,
                                                plotname="xiaoesp32c5-noisyusb-0",
                                                filename="data/adc-test-prelim/putty-adctestsg-v0.13-xiaoesp32c5-usb-20260822-i.txt",
                                                notes="Noisy desktop USB test from development stage",
                                                csv_notes="from development testing"),
                   "CPX (K1) : USB t0"=list(but_v=3.277,
                                           dac_vdd=5.242,
                                           power=usb_desktop_power,
                                           temp=27,
                                           plotname="cpxk1-noisyusb-0",
                                           filename="data/adc-test-prelim/putty-adctestsg-v1.1-cpx-usb-20260826-i.txt",
                                           notes="Noisy desktop USB test from development stage",
                                           csv_notes="from development testing"),
                   "Pi Pico : USB t0"=list(but_v=3.277,
                                           dac_vdd=5.242,
                                           power=usb_desktop_power,
                                           temp=27,
                                           plotname="pipico-noisyusb-0",
                                           filename="data/adc-test-prelim/putty-adctestsg-v1.0-pipico-usb-20260825-i.txt",
                                           notes="Noisy desktop USB test from development stage",
                                           csv_notes="from development testing"),

                   ### These are the from the main test run for the Instructables article
                   "CPX (K3) : USB t1"=list(but_v=3.338,
                                           dac_vdd=5.244,
                                           power=usb_laptop_power,
                                           temp=24.8,
                                           plotname="cpxk3-usb-1",
                                           filename="data/adc-test/putty-adctestsg-v1.1-cpxk3-usb-20260826-i.txt",
                                           notes=""),
                   "CPX (K3) : LiPo t1"=list(but_v=3.338,
                                            dac_vdd=5.250,
                                            power=lipo_power,
                                            temp=25.0,
                                            plotname="cpxk3-lipo-1",
                                            filename="data/adc-test/putty-adctestsg-v1.1-cpxk3-lipo-20260827-i.txt",
                                            notes=""),

                   "Feather M4 : LiPo t1"=list(but_v=3.295,
                                               dac_vdd=5.250,
                                               power=lipo_power,
                                               temp=25.0,
                                               plotname="featherm4-lipo-1",
                                               filename="data/adc-test/putty-adctestsg-v1.1-featherm4-lipo-20260827-i.txt",
                                               notes=""),
                   "Feather M4 : USB t1"=list(but_v=3.293,
                                             dac_vdd=5.250,
                                             power=usb_laptop_power,
                                             temp=24.4,
                                             plotname="featherm4-usb-1",
                                             filename="data/adc-test/putty-adctestsg-v1.1-featherm4-usb-20260828-i.txt",
                                             notes=""),
                   
                   "CPX (K1) : USB t1"=list(but_v=3.279,
                                            dac_vdd=5.249,
                                            power=usb_laptop_power,
                                            temp=24.6,
                                            plotname="cpxk1-usb-1",
                                            filename="data/adc-test/putty-adctestsg-v1.1-cpx1-usb-20260828-i.txt",
                                            notes=""),
                   "CPX (K1) : LiPo t1"=list(but_v=3.280,
                                             dac_vdd=5.250,
                                             power=lipo_power,
                                             temp=24.8,
                                             plotname="cpxk1-lipo-1",
                                             filename="data/adc-test/putty-adctestsg-v1.1-cpx1-lipo-20260828-i.txt",
                                             notes=""),

                   "Pi Pico 2W {A2} : NiMH t1"=list(but_v=3.309,
                                                    dac_vdd=5.250,
                                                    power=battreg_power,
                                                    temp=24.4,
                                                    plotname="pipico2w-nimh-1",
                                                    filename="data/adc-test/putty-adctestsg-v1.1-pipico2w-nimh-20260829-ii.txt",
                                                    notes=""),                   
                   "Pi Pico 2W {A2} : USB t1"=list(but_v=3.300,
                                                   dac_vdd=5.250,
                                                   power=usb_laptop_power,
                                                   temp=24.6,
                                                   plotname="pipico2w-usb-1",
                                                   filename="data/adc-test/putty-adctestsg-v1.1-pipico2w-usb-20260829-ii.txt",
                                                   notes=""),  
                   "Pi Pico 2W {A2} : NiMH 3.3V t1"=list(but_v=3.297,
                                                         dac_vdd=5.249,
                                                         power=battreg_power_bypass,
                                                         temp=24.7,
                                                         plotname="pipico2w-nimh33-1",
                                                         filename="data/adc-test/putty-adctestsg-v1.1-pipico2w-nimh33-20260829-ii.txt",
                                                         notes=""),                   

                   "Pi Pico : NiMH 3.3V t1"=list(but_v=3.297,
                                                 dac_vdd=5.250,
                                                 power=battreg_power_bypass,
                                                 temp=24.9,
                                                 plotname="pipico-nimh33-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-pipico-nimh33-20260829-ii.txt",
                                                 notes=""),                   
                   "Pi Pico : NiMH t1"=list(but_v=3.272,
                                            dac_vdd=5.250,
                                            power=battreg_power,
                                            temp=24.4,
                                            plotname="pipico-nimh-1",
                                            filename="data/adc-test/putty-adctestsg-v1.1-pipico-nimh-20260829-ii.txt",
                                            notes=""),                   
                   "Pi Pico : USB t1"=list(but_v=3.272,
                                           dac_vdd=5.250,
                                           power=usb_laptop_power,
                                           temp=24.3,
                                           plotname="pipico-usb-1",
                                           filename="data/adc-test/putty-adctestsg-v1.1-pipico-usb-20260829-ii.txt",
                                           notes=""),
                   
                   "Tiny 2350 {A4} : USB t1"=list(but_v=3.307,
                                                  dac_vdd=5.250,
                                                  power=usb_laptop_power,
                                                  temp=24.2,
                                                  plotname="tiny2350-usb-1",
                                                  filename="data/adc-test/putty-adctestsg-v1.1-tiny2350-usb-20260829-i.txt",
                                                  notes=""),  
                   "Tiny 2350 {A4} : NiMH t1"=list(but_v=3.307,
                                                   dac_vdd=5.249,
                                                   power=battreg_power,
                                                   temp=24.3,
                                                   plotname="tiny2350-nimh-1",
                                                   filename="data/adc-test/putty-adctestsg-v1.1-tiny2350-nimh-20260829-i.txt",
                                                   notes=""), 
                   
                   "Xiao ESP32C5 : NiMH t1"=list(but_v=3.294,
                                                 dac_vdd=5.250,
                                                 power=battreg_power,
                                                 temp=24.1,
                                                 plotname="xiaoesp32c5-nimh-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-xiaoesp32c5-nimh-20260830-i.txt",
                                                 notes=""),  
                   "Xiao ESP32C6 : NiMH t1"=list(but_v=3.298,
                                                 dac_vdd=5.249,
                                                 power=battreg_power,
                                                 temp=24.4,
                                                 plotname="xiaoesp32c6-nimh-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-xiaoesp32c6-nimh-20260830-i.txt",
                                                 notes=""),  
                   "Xiao ESP32C6 : USB t1"=list(but_v=3.297,
                                                 dac_vdd=5.249,
                                                 power=usb_laptop_power,
                                                 temp=24.6,
                                                 plotname="xiaoesp32c6-usb-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-xiaoesp32c6-usb-20260830-i.txt",
                                                 notes=""),  
                   "Xiao ESP32C5 : USB t1"=list(but_v=3.294,
                                                 dac_vdd=5.251,
                                                 power=usb_laptop_power,
                                                 temp=24.6,
                                                 plotname="xiaoesp32c5-usb-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-xiaoesp32c5-usb-20260830-i.txt",
                                                 notes=""),
                   "Xiao ESP32C6 {NOCAL} : NiMH t1"=list(but_v=3.298,
                                                         dac_vdd=5.250,
                                                         power=battreg_power,
                                                         temp=24.4,
                                                         plotname="xiaoesp32c6raw-nimh-1",
                                                         filename="data/adc-test/putty-adctestsg-v1.1-xiaoesp32c6raw-nimh-20260830-i.txt",
                                                         notes=""),                     
                   "Xiao ESP32C5 {NOCAL} : NiMH t1"=list(but_v=3.293,
                                                         dac_vdd=5.250,
                                                         power=battreg_power,
                                                         temp=24.2,
                                                         plotname="xiaoesp32c5raw-nimh-1",
                                                         filename="data/adc-test/putty-adctestsg-v1.1-xiaoesp32c5raw-nimh-20260830-i.txt",
                                                         notes=""),                     
                   
                   "FeatherS2 : LiPo t1"=list(but_v=3.294,
                                              dac_vdd=5.250,
                                              power=lipo_power,
                                              temp=23.5,
                                              plotname="feathers2-lipo-1",
                                              filename="data/adc-test/putty-adctestsg-v1.1-feathers2-lipo-20260831-ii.txt",
                                              notes=""),  
                   "FeatherS2 : USB t1"=list(but_v=3.294,
                                              dac_vdd=5.249,
                                              power=usb_laptop_power,
                                              temp=23.8,
                                              plotname="feathers2-usb-1",
                                              filename="data/adc-test/putty-adctestsg-v1.1-feathers2-usb-20260831-ii.txt",
                                              notes=""),  
                   "FeatherS2 {NOCAL} : USB t1"=list(but_v=3.294,
                                                     dac_vdd=5.249,
                                                     power=usb_laptop_power,
                                                     temp=24.0,
                                                     plotname="feathers2raw-usb-1",
                                                     filename="data/adc-test/putty-adctestsg-v1.1-feathers2raw-usb-20260831-i.txt",
                                                     notes=""),                     
                   "FeatherS2 {NOCAL} : LiPo t1"=list(but_v=3.294,
                                                      dac_vdd=5.250,
                                                      power=lipo_power,
                                                      temp=24.0,
                                                      plotname="feathers2raw-lipo-1",
                                                      filename="data/adc-test/putty-adctestsg-v1.1-feathers2raw-lipo-20260831-i.txt",
                                                      notes=""),
                   
                   "ESP32 DevKitC Clone : USB t1"=list(but_v=3.270,
                                                       dac_vdd=5.249,
                                                       power=usb_laptop_power,
                                                       temp=24.4,
                                                       plotname="esp32clone-usb-1",
                                                       filename="data/adc-test/putty-adctestsg-v1.1-esp32clone-usb-20260831-i.txt",
                                                       notes=""),
                   "ESP32 DevKitC Clone {NOCAL} : USB t1"=list(but_v=3.271,
                                                               dac_vdd=5.249,
                                                               power=usb_laptop_power,
                                                               temp=24.7,
                                                               plotname="esp32cloneraw-usb-1",
                                                               filename="data/adc-test/putty-adctestsg-v1.1-esp32cloneraw-usb-20260831-i.txt",
                                                               notes=""),
                   "ESP32 DevKitC Clone {NOCAL} : USB PB t1"=list(but_v=3.271,
                                                                  dac_vdd=5.248,
                                                                  power=pb_power,
                                                                  temp=24.8,
                                                                  plotname="esp32cloneraw-usbpb-1",
                                                                  filename="data/adc-test/putty-adctestsg-v1.1-esp32cloneraw-usbpb-20260906-i.txt",
                                                                  notes=""),
                   "ESP32 DevKitC Clone {NOCAL} : NiMH t1"=list(but_v=3.272,
                                                                dac_vdd=5.248,
                                                                power=battreg_power,
                                                                temp=24.7,
                                                                plotname="esp32cloneraw-nimh-1",
                                                                filename="data/adc-test/putty-adctestsg-v1.1-esp32cloneraw-nimh-20260906-i.txt",
                                                                notes=""),
                   "ESP32 DevKitC Clone : NiMH t1"=list(but_v=3.273,
                                                        dac_vdd=5.249,
                                                        power=battreg_power,
                                                        temp=24.6,
                                                        plotname="esp32clone-nimh-1",
                                                        filename="data/adc-test/putty-adctestsg-v1.1-esp32clone-nimh-20260906-i.txt",
                                                        notes=""),

                   "Feather nRF52840 : USB t1"=list(but_v=3.314,
                                                    dac_vdd=5.250,
                                                    power=usb_laptop_power,
                                                    temp=24.4,
                                                    plotname="feathernrf52840-usb-1",
                                                    filename="data/adc-test/putty-adctestsg-v1.1-feathernrf52840-usb-20260907-i.txt",
                                                    notes=""), 
                   "Feather nRF52840 : LiPo t1"=list(but_v=3.314,
                                                     dac_vdd=5.249,
                                                     power=lipo_power,
                                                     temp=24.8,
                                                     plotname="feathernrf52840-lipo-1",
                                                     filename="data/adc-test/putty-adctestsg-v1.1-feathernrf52840-lipo-20260907-i.txt",
                                                     notes="noise is strange, perhaps LiPo dropped to point where regulator on the feather board could not produce 3.3V",
                                                     csv_skip=TRUE),
                   
                   "UNO R4 WiFi : NiMH t1"=list(but_v=4.985,
                                                dac_vdd=5.249,
                                                power=battreg_power,
                                                temp=25.3,
                                                plotname="unor4wifi-nimh-1",
                                                filename="data/adc-test/putty-adctestsg-v1.1-unor4wifi-nimh-20260907-i.txt",
                                                notes=""),         
                   "UNO R4 WiFi : USB t1"=list(but_v=4.619,
                                               dac_vdd=5.249,
                                               power=usb_laptop_power,
                                               temp=25.5,
                                               plotname="unor4wifi-usb-1",
                                               filename="data/adc-test/putty-adctestsg-v1.1-unor4wifi-usb-20260907-i.txt",
                                               notes=""), 
                   
                   "UNO R4 Minima : USB t1"=list(but_v=4.682,
                                                 dac_vdd=5.248,
                                                 power=usb_laptop_power,
                                                 temp=25.1,
                                                 plotname="unor4minima-usb-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-unor4minima-usb-20260907-i.txt",
                                                 notes="low code range observed, min code is 248, something isn't quiet right",
                                                 csv_notes="possible loose ground wire?"),   
                   "UNO R4 Minima : NiMH t1"=list(but_v=5.005,
                                                 dac_vdd=5.249,
                                                 power=battreg_power,
                                                 temp=24.5,
                                                 plotname="unor4minima-nimh-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-unor4minima-nimh-20260907-i.txt",
                                                 notes=""), 

                   "Leonardo : USB t1"=list(but_v=4.982,
                                            dac_vdd=5.250,
                                            power=usb_laptop_power,
                                            temp=24.4,
                                            plotname="leonardo-usb-1",
                                            filename="data/adc-test/putty-adctestsg-v1.1-leonardo-usb-20260908-i.txt",
                                            notes=""),                    
                   "Leonardo : NiMH t1"=list(but_v=4.999,
                                            dac_vdd=5.249,
                                            power=battreg_power,
                                            temp=24.6,
                                            plotname="leonardo-nimh-1",
                                            filename="data/adc-test/putty-adctestsg-v1.1-leonardo-nimh-20260908-i.txt",
                                            notes=""),         

                   "UNO R3 : NiMH t1"=list(but_v=4.980,
                                           dac_vdd=5.249,
                                           power=battreg_power,
                                           temp=24.8,
                                           plotname="unor3-nimh-1",
                                           filename="data/adc-test/putty-adctestsg-v1.1-unor3-nimh-20260908-i.txt",
                                           notes=""), 
                   "UNO R3 : USB t1"=list(but_v=4.975,
                                          dac_vdd=5.249,
                                          power=usb_laptop_power,
                                          temp=24.8,
                                          plotname="unor3-usb-1",
                                          filename="data/adc-test/putty-adctestsg-v1.1-unor3-usb-20260908-i.txt",
                                          notes=""),   

                   "UNO R3 Clone : USB t1"=list(but_v=4.964,
                                                dac_vdd=5.249,
                                                power=usb_laptop_power,
                                                temp=24.6,
                                                plotname="unor3clone-usb-1",
                                                filename="data/adc-test/putty-adctestsg-v1.1-unor3clone-usb-20260908-i.txt",
                                                notes=""),                    
                   "UNO R3 Clone : NiMH t1"=list(but_v=4.976,
                                                 dac_vdd=5.249,
                                                 power=battreg_power,
                                                 temp=24.4,
                                                 plotname="unor3clone-nimh-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-unor3clone-nimh-20260908-i.txt",
                                                 notes=""),

                   "micro:bit V1 : USB t1"=list(but_v=3.166,
                                                dac_vdd=5.250,
                                                power=usb_laptop_power,
                                                temp=24.5,
                                                plotname="microbitv1-usb-1",
                                                filename="data/adc-test/putty-adctestsg-v1.1-microbitv1-usb-20260909-i.txt",
                                                notes=""),                    
                   "micro:bit V1 : 2Alk t1"=list(but_v=2.906,  ### 2.898V at end
                                                 dac_vdd=5.250,
                                                 power=alk2bat_power,
                                                 temp=24.5,
                                                 plotname="microbitv1-2alk-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-microbitv1-2alk-20260909-i.txt",
                                                 notes="",
                                                 csv_notes="voltage significantly above Vdd"),

                   "micro:bit V2 : 2Alk t1"=list(but_v=2.710,  ### didn't write down end voltage
                                                 dac_vdd=5.249,
                                                 power=alk2bat_power,
                                                 temp=24.6,
                                                 plotname="microbitv2-2alk-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-microbitv2-2alk-20260909-i.txt",
                                                 notes="",
                                                 csv_notes="Vdd lifting due to back-powering"),
                   "micro:bit V2 : 3Alk t1"=list(but_v=3.288,   ### 3.287 at end
                                                 dac_vdd=5.250,
                                                 power=alk3bat_power,
                                                 temp=24.3,
                                                 plotname="microbitv2-3alk-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-microbitv2-3alk-20260909-i.txt",
                                                 notes=""),
                   "micro:bit V2 : USB t1"=list(but_v=3.287,
                                                 dac_vdd=5.250,
                                                 power=usb_laptop_power,
                                                 temp=23.9,
                                                 plotname="microbitv2-usb-1",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-microbitv2-usb-20260910-i.txt",
                                                 notes=""),
                   
                   "Meowbit : USB t1"=list(but_v=3.312,
                                           dac_vdd=5.249,
                                           power=usb_laptop_power,
                                           temp=23.9,
                                           plotname="meowbit-usb-1",
                                           filename="data/adc-test/putty-adctestsg-v1.1-meowbit-usb-20260910-i.txt",
                                           notes=""),
                   "Meowbit : LiPo t1"=list(but_v=3.310,
                                           dac_vdd=5.249,
                                           power=lipo_power,
                                           temp=24.4,
                                           plotname="meowbit-lipo-1",
                                           filename="data/adc-test/putty-adctestsg-v1.1-meowbit-lipo-20260910-i.txt",
                                           notes=""),
### Extras

                   "UNO R4 Minima : NiMH 5.0 t1"=list(but_v=4.973,
                                                      dac_vdd=5.249,
                                                      power=battreg_power_bypass50,
                                                      temp=24.6,
                                                      plotname="unor4minima-nimh50-1",
                                                      filename="data/adc-test/putty-adctestsg-v1.1-unor4minima-nimh50-20260910-i.txt",
                                                      notes=""), 

                   "Feather nRF52840 : LiPo t2"=list(but_v=3.315,
                                                     dac_vdd=5.249,
                                                     power=lipo_power,
                                                     temp=24.4,
                                                     plotname="feathernrf52840-lipo-2",
                                                     filename="data/adc-test/putty-adctestsg-v1.1-feathernrf52840-lipo-20260910-ii.txt",
                                                     notes="repeat with LiPo starting at 4.180V"),
                   
                   "MCP3208 (IC1) : NiMH t1"=list(but_v=3.302,
                                                  dac_vdd=5.249,
                                                  power=battreg_power,
                                                  temp=24.4,
                                                  plotname="mcp3208ic1-nimh-1",
                                                  filename="data/adc-test/putty-adctestsg-v1.1-pipicowmcp3208-nimh33-20260910-ii.txt",
                                                  notes="MCP3208 hanging off Pi Pico WH on a Cytron EDU PICO"),
                   "MCP3208 (IC2) : NiMH t1"=list(but_v=3.303,
                                                  dac_vdd=5.249,
                                                  power=battreg_power,
                                                  temp=24.0,
                                                  plotname="mcp3208ic2-nimh-1",
                                                  filename="data/adc-test/putty-adctestsg-v1.1-pipicowmcp3208ic2-nimh33-20260911-i.txt",
                                                  notes="second MCP3208, fresh out of the bag"),

                   "Teensy 4.1 : USB t1"=list(but_v=3.301,
                                              dac_vdd=5.249,
                                              power=usb_laptop_power,
                                              temp=24.6,
                                              plotname="teensy41-usb-1",
                                              filename="data/adc-test/putty-adctestsg-v1.1-teensy41-usb-20260911-i.txt",
                                              notes=""),
                   "Teensy 4.1 : NiMH 5.0 t1"=list(but_v=3.301,
                                                   dac_vdd=5.249,
                                                   power=battreg_power,
                                                   temp=24.3,
                                                   plotname="teensy41-nimh50-1",
                                                   filename="data/adc-test/putty-adctestsg-v1.1-teensy41-nimh50-20260911-i.txt",
                                                   csv_skip=TRUE,
                                                   notes="ground lift",
                                                   csv_notes="unwise ground wiring I"),
                   "Teensy 4.1 : NiMH 5.0 t2"=list(but_v=3.301,
                                                   dac_vdd=5.250,
                                                   power=battreg_power,
                                                   temp=24.3,
                                                   plotname="teensy41-nimh50-2",
                                                   filename="data/adc-test/putty-adctestsg-v1.1-teensy41-nimh50-20260912-ii.txt",
                                                   csv_skip=TRUE,
                                                   notes="ground wire rework I",
                                                   csv_notes="unwise ground wiring II"),
                   "Teensy 4.1 : NiMH 5.0 t3"=list(but_v=3.301,
                                                   dac_vdd=5.250,
                                                   power=battreg_power,
                                                   temp=24.3,
                                                   plotname="teensy41-nimh50-3",
                                                   filename="data/adc-test/putty-adctestsg-v1.1-teensy41-nimh50-20260912-iii.txt",
                                                   notes="ground wire rework II"),

                   "FeatherS2 {CP} : LiPo t1"=list(but_v=3.294,
                                                   dac_vdd=5.251,
                                                   power=lipo_power,
                                                   temp=24.4,
                                                   plotname="feathers2cp-lipo-1",
                                                   filename="data/adc-test/putty-adctestsg-v1.1-feathers2cp-lipo-20260915-i.txt",
                                                   notes="one-off CircuitPython test"),               

                   "UNO R4 Minima : USB t2"=list(but_v=4.678,
                                                 dac_vdd=5.251,
                                                 power=usb_laptop_power,
                                                 temp=24.0,
                                                 plotname="unor4minima-usb-2",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-unor4minima-usb-20260920-ii.txt",
                                                 notes="repeat after a first test showed low code range"),  
                   "UNO R4 Minima : USB t3"=list(but_v=4.678,
                                                 dac_vdd=5.250,
                                                 power=usb_laptop_power,
                                                 temp=23.9,
                                                 plotname="unor4minima-usb-3",
                                                 filename="data/adc-test/putty-adctestsg-v1.1-unor4minima-usb-20260920-iii.txt",
                                                 notes="repeat with extended (10k) noise samples",
                                                 csv_notes="10k noise samples")

                    )


extra_data_100k <- list(
  ### Two special runs with 100k noise samples to look at ESP32-S2
  "FeatherS2 {CP} : LiPo 100k t1"=list(but_v=3.294,
                                      dac_vdd=5.251,
                                      power=lipo_power,
                                      temp=23.1,
                                      plotname="feathers2cp-lipo-100k-1",
                                      filename="data/adc-test/putty-adctestsg-v1.1-feathers2cp-lipo-20260924-ii.txt",
                                      notes="Investigating any noise differences between Arduino and CircuitPython with 100k samples",
                                      csv_notes="100k noise samples"),
  "FeatherS2 {ARD} : LiPo 100k t1"=list(but_v=3.294,
                                       dac_vdd=5.250,
                                       power=lipo_power,
                                       temp=23.6,
                                       plotname="feathers2raw-lipo-100k-1",
                                       filename="data/adc-test/putty-adctestsg-v1.1-feathers2raw-lipo-20260924-iii.txt",
                                       notes="Investigating any noise differences between Arduino and CircuitPython with 100k samples",
                                       csv_notes="100k noise samples")
)


extra_data <- extra_data_100k


test_list_names <- names(extra_data)

### Final run subset which haven't been plotted
#test_list_names <- names(extra_data[15:24])
#test_list_names <- names(extra_data[25:29])
#test_list_names <- c("Xiao ESP32C5 {NOCAL} : NiMH t1")
#test_list_names <- c("ESP32 DevKitC Clone : NiMH t1")
#test_list_names <- c("Tiny 2350 {A4} : NiMH t1")
#test_list_names <- names(extra_data[30:length(extra_data)])
#test_list_names <- c("UNO R4 WiFi : NiMH t1")
#test_list_names <- c("UNO R4 WiFi : NiMH t1", "Feather nRF52840 : USB t1")
#test_list_names <- names(extra_data[(length(extra_data)-5):length(extra_data)])
#test_list_names <- C("micro:bit V2 : 3Alk t1")
#test_list_names <- c("Feather nRF52840 : LiPo t2")
#test_list_names <- c("MCP3208 (IC2) : NiMH t1")
#test_list_names <- names(extra_data[(length(extra_data)-6):length(extra_data)])


#test_list_names <- c("Teensy 4.1 : NiMH 5.0 t3",
#                     "Feather nRF52840 : LiPo t2",
#                     "MCP3208 (IC2) : NiMH t1")
#test_list_names <- c("Teensy 4.1 : NiMH 5.0 t3")
#test_list_names <- c("FeatherS2 {CP} : LiPo t1", "Feather nRF52840 : LiPo t1")

# test_list_names <- names(extra_data[(length(extra_data)-4):length(extra_data)])
# test_list_names <- c("Pi Pico : NiMH 3.3V t1",
#                      "Meowbit : LiPo t1", 
#                      "UNO R4 WiFi : USB t1",
#                      "FeatherS2 {CP} : LiPo t1",
#                      "Feather nRF52840 : LiPo t1",
#                      "MCP3208 (IC2) : NiMH t1",
#                      "Leonardo : NiMH t1",
#                      "CPX (K1) : USB t1"
#                      )

#test_list_names <- c("Feather nRF52840 : LiPo t1",
#                     "Pi Pico : NiMH 3.3V t1")

#test_list_names <- c("ESP32 DevKitC Clone {NOCAL} : NiMH t1",
#                     "UNO R4 Minima : USB t1",
#                     "Pi Pico : NiMH 3.3V t1")


### For testing new alpha / point size 
# test_list_names <- c("ESP32 DevKitC Clone : NiMH t1",
#                      "ESP32 DevKitC Clone {NOCAL} : NiMH t1",
#                      "Leonardo : NiMH t1",
#                      "Feather nRF52840 : LiPo t1",
#                      "MCP3208 (IC2) : NiMH t1",
#                      "UNO R4 WiFi : NiMH t1"
#                      )

#test_list_names <- names(extra_data[1:6])

#test_list_names <- c("Leonardo : USB t1")

# test_list_names <- c("UNO R4 Minima : USB t0",
#                      "UNO R4 Minima : USB t1",
#                      "UNO R4 Minima : USB t2",
#                      "UNO R4 Minima : USB t3",
#                      "UNO R4 Minima : NiMH t1",
#                      "UNO R4 Minima : NiMH 5.0 t1")

#test_list_names <- names(extra_data[(length(extra_data)-1):length(extra_data)])

#test_list_names <- c("FeatherS2 : LiPo t1", "FeatherS2 {NOCAL} : LiPo t1", "FeatherS2 {CP} : LiPo t1")
#test_list_names <- c("Pi Pico : USB t0")

#test_list_names <- c("ESP32 DevKitC Clone {NOCAL} : NiMH t1", "FeatherS2 {NOCAL} : LiPo t1", "Xiao ESP32C6 {NOCAL} : NiMH t1", "Xiao ESP32C5 {NOCAL} : NiMH t1",
#                     "ESP32 DevKitC Clone : NiMH t1",         "FeatherS2 : LiPo t1",         "Xiao ESP32C6 : NiMH t1",         "Xiao ESP32C5 : NiMH t1")

#test_list_names <- c("Leonardo : NiMH t1", "UNO R4 Minima : NiMH t1", "ESP32 DevKitC Clone {NOCAL} : NiMH t1")
#
#test_list_names <- c("Xiao ESP32C6 : USB t0", "Xiao ESP32C5 : USB t0",
#                     "FeatherS2 : LiPo t1")

### do a sample of the boards
#test_list_names <- test_list_names[seq(1,length(test_list_names), by=3)]
#test_list_names <- c("Teensy 4.1 : NiMH 5.0 t3", "Teensy 4.1 : NiMH 5.0 t1", "Teensy 4.1 : NiMH 5.0 t2")



adc_data <- list()
for (name in test_list_names) {
  print(name)
  e_d <- extra_data[[name]]
  adc_data[[name]] <- read_data_file(e_d[["filename"]],
                                     e_d[["but_v"]],
                                     e_d[["dac_vdd"]])
}


### Hack until I understand the whole dpi image size and why png resolution
### cannot be changed without side effects
scale_chart <- 1
font_sf <- scale_chart
title_size <- 42 * font_sf
custom_theme <- theme_light(base_size=28 * font_sf) +
  theme(### element_markdown allows playing around with font/sizes/colours
        plot.title=ggtext::element_markdown(hjust=0.5, size=title_size),
        plot.subtitle=ggtext::element_markdown(hjust=0.5, size=title_size * 0.75 * 0.85),
        ##plot.title=element_text(hjust=0.5, size=title_size),
        ##plot.subtitle=element_text(hjust=0.5, size=title_size * 0.75 * 0.85),
        plot.title.position='plot', 
        panel.grid.major=element_line(color="grey65", linewidth=1.25),
        panel.grid.minor=element_line(color="grey65"),
        ##axis.text.x=element_text(family="mono", face="bold"),
        ##axis.text.y=element_text(family="mono", face="bold"),
        ##axis.ticks.x.top=element_line(color="grey30", size=2),
        ##legend.key.width=unit(3.5, "line"),
        ##legend.key.spacing.x=unit(3, "char"),
        legend.key.spacing.y=unit(1.5, "char"),
        legend.position="right",
        ##legend.margin=margin(t=0, unit='cm'),
        ##legend.justification = c(0, 1),
        ##legend.position = c(0.06, 0.96),  ### upper middle right
        ##legend.background = element_rect(fill="white", color="black"),
        #legend.margin=margin(t=0.25, b=-1.5, unit="char"),
        #strip.text.y=element_text(angle=90),
        panel.spacing=unit(2.5, "line"))


### R "reminder" - can't use levels to extract dac_chans here because
### a factor can have levels when there are no values present in data
extract_channels <- function(df) {
  chans <- gsub(";", "", sort(unique(df$dac_chans)))
  ordered_chans <- c()
  for (chan_len in seq(min(str_length(chans)), max(str_length(chans)))) {
    ordered_chans <- c(ordered_chans,
                       chans[str_length(chans) == chan_len])
  }
  return(paste("output", paste(ordered_chans, collapse=":")))
}


vref_map = c("2048"="2.048V",
             "4096"="4.096V",
             "max"="Vdd")



process_and_plot <- function(board_data_df) {
  dac_data_df <-   subset(board_data_df$expanded_data_df, test_name %in% c("dac"))
  tri_data_df <-   subset(board_data_df$expanded_data_df, test_name %in% c("triangle"))
  trigaps_data_df <- subset(board_data_df$expanded_data_df, test_name %in% c("triangle", "gaps"))
  most_data_df <-  subset(board_data_df$expanded_data_df, test_name %in% c("triangle", "gaps", "verylow"))
  noise_data_df <- subset(board_data_df$expanded_data_df, test_name %in% c("noise"))
  
  sample_count_summary_df <- board_data_df$expanded_data_df %>%
    filter(test_name %in% c("triangle", "gaps", "verylow", "noise")) %>%
    group_by(test_name, dac_chans, adc_chan, adc_mode, dac_vref_mode,    dac_codes) %>%
    summarise(sample_count=length(but_adc_code)) %>%
    group_by(test_name) %>%
    summarise(med_sample_count=median(sample_count)) %>%
    ungroup()
  sample_count_summary_text <- paste(mapply(function(key,val) { return(paste0(key, ":", val)) },
                                            sample_count_summary_df$test_name, sample_count_summary_df$med_sample_count),
                                     collapse=" ")
  
  but_data <- board_data_df$but_data
  info <- extra_data[[name]]                  
  ### Let's see if the manufacturer's name fits
  board_manu = but_data[["board_manu"]]
  board_name = but_data[["board_name"]]
  if (grepl("clone", name, ignore.case=TRUE)) {
    board_manu <- "Unbranded clone"
  }
  if (grepl("ESP32C", board_name, ignore.case=TRUE)
      & !grepl("XIAO", board_name, ignore.case=TRUE)) {
    ### Hack to put Xiao name on the relevant boards
    board_name <- paste("XIAO", board_name)
  } else if (grepl("^micro:?bit$", board_name, ignore.case=TRUE)) {
    board_name <- "micro:bit v1"
  }
  plot_board_name <- sprintf("%s %s", board_manu, board_name)
  
  ### Look for an instance name in 
  board_instance <- str_extract(name, "\\((.+)\\)", group=TRUE)
  if (is.na(board_instance)) {
    board_instance <- ""
  } else {
    plot_board_name <- paste(plot_board_name, paste0("[", board_instance, "]"))
  }
  ### plot_board_name is further modified later based on but_read
  
  csv_skip <- ifelse(is.null(info$csv_skip), FALSE, info$csv_skip)
  csv_notes <- ifelse(is.null(info$csv_notes), "", info$csv_notes)
  
  power_source <- info$power
  but_v <- info[["but_v"]]
  but_ref_v <- but_v   ### this is a bit more complex for ADCs not using Vdd as reference
  but_nominal_v <- as.numeric(but_data[["aref"]])
  but_adc_bits <- as.numeric(but_data[["adc_bits"]])
  but_read <- but_data[["read"]]  ### this will be empty string if not present
  badc_codesize <- most_data_df$but_adc_codesize[1]
  but_adc_nominal_lsb_v <- but_nominal_v / badc_codesize
  but_adc_actual_lsb_v <- but_v / badc_codesize
  if (but_read == READ_CALIBRATEDMV) {
    fsr_actual_code <- round(but_v * 1000)
    converted_text <- "rescaled"
    ### Add something short and sweet to indicate the board's output has been calibrated
    ### Got space on the top (title) line
    plot_board_name <- paste(plot_board_name, "(CAL)")
  } else {
    fsr_actual_code <- badc_codesize - 1
    converted_text <- NA
  }
  
  but_language <- but_data["language"]
  if (but_language == "Arduino") {
    adc_read_function_name <- READ_MAP[but_read]
  } else if (but_language == "CircuitPython") {
    adc_read_function_name <- "AnalogIn value"
  } else if (but_language == "MicroPython") {
    adc_read_function_name <- "read_analog()" 
  }  else {
    adc_read_function_name <- "unknown" 
  }
  
  but_sw_ver <- but_data[["software_version"]]
  
  board_mcu <- but_data[["board_mcu"]]
  board_mcu <- sub("^NRF5", "nRF5", board_mcu)
  common_conditions_pre <- c(sprintf("%s, %s, Vdd=%.3f",
                                     board_mcu, but_language, but_v))
  amb_temp <- info[["temp"]]
  common_conditions_post <- c(power_source,
                              sprintf("%s\u00B0C", as.character(info[["temp"]])))
  common_conditions_post_detailed <- c("input A0",
                                       "100k load",
                                       common_conditions_post)
  if (str_length(csv_notes) > 0) {
    extra_text_color <- "blue3"
    extra_text <- paste0("<span style='font-size: 22px; color: ", extra_text_color, "'>(", csv_notes, ")</span>")
    common_conditions_post <- paste(c(common_conditions_post,
                                      extra_text),
                                    collapse=", ")
    common_conditions_post_detailed <- paste(c(common_conditions_post_detailed,
                                               extra_text),
                                               collapse=", ")
  }
  
  dac_chan_list <- extract_channels(dac_data_df)
  tri_chan_list <- extract_channels(tri_data_df)
  most_chan_list <- extract_channels(most_data_df)
  noise_chan_list <- extract_channels(noise_data_df)
  
  plot_lower_v = 0.0
  plot_upper_v = most_data_df$but_adc_ref_nominal_v[1] + 0.1
  
  ### Used for DNL / INL
  upper_noise_lsb <- most_data_df$but_adc_codesize[1] / 50.0
  upper_noise_lsb_zoom <- upper_noise_lsb / 4
  lower_noise_lsb <- 0 - upper_noise_lsb
  lower_noise_lsb_zoom <- lower_noise_lsb / 4
  
  ### Second approach for saturation voltage, look for lowest code with 51% max values
  max_code_vrefmax <- max(subset(tri_data_df, dac_vref_mode == "max")$but_adc_code)
  near_sat_df <- subset(tri_data_df,
                        dac_vref_mode == "max") %>%
    group_by(dac_chans, dac_code_1) %>%
    summarise(mean_v=mean(adc_v),
              frac=sum(but_adc_code == max_code_vrefmax) / length(but_adc_code)) %>%
    filter(frac > 0.51)
  sat_v <- min(near_sat_df$mean_v)
  print(sprintf("saturation voltage %f", sat_v))
  
  near_zero_df <- subset(tri_data_df,
                         adc_v < 0.200) %>%
    arrange(adc_v) %>%
    group_by(adc_v) %>%
    summarise(riqrmean_bac=round(iqrmean(but_adc_code)))
  
  ### Now look for the lowest code voltages and pick the highest one
  lowest_v <- max(0.0,
                  max(subset(near_zero_df, riqrmean_bac == min(riqrmean_bac))$adc_v))
  print(sprintf("lowest mostly reliable measureable voltage %f", lowest_v))  
  
  ### Pick a default nice region as a guess at good linearity
  nice_lower_v <- 0.1           ### below 100mV values will be down weighted
  nice_upper_v <- 0.98 * sat_v  ### ignore anything above this
  
  ### a=0, b=1 gave a "step factor 0.000488281 reduced below 'minFactor' of 0.000976562"
  for (start_parms in list(list(a=0.05, b=1.05),
                           list(a=0.21, b=0.81),
                           list(a=0.17, b=1.3),
                           list(a=-0.0777, b=1.32132),
                           list(a=0.434417, b=0.4398),
                           list(a=0.033333, b=1.03333))) {
    model <- try(nls(but_actual_v~a + b * adc_v,
                     #data=most_data_df %>% subset(adc_v >= nice_lower_v & adc_v < nice_upper_v),
                     data=most_data_df,
                     weights=(most_data_df %>% 
                                mutate(weight=ifelse(adc_v < nice_lower_v, 0.6, ifelse(adc_v < nice_upper_v, 1, 0))))$weight,
                     start=start_parms))
    if (inherits(model, "try-error")) {
      print("trying another set of start parameters for nls()")
    } else {
      break
    }
  }
  
  model_coef <- coef(model)
  adc_cor_fn <- function(x) { (x - model_coef[["a"]]) / model_coef[["b"]] }
  
  actual_step_size_v <-  but_ref_v / badc_codesize
  ys <- seq(0, badc_codesize) * actual_step_size_v
  actual_perfect_adc_steps <- data.frame(x=ys - actual_step_size_v/2.0,
                                         y=ys)
  
  
  tri_data_df <- tri_data_df %>%
    mutate(model1_v=predict(model, newdata=tri_data_df),
           but_corrected_v=adc_cor_fn(but_actual_v),
           bestfit_err_v=but_actual_v - model1_v,
           bestfit_err_lsb=bestfit_err_v / (but_adc_ref_actual_v / badc_codesize))
  
  most_data_df <- most_data_df %>%
    mutate(model1_v=predict(model, newdata=most_data_df),
           but_corrected_v=adc_cor_fn(but_actual_v),
           bestfit_err_v=but_actual_v - model1_v,
           bestfit_err_lsb=bestfit_err_v / (but_adc_ref_actual_v / badc_codesize))
  
  lc_near_sat_df <- subset(most_data_df,
                           dac_vref_mode == "max") %>%
    group_by(but_adc_code) %>%
    summarise(mean_v=mean(but_corrected_v),
              frac=sum(but_adc_code == max_code_vrefmax) / length(but_adc_code)) %>%
    filter(frac > 0.60)
  lc_sat_v <- lc_near_sat_df[lc_near_sat_df$but_adc_code == min(lc_near_sat_df$but_adc_code)]$mean_v
  print(sprintf("lc but saturation voltage %f", lc_sat_v))
  
  lc_step_size_v <-  lc_sat_v / badc_codesize
  ys <- seq(0, badc_codesize) * lc_step_size_v
  lc_perfect_adc_steps <- data.frame(x=ys - lc_step_size_v/2.0,
                                     y=ys)
  
  # ### Assign but_ideal_adc_code to data using but_adc_ref_actual_v and adc_v as input
  # ### limited to 0 to but_adc_codesize - NA outside those values?
  # most_data_df <- most_data_df %>%
  #   mutate(but_adc_ideal_code=as.integer(sapply(round(adc_v / but_adc_ref_actual_v * badc_codesize),
  #                                               function(x) ifelse(x < 0, NA, ifelse(x >= badc_codesize, NA, x)))))
  # 
  # ##bestfit_lsb_size <- model_coef[["b"]]
  # most_dnl_data_df <- most_data_df %>%
  #   subset(!is.na(but_adc_ideal_code)) %>%
  #   select(but_adc_ideal_code, bestfit_err_lsb,
  #          dac_vref_mode, idx) %>%
  #   group_by(but_adc_ideal_code) %>%
  #   arrange(but_adc_ideal_code) %>%
  #   mutate(temp_grp_id=cur_group_id(),
  #          mean_bestfit_err_lsb=mean(bestfit_err_lsb)) %>%
  #   ungroup()
  # 
  # mean_bel <- (most_dnl_data_df %>%
  #              group_by(temp_grp_id) %>%
  #              arrange(temp_grp_id) %>%
  #              summarise(value=mean_bestfit_err_lsb[1]))$value
  # 
  # ### this is slow but it works, probably a better way to refer to previous group
  # most_dnl_data_df <- most_dnl_data_df %>%
  #   group_by(temp_grp_id) %>%
  #   arrange(temp_grp_id) %>%
  #   mutate(diffbestfit_err_lsb=bestfit_err_lsb - ifelse(temp_grp_id == 1, NA, mean_bel[temp_grp_id - 1])) %>%
  #   ungroup() %>%
  #   filter(!is.na(diffbestfit_err_lsb)) %>%
  #   mutate(but_adc_ideal_code = but_adc_ideal_code - 1)
  
  ### Assign but_ideal_adc_code to data using but_adc_ref_actual_v and adc_v as input
  ### limited to 0 to but_adc_codesize - NA outside those values?
  most_data_df <- most_data_df %>%
    mutate(but_adc_ideal_code=as.integer(sapply(round(adc_v / but_ref_v * badc_codesize),
                                                function(x) ifelse(x < 0, NA, ifelse(x >= badc_codesize, NA, x)))))
  
  
  most_dnl_data_df <- most_data_df %>%
    select(but_adc_code, bestfit_err_lsb,
           dac_vref_mode, idx) %>%
    group_by(but_adc_code) %>%
    arrange(but_adc_code) %>%
    mutate(temp_grp_id=cur_group_id(),
           mean_bestfit_err_lsb=mean(bestfit_err_lsb)) %>%
    ungroup()
  
  mean_bel <- (most_dnl_data_df %>%
                 group_by(temp_grp_id) %>%
                 arrange(temp_grp_id) %>%
                 summarise(value=mean_bestfit_err_lsb[1]))$value
  
  ### this is slow but it works, probably a better way to refer to previous group
  most_dnl_data_df <- most_dnl_data_df %>%
    group_by(temp_grp_id) %>%
    arrange(temp_grp_id) %>%
    mutate(diffbestfit_err_lsb=bestfit_err_lsb - ifelse(temp_grp_id == 1, NA, mean_bel[temp_grp_id - 1])) %>%
    ungroup() %>%
    filter(!is.na(diffbestfit_err_lsb)) %>%
    mutate(but_adc_code = but_adc_code - 1)
  
  
  ### sat_v is used here to be kind to the ADCs which cannot measure above
  ### a certain voltage below the ADC reference voltage for whatever reason
  
  max_good_v <- min(but_ref_v, sat_v)
  stats_df <- most_data_df %>%
    filter(between(adc_v, lowest_v, max_good_v)) %>%
    summarise(nd_mes_v=mean((but_corrected_v - adc_v)**2))
  most_nd_rms_v <- stats_df$nd_mes_v**0.5
  
  stats_df <- most_data_df %>%
    filter(between(adc_v, lowest_v, max_good_v)) %>%
    group_by(adc_v) %>%
    mutate(n_es_v_per_adc=(but_corrected_v - mean(but_corrected_v))**2) %>%
    ungroup() %>%
    summarise(n_mes_v=mean(n_es_v_per_adc))
  most_n_rms_v <- stats_df$n_mes_v**0.5
  
  stats_df <- most_data_df %>%
    filter(between(adc_v, lowest_v, max_good_v)) %>%
    group_by(adc_v) %>%
    summarise(d_es_v_per_adc=(mean(but_corrected_v) - adc_v[1])**2) %>%
    ungroup() %>%
    summarise(d_mes_v=mean(d_es_v_per_adc))
  most_d_rms_v <- stats_df$d_mes_v**0.5
  
  
  most_enob <- log(1 / 12**0.5 / (most_nd_rms_v / but_ref_v), 2)
  most_n_bits <- max(0.0, but_adc_bits - log(1 / 12**0.5 / (most_n_rms_v / but_ref_v), 2))
  most_d_bits <- max(0.0, but_adc_bits - most_enob - most_n_bits)
  most_wc_enob <- min(most_enob, but_adc_bits - most_n_bits - most_d_bits)
  most_sinad_db <- 20 * log(2, 10) * most_enob + 10 * log(1.5, 10)
  print(sprintf("ENOB (most tests) %.2fbits, noise=%.2fbits (%.2fmV), distortion=%.2fbits (%.2fmV)",
                most_enob, most_n_bits, most_n_rms_v * 1e3, most_d_bits, most_d_rms_v * 1e3))
  
  ac_breaks <- round(seq(0, badc_codesize - 1, length.out=42))
  ac_breaks[1] <- ac_breaks[1] - 1
  ac_labels <- head(paste0(ac_breaks + 1, "-", lead(ac_breaks)), length(ac_breaks) - 1)
  lsb_breaks <- seq(-200, 200, by=10) / 10.0
  
  ### The frac(tion) is the fraction per group of adc codes, i.e.
  ### if there were five groups each group would add up to 0.2 and total would be 1.0
  tri_dnl_hmp_data_df <- most_dnl_data_df %>%
    drop_na(diffbestfit_err_lsb) %>%
    mutate(ac_group=cut(but_adc_code, breaks=ac_breaks, labels=ac_labels),
           lsb_group=cut(diffbestfit_err_lsb, breaks=lsb_breaks)) %>%
    group_by(ac_group, lsb_group) %>%
    summarise(count=length(diffbestfit_err_lsb)) %>%
    ungroup() %>%
    complete(ac_group, lsb_group, fill=list(count=0)) %>%
    group_by(ac_group) %>%
    mutate(frac=count / sum(count) / (length(ac_breaks) - 1)) %>%
    ungroup() %>%
    subset(!is.na(lsb_group))   ### need to remove NA stuff which must be the out of range values
  
  ### How to do runs?
  ### https://stackoverflow.com/questions/16911773/collapse-runs-of-consecutive-numbers-to-ranges
  ### https://stackoverflow.com/questions/14868406/collapse-continuous-integer-runs-to-strings-of-ranges/14868742#14868742
  but_adc_code_count <- table(c(most_data_df$but_adc_unproc_code, noise_data_df$but_adc_unproc_code))
  absent_codes <- setdiff(seq(0, fsr_actual_code), as.numeric(names(but_adc_code_count)))
  split_each <- function(x, n) { return(split(x, ceiling(seq_along(x) / n))) }
  range_str <- function(x) { st_x <- x[1] ;
  en_x <- tail(x, 1);
  return(ifelse(st_x == en_x, as.character(st_x), paste(st_x, en_x, sep="-") )) }
  absent_codes_summary1 <- sapply(split(absent_codes,
                                        cumsum(c(1, diff(absent_codes) - 1))),
                                  range_str)
  absent_codes_summary1_txt <- paste(absent_codes_summary1,collapse=", ")
  
  but_min_code <- min(as.numeric(names(but_adc_code_count)))
  but_max_code <- max(as.numeric(names(but_adc_code_count)))
  but_absent_code_count <- length(absent_codes)
  but_absent_codegroup_count <- ifelse(but_absent_code_count == 0, 0, length(absent_codes_summary1))
  
  ### TODO - this doesn't do what I wanted
  absent_codes_chunked <- split_each(absent_codes, 5)
  absent_codes_summary2 <- paste(lapply(split_each(absent_codes, 5),
                                        function(x) { return(paste(sapply(split(x, cumsum(c(1,diff(x)-1))),
                                                                          range_str),collapse=", ")) }), collapse="\n")
  ### remember but_adc_code here can have a fractional value due to
  ### how it's converted back to a pseudo-raw value for ESP32 series
  code_coverage_df <- most_data_df %>%
    group_by(test_name) %>%
    reframe(bac_count=table(factor(round(but_adc_code), levels=seq(0, badc_codesize - 1))),
            bac_unproc_count=table(factor(but_adc_unproc_code, levels=seq(0, badc_codesize - 1))))
  
  noise_data_df <- noise_data_df %>%
    mutate(but_corrected_v=adc_cor_fn(but_actual_v)) %>%
    group_by(vref, dac_code_1) %>%
    mutate(mean_adc_v=mean(adc_v),
           mean_but_actual_v=mean(but_actual_v),
           mean_but_nominal_v=mean(but_nominal_v),
           mean_but_corrected_v=mean(but_corrected_v),
           iqrmean_but_corrected_v=iqrmean((but_corrected_v))) %>%
    ungroup() %>% arrange(mean_adc_v) %>%
    group_by(mean_adc_v) %>%
    arrange(mean_adc_v) %>%
    mutate(v_idx=as.factor(cur_group_id()),
           mean_adc_v_fac=as.factor(round(mean_adc_v,3))) %>%
    ungroup()
  
  max_good_v <- min(but_ref_v, sat_v)
  stats_df <- noise_data_df %>%
    filter(between(adc_v, lowest_v, max_good_v)) %>%
    summarise(nd_mes_v=mean((but_corrected_v - adc_v)**2))
  noise_nd_rms_v <- stats_df$nd_mes_v**0.5
  
  stats_df <- noise_data_df %>%
    filter(between(adc_v, lowest_v, max_good_v)) %>%
    group_by(adc_v) %>%
    mutate(n_es_v_per_adc=(but_corrected_v - mean(but_corrected_v))**2) %>%
    ungroup() %>%
    summarise(n_mes_v=mean(n_es_v_per_adc))
  noise_n_rms_v <- stats_df$n_mes_v**0.5
  
  stats_df <- noise_data_df %>%
    filter(between(adc_v, lowest_v, max_good_v)) %>%
    group_by(adc_v) %>%
    summarise(d_es_v_per_adc=(mean(but_corrected_v) - adc_v[1])**2) %>%
    ungroup() %>%
    summarise(d_mes_v=mean(d_es_v_per_adc))
  noise_d_rms_v <- stats_df$d_mes_v**0.5
  
  noise_enob <- log(1 / 12**0.5 / (noise_nd_rms_v / but_ref_v), 2)
  noise_n_bits <- max(0.0, but_adc_bits - log(1 / 12**0.5 / (noise_n_rms_v / but_ref_v), 2))
  noise_d_bits <- max(0.0, but_adc_bits - noise_enob - noise_n_bits)
  noise_enob <- min(noise_enob, but_adc_bits - noise_n_bits - noise_d_bits)
  noise_sinad_db <- 20 * log(2, 10) * noise_enob + 10 * log(1.5, 10)
  print(sprintf("ENOB (noise tests) %.2fbits, noise=%.2fbits (%.2fmV), distortion=%.2fbits (%.2fmV)",
                noise_enob, noise_n_bits, noise_n_rms_v * 1e3, noise_d_bits, noise_d_rms_v * 1e3))
  
  combo_enob <- but_adc_bits - most_d_bits - max(noise_n_bits, most_n_bits)
  
  if (dim(noise_data_df)[1] > 0) {    
    noise_stats_df <- noise_data_df %>%
      group_by(v_idx, mean_adc_v, mean_adc_v_fac,
               mean_but_nominal_v, mean_but_actual_v, mean_but_corrected_v, iqrmean_but_corrected_v) %>%
      summarise(sd_actual_v=sd(but_actual_v),
                sd_nominal_v=sd(but_nominal_v),
                sd_corrected_v=sd(but_corrected_v),
                
                actual_range_v=max(but_actual_v) - min(but_actual_v),
                nominal_range_v=max(but_nominal_v) - min(but_nominal_v),
                corrected_range_v=max(but_corrected_v) - min(but_corrected_v),
                actual_range90_v=quantile(but_actual_v, 0.95) - quantile(but_actual_v, 0.05),
                nominal_range90_v=quantile(but_nominal_v, 0.95) - quantile(but_nominal_v, 0.05),
                corrected_range90_v=quantile(but_corrected_v, 0.95) - quantile(but_corrected_v, 0.05),
                actual_iqr_v=quantile(but_actual_v, 0.75) - quantile(but_actual_v, 0.25),
                nominal_iqr_v=quantile(but_nominal_v, 0.75) - quantile(but_nominal_v, 0.25),
                corrected_iqr_v=quantile(but_corrected_v, 0.75) - quantile(but_corrected_v, 0.25),
                
                actual_noise_rms_v=(sum((but_actual_v - mean_but_actual_v)**2) / length(but_actual_v))**0.5,
                nominal_noise_rms_v=(sum((but_actual_v - mean_but_actual_v)**2) / length(but_actual_v))**0.5,
                corrected_noise_rms_v=(sum((but_corrected_v - mean_but_corrected_v)**2) / length(but_corrected_v))**0.5,
                
                sd_lsb=sd(but_adc_code),
                range_lsb=max(but_adc_code) - min(but_adc_code),
                range90_lsb=quantile(but_adc_code, 0.95) - quantile(but_adc_code, 0.05),
                
                mean_actual_offset_v=mean_but_actual_v[1] - mean_adc_v[1],
                mean_nominal_offset_v=mean_but_nominal_v[1] - mean_adc_v[1],
                mean_corrected_offset_v=mean_but_corrected_v[1] - mean_adc_v[1],
                iqrmean_corrected_offset_v=iqrmean_but_corrected_v[1] - mean_adc_v[1],
                
                #summary1_txt=sprintf("rms noise=%.2fmV, sd=%.2fmV, p90range=%.2fmV, ",
                #                     actual_noise_rms_v * 1000.0, sd_actual_v * 1000.0, actual_range90_v * 1000.0)
                ### RMS is defined same as sd (population)
                summary_ac1_txt=sprintf(paste("sd (rms noise)=%.2f",
                                              "IQR=%.2f, p90r=%.2f, r=%.2f",
                                              sep="\n"),
                                        sd_actual_v * 1000.0,
                                        actual_iqr_v * 1000.0,
                                        actual_range90_v * 1000.0,
                                        actual_range_v * 1000.0),
                summary_co1_txt=sprintf(paste("sd (rms noise)=%.2f",
                                              "IQR=%.2f, p90r=%.2f, r=%.2f",
                                              sep="\n"),
                                        sd_corrected_v * 1000.0,
                                        corrected_iqr_v * 1000.0,
                                        corrected_range90_v * 1000.0,
                                        corrected_range_v * 1000.0),
                
                summary_co2_txt=sprintf(paste("uncorr. voltage mean=%.2f, IQRmean=%.2f",
                                              "cor. offsets, mean=%.2f, IQRmean=%.2f",
                                              sep="\n"),
                                        mean_but_actual_v[1] * 1000.0,
                                        iqrmean(mean_but_actual_v) * 1000.0,
                                        mean_corrected_offset_v * 1000.0,
                                        iqrmean_corrected_offset_v * 1000.0)
                
      ) %>% ungroup()
  }
  
  ### For sd/rms values in mV
  ### Not used, used scale_fill_gradientn() in the end
  ##rag20 <- colorRampPalette(c("green2", "yellow2", "orange2", "red2", "red3"))(20)
  
  three_voltages <- c(sat_v, but_v, but_nominal_v)
  ### colours aren't really working 
  #three_colours <- c("black", "blue", "yellow")
  three_colours <- c("black", "black", "black")
  abovevref_top_df <- data.frame(xmin=c(-Inf),
                                 xmax=c(Inf),
                                 ymin=three_voltages,
                                 ymax=c(Inf),
                                 fill=three_colours,
                                 alpha=0.10)
  abovevref_corner_df <- data.frame(xmin=as.vector(sapply(three_voltages, function(x) { return (c(x, -Inf))})),
                                    xmax=as.vector(sapply(three_voltages, function(x) { return (c(Inf, Inf))})),
                                    ymin=as.vector(sapply(three_voltages, function(x) { return (c(-Inf, x))})),
                                    ymax=as.vector(sapply(three_voltages, function(x) { return (c(x, Inf))})),
                                    fill=rep(three_colours, each=2),
                                    alpha=0.10)
  
  plot_name <- info[["plotname"]]
  
  g0 <- ggplot(dac_data_df %>% arrange(idx),
               aes(x=dac_code_1 / dac_code_num * as.numeric(as.character(vref)),
                   y=adc_v, 
                   color=dac_chans, shape=vref_map[dac_vref_mode])) +
    custom_theme + 
    ggtitle(sprintf("%s - MCP4728 DAC check", plot_board_name),
            subtitle=paste(c(common_conditions_pre,
                             dac_chan_list,
                             common_conditions_post_detailed),
                           collapse=", ")) + 
    geom_rect(data=abovevref_corner_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=as.character(fill), alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    geom_point(size=1,
               alpha=0.5) +
    labs(x="ideal DAC (v)", y="ADS1115 measurement (V)") +
    scale_color_discrete(name="DAC\nchan") +
    scale_shape_discrete(name="DAC\nVref") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1))) +
    coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                    ylim=c(plot_lower_v, plot_upper_v))
  
  
  chart_filename <- sprintf("%s-%s-g0", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g0,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  
  
  
  g1a <- ggplot(tri_data_df %>% arrange(idx),
                aes(x=adc_v,
                    y=but_nominal_v, 
                    color=vref_map[dac_vref_mode], shape=adc_mode)) +
    custom_theme + 
    ggtitle(paste(plot_board_name),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post_detailed),
                           collapse=", ")) +
    geom_rect(data=abovevref_corner_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=fill, alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    geom_point(size=1,
               alpha=0.5) +
    labs(x="ADS1115 measurement (V)", y="BUT ADC nominal (V)") +
    annotate("text", parse=TRUE,
             size=9,
             x=plot_upper_v*0.01, y=plot_upper_v*0.82,
             hjust=0,
             label=sprintf('nominal==frac(ADC_code, %d) %%*%% %s',
                           badc_codesize,
                           as.character(but_nominal_v))) +
    scale_color_discrete(name="DAC\nVref") +
    scale_shape_discrete(name="ADS1115\nmode") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1))) +
    coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                    ylim=c(plot_lower_v, plot_upper_v))
  
  chart_filename <- sprintf("%s-%s-g1a", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g1a,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  g1b <- ggplot(tri_data_df %>% arrange(idx),
                aes(x=adc_v,
                    y=but_actual_v, 
                    color=vref_map[dac_vref_mode], shape=adc_mode)) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "scaled by Vdd"),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post_detailed),
                           collapse=", ")) +
    geom_rect(data=abovevref_corner_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=fill, alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    geom_point(size=1,
               alpha=0.5) +
    geom_line(aes(x=adc_v, y=model1_v),
              color="grey20") + 
    labs(x="ADS1115 measurement (V)", y="BUT ADC actual (V)") +
    annotate("text", parse=TRUE,
             size=9,
             x=plot_upper_v*0.01, y=plot_upper_v*0.82,
             hjust=0,
             label=sprintf('actual==frac(ADC_code, %d) %%*%% %.3f',
                           badc_codesize,
                           but_v)) +
    scale_color_discrete(name="DAC\nVref") +
    scale_shape_discrete(name="ADS1115\nmode") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1))) +
    coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                    ylim=c(plot_lower_v, plot_upper_v))
  
  chart_filename <- sprintf("%s-%s-g1b", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g1b,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  
  g2 <- ggplot(tri_data_df %>% arrange(reltime_s),
               aes(x=reltime_s,
                   y=but_actual_v,
                   color=vref_map[dac_vref_mode], shape=adc_mode)) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "triangle waveform tests"),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    geom_rect(data=abovevref_top_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=fill, alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    geom_point(size=1,
               alpha=0.5) +
    scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
    labs(x="time (s)", y="BUT ADC actual (V)") +
    scale_color_discrete(name="DAC\nVref") +
    scale_shape_discrete(name="ADS1115\nmode") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1))) +
    coord_cartesian(ylim=c(plot_lower_v, plot_upper_v))
  
  chart_filename <- sprintf("%s-%s-g2", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g2,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  ### these may be a tiny bit too wide due to noise depending on
  ### one's point of view
  grey_bands_df <- tri_data_df %>%
    group_by(dac_vref_mode) %>%
    arrange(reltime_s) %>%
    filter(adc_v > but_v) %>%
    summarise(xmin=min(reltime_s),
              xmax=max(reltime_s),
              ymin=-Inf,
              ymax=Inf)
  
  facet_names = vref_map[levels(tri_data_df$dac_vref_mode)]
  names(facet_names) <- facet_names
  facet_names = c(facet_names,
                  "C"="ADS1115 continuous sampling (C)",
                  "S"="ADS1115 single-shot sampling (S)")
  g3a <- ggplot(tri_data_df %>% arrange(reltime_s),
                aes(x=reltime_s, y=(but_actual_v-adc_v) * 1000.0,
                    color=vref_map[dac_vref_mode])) +
    custom_theme + 
    ggtitle(paste(plot_board_name,
                  "difference to ADS1115"),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    geom_rect(data=grey_bands_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax),
              fill="black",
              alpha=0.2,
              color=NA,
              inherit.aes=FALSE) +
    geom_point(size=1,
               show.legend=FALSE) +
    scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
    labs(x="time", y="BUT ADC actual difference (mV)") +
    scale_color_discrete(name="DAC\nVref") +
    ##scale_shape_discrete(name="ADS1115\nmode") +
    guides(color=guide_legend(override.aes=list(size=5)),
           shape=guide_legend(override.aes=list(size=5))) +
    facet_wrap(vars(vref_map[dac_vref_mode]),
               ncol=1,
               dir="v",
               strip.position="right")
  
  chart_filename <- sprintf("%s-%s-g3a", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g3a,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  ### these may be a tiny bit too wide due to noise depending on
  ### one's point of view
  grey_bands_am_df <- tri_data_df %>%
    group_by(dac_vref_mode, adc_mode) %>%
    arrange(reltime_s) %>%
    filter(adc_v > but_v) %>%
    summarise(xmin=min(reltime_s),
              xmax=max(reltime_s),
              ymin=-Inf,
              ymax=Inf)
  
  g3b <- ggplot(tri_data_df %>% arrange(reltime_s),
                aes(x=reltime_s, y=(but_actual_v-adc_v) * 1000.0,
                    #shape=adc_mode,
                    color=vref_map[dac_vref_mode])) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "difference to ADS1115"),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    geom_rect(data=grey_bands_am_df,
              aes(xmin=xmin, ymin=ymin,
                  xmax=xmax, ymax=ymax),
              fill="black",
              alpha=0.2,
              color=NA,
              inherit.aes=FALSE) +
    geom_point(size=1,
               show.legend=FALSE) +
    scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
    labs(x="time", y="BUT ADC actual difference (mV)") +
    #scale_color_discrete(name="ADS1115\nmode") +
    #scale_shape_discrete(name="DAC\nVref") +
    guides(color=guide_legend(override.aes=list(size=5)),
           shape=guide_legend(override.aes=list(size=5))) +
    facet_grid(vref_map[dac_vref_mode] ~ adc_mode,
               labeller=as_labeller(facet_names))
  
  chart_filename <- sprintf("%s-%s-g3b", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g3b,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  
  
  ### Visual checking if the direction is influencing the ADC
  ### Will also pick up on any other subtle ADC issues like drops in Vdd
  ### due to MCU battery discharge
  range_df <- tri_data_df %>% 
    filter(adc_v < sat_v) %>%
    mutate(diff_mv=(but_actual_v-adc_v) * 1000.0) %>% 
    summarise(lo=quantile(diff_mv, 0.1),
              mid=quantile(diff_mv, 0.5),
              hi=quantile(diff_mv, 0.9))
  range_mv <- max(50, (range_df$hi - range_df$lo) * 1.5)
  dir_check_lower_v <- range_df$mid - range_mv * 0.5
  dir_check_upper_v <- range_df$mid + range_mv * 0.5
  
  facet_names = vref_map[levels(tri_data_df$dac_vref_mode)]
  names(facet_names) <- facet_names
  facet_names = c(facet_names,
                  "A"="DAC channel A",
                  "B"="DAC channel B",
                  "C"="DAC channel C",
                  "D"="DAC channel D")
  g3c <- ggplot(tri_data_df %>% arrange(adc_v),
                aes(x=adc_v, y=(but_actual_v-adc_v) * 1000.0,
                    shape=adc_mode,
                    color=direction)) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "rise/fall difference to ADS1115"),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    geom_rect(data=data.frame(xmin=but_v, xmax=Inf,
                              ymin=-Inf, ymax=Inf),
              aes(xmin=xmin, ymin=ymin,
                  xmax=xmax, ymax=ymax),
              fill="black",
              alpha=0.2,
              color=NA,
              inherit.aes=FALSE) +
    geom_point(size=0.5 * (4096.0 / badc_codesize)**0.8,
               alpha=0.3) +
    labs(x="ADS1115 measurement (V)", y="BUT ADC actual difference (mV)") +
    scale_shape_discrete(name="ADS1115\nmode") +
    #scale_color_discrete(name="ADS1115\nmode") +
    #scale_shape_discrete(name="DAC\nVref") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1))) +
    coord_cartesian(xlim=c(0, but_v),
                    ylim=c(dir_check_lower_v, dir_check_upper_v)) + 
    facet_grid(vref_map[dac_vref_mode] ~ dac_chans,
               labeller=as_labeller(facet_names))
  #facet_wrap(vars(vref_map[dac_vref_mode]),
  #           ncol=1,
  #           dir="v",
  #           strip.position="right")
  
  chart_filename <- sprintf("%s-%s-g3c", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g3c,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  
  
  
  ### INL, DNL time
  subtitle <- paste(c(common_conditions_pre,
                      most_chan_list,
                      common_conditions_post),
                    collapse=", ")
  
  g4inla <- ggplot(most_data_df %>% arrange(idx, dac_vref_mode),
                   mapping=aes(x=but_adc_code,
                               y=bestfit_err_lsb,
                               color=vref_map[dac_vref_mode])) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "best fit integral nonlinearity (INL)"),
            subtitle=subtitle) +
    geom_point(size=1,
               alpha=0.25,
               stroke=0) +
    #labs(x="ADS1115 measurement (V)", y="BUT ADC code difference (LSB)") +
    scale_x_continuous(breaks=seq(0,badc_codesize,by=badc_codesize %/% 16),
                       minor_breaks=seq(0,badc_codesize,by=badc_codesize %/% 32)) +
    scale_color_discrete(name="DAC\nVref") +
    labs(x="BUT ADC code", y="BUT ADC corrected difference (LSB)") +
    ## scale_y_continuous(breaks=seq(-10, 10, by=5), minor_breaks=seq(-10, 10, by=1)) +
    #test#scale_color_brewer(name="DAC\nVref", palette="Set1") +
    guides(color=guide_legend(override.aes=list(size=8, alpha=1))) +
    coord_cartesian(ylim=c(lower_noise_lsb, upper_noise_lsb))
  
  chart_filename <- sprintf("%s-%s-g4-inla", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g4inla,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  ### IQRmean with background of other data
  g4inlb <- ggplot(most_data_df %>% arrange(idx, dac_vref_mode),
                   mapping=aes(x=but_adc_code,
                               y=bestfit_err_lsb)) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "best fit integral nonlinearity (INL)"),
            subtitle=subtitle) +
    geom_point(aes(color=vref_map[dac_vref_mode]),
               stroke=0,
               size=upper_noise_lsb / upper_noise_lsb_zoom,
               ##alpha=400 / max(na.omit(most_data_df$but_adc_code)),
               alpha=(3 / max(na.omit(most_data_df$but_adc_code)))**0.7) +
    stat_summary(orientation="x",
                 fun=iqrmean,
                 geom="line",
                 size=1,
                 show.legend=FALSE) +
    geom_label(data=data.frame(x=badc_codesize / 2, y=lower_noise_lsb_zoom,
                               text="noise should not be inferred from the black line (IQRmean) due to fewer samples above 2.048V and 4.096V"),
               mapping=aes(x=x, y=y, label=text),
               hjust=0.5, vjust=0.5,
               size=6,                
               family="mono", fontface="bold",
               fill="pink",
               alpha=0.7,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    scale_x_continuous(breaks=seq(0,badc_codesize,by=badc_codesize %/% 16),
                       minor_breaks=seq(0,badc_codesize,by=badc_codesize %/% 32)) +
    scale_color_discrete(name="DAC\nVref") +
    labs(x="BUT ADC code", y="IQRmean BUT ADC corrected difference (LSB)") +
    ## scale_y_continuous(breaks=seq(-10, 10, by=5), minor_breaks=seq(-10, 10, by=1)) +
    #test#scale_color_brewer(name="DAC\nVref", palette="Set1") +
    guides(color=guide_legend(override.aes=list(size=8, alpha=1))) +
    coord_cartesian(ylim=c(lower_noise_lsb_zoom, upper_noise_lsb_zoom))
  if (between(upper_noise_lsb_zoom, 4.5, 5.5)) {
    ### Hack for dealing with ugly breaks of (-3, 0, 3) which occur for 10bit ADCs
    g4inlb <-  g4inlb + scale_y_continuous(breaks=seq(-100, 100, by=2),
                                           minor_breaks=seq(-100, 100)) 
  }
  
  chart_filename <- sprintf("%s-%s-g4-inlb", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g4inlb,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  
  g4dnla <- ggplot(most_dnl_data_df %>% arrange(idx, dac_vref_mode),
                   mapping=aes(x=but_adc_code,
                               y=diffbestfit_err_lsb)) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "differential nonlinearity (DNL)"),
            subtitle=subtitle) +
    geom_point(aes(color=vref_map[dac_vref_mode]),
               stroke=0,
               size=1,
               alpha=0.4) + 
    labs(x="BUT ADC code", y="BUT ADC corrected difference (LSB)") +
    #scale_y_continuous(breaks=seq(-1, 1, by=0.5), minor_breaks=seq(-1, 1, by=0.1)) +
    #test#scale_color_brewer(name="DAC\nVref", palette="Set1") +
    scale_x_continuous(breaks=seq(0,badc_codesize,by=badc_codesize %/% 16),
                       minor_breaks=seq(0,badc_codesize,by=badc_codesize %/% 32)) +
    scale_color_discrete(name="DAC\nVref") +
    scale_fill_discrete(guide="none") +
    guides(color=guide_legend(override.aes=list(size=8, alpha=1))) +
    coord_cartesian(ylim=c(lower_noise_lsb, upper_noise_lsb))
  
  chart_filename <- sprintf("%s-%s-g4-dnla", plot_name, myversion)  
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g4dnla,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  g4dnlb <- ggplot(most_dnl_data_df %>% arrange(idx, dac_vref_mode),
                   mapping=aes(x=but_adc_code,
                               y=diffbestfit_err_lsb)) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "differential nonlinearity (DNL)"),
            subtitle=subtitle) +
    geom_point(aes(color=vref_map[dac_vref_mode]),
               stroke=0,
               size=upper_noise_lsb / upper_noise_lsb_zoom,
               alpha=(3 / max(na.omit(most_data_df$but_adc_code)))**0.7) +
    stat_summary(orientation="x",
                 fun=iqrmean,
                 geom="line",
                 size=1,
                 show.legend=FALSE) +   
    geom_label(data=data.frame(x=badc_codesize / 2, y=lower_noise_lsb_zoom,
                               text="noise should not be inferred from the black line (IQRmean) due to fewer samples above 2.048V and 4.096V"),
               mapping=aes(x=x, y=y, label=text),
               hjust=0.5, vjust=0.5,
               size=6,                
               family="mono", fontface="bold",
               fill="pink",
               alpha=0.7,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    labs(x="BUT ADC code", y="IQRmean BUT ADC corrected difference (LSB)") +
    #scale_y_continuous(breaks=seq(-1, 1, by=0.5), minor_breaks=seq(-1, 1, by=0.1)) +
    #test#scale_color_brewer(name="DAC\nVref", palette="Set1") +
    scale_x_continuous(breaks=seq(0,badc_codesize,by=badc_codesize %/% 16),
                       minor_breaks=seq(0,badc_codesize,by=badc_codesize %/% 32)) +   
    scale_color_discrete(name="DAC\nVref") +
    scale_fill_discrete(guide="none") +
    guides(color=guide_legend(override.aes=list(size=8, alpha=1))) +
    coord_cartesian(ylim=c(lower_noise_lsb_zoom, upper_noise_lsb_zoom))
  if (between(upper_noise_lsb_zoom, 4.5, 5.5)) {
    ### Hack for dealing with ugly breaks of (-3, 0, 3) which occur for 10bit ADCs
    g4dnlb <-  g4dnlb + scale_y_continuous(breaks=seq(-100, 100, by=2),
                                           minor_breaks=seq(-100, 100)) 
  }
  chart_filename <- sprintf("%s-%s-g4-dnlb", plot_name, myversion)  
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g4dnlb,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE) 
  
  
  g4dnlhmp <- ggplot(data=tri_dnl_hmp_data_df,
                     mapping=aes(x=ac_group,
                                 y=lsb_group)) +
    custom_theme + 
    theme(axis.text.x=element_text(size=14, angle=90, vjust=0.5, hjust=1),
          axis.text.y=element_text(size=16)) +
    ggtitle(paste(plot_board_name, "differential nonlinearity (DNL)"),
            subtitle=subtitle) +
    geom_tile(aes(fill=frac * 1000,
                  alpha=ifelse(frac > 0, 1, 0))) +
    geom_text(aes(x=ac_group,
                  y=lsb_group,
                  label=ifelse(frac > 0.0, round(frac * 1000, ifelse(frac * 1000 < 10, 1, 0)), "")),
              size=5,
              color="white") +
    scale_fill_continuous(name="per mille") + 
    scale_alpha_continuous(guide="none") +
    labs(x="BUT ADC code", y="BUT ADC code difference (LSB)") +
    scale_x_discrete(sec.axis=dup_axis(name=NULL)) +
    scale_y_discrete(sec.axis=dup_axis(name=NULL))
  
  ##scale_y_continuous("code difference (LSB)",
  ##                   sec.axis=sec_axis(~ . * (select_vref / dac_codes) * 1e3,
  ##                                     name="voltage difference (mV)")) +
  ##scale_y_continuous(breaks=seq(-1, 1, by=0.5), minor_breaks=seq(-1, 1, by=0.1)) +
  ##scale_color_brewer(name="DAC\nVref", palette="Set1") +
  ##scale_color_discrete(name="DAC\nVref") +
  ##scale_fill_discrete(guide="none") +
  ##guides(color=guide_legend(override.aes=list(size=8, alpha=1))) +
  ##coord_cartesian(ylim=c(-1, 1))
  
  chart_filename <- sprintf("%s-%s-g4-dnlhml", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g4dnlhmp,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  
  
  
  n_sample_details <- sprintf("%d samples per v.",
                              sample_count_summary_df[sample_count_summary_df$test_name == "noise",]$med_sample_count)
  
  g5a <- ggplot(noise_data_df %>% arrange(test_idx, idx),
                aes(x=mean_adc_v,
                    y=but_nominal_v, 
                    color=vref_map[dac_vref_mode])) +
    custom_theme +
    ggtitle(paste(plot_board_name, "noise"),
            subtitle=paste(c(common_conditions_pre,
                             noise_chan_list,
                             common_conditions_post,
                             n_sample_details),
                           collapse=", ")) +
    geom_rect(data=abovevref_corner_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=fill, alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    geom_point(size=5,
               alpha=0.25,
               stroke=0) +
    geom_line(data=data.frame(x=c(0, 1000),
                              y=c(0, 1000)),
              aes(x=x, y=y),
              color="grey20",
              inherit.aes=FALSE,
              show.legend=FALSE)  +
    labs(x="ADS1115 measurement (V)", y="BUT ADC nominal (V)", ) +
    #scale_color_discrete(name="DAC chan") +
    scale_color_discrete(name="DAC\nVref") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1))) +
    coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                    ylim=c(plot_lower_v, plot_upper_v))
  
  chart_filename <- sprintf("%s-%s-g5a", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g5a,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  g5b <- ggplot(noise_data_df %>% arrange(test_idx, idx),
                aes(x=mean_adc_v,
                    y=but_actual_v, 
                    color=vref_map[dac_vref_mode])) +
    custom_theme +
    ggtitle(paste(plot_board_name, "noise"),
            subtitle=paste(c(common_conditions_pre,
                             noise_chan_list,
                             common_conditions_post,
                             n_sample_details),
                           collapse=", ")) +
    geom_rect(data=abovevref_corner_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=fill, alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    geom_point(size=5,
               alpha=0.25,
               stroke=0) +
    geom_line(data=data.frame(x=c(0, 1000),
                              y=c(0, 1000)),
              aes(x=x, y=y),
              color="grey20",
              inherit.aes=FALSE,
              show.legend=FALSE)  +
    labs(x="ADS1115 measurement (V)", y="BUT ADC actual (V)", ) +
    #scale_color_discrete(name="DAC chan") +
    scale_color_discrete(name="DAC\nVref") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1))) +
    coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                    ylim=c(plot_lower_v, plot_upper_v))
  
  chart_filename <- sprintf("%s-%s-g5b", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g5b,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  
  
  if (dim(noise_data_df)[1] > 0) { 
    
    n_min <- min((noise_data_df$but_nominal_v - noise_data_df$mean_but_nominal_v) * 1000.0)
    n_max <- max((noise_data_df$but_nominal_v - noise_data_df$mean_but_nominal_v) * 1000.0)
    n_absmax <- max(abs(c(n_min, n_max)))
    n_abs_mv = 30  ### trying a fixed value (in mV)
    ### good wider values very much depends on the plot as to how much overlap there will be
    wider <- 2.0
    
    g6a <- ggplot(noise_data_df,
                  aes(x=(but_nominal_v - mean_adc_v) * 1000.0,
                      y=mean_adc_v_fac,
                      group=mean_adc_v_fac,
                      fill=vref_map[dac_vref_mode])) +
      custom_theme + 
      theme(panel.grid.major.y=element_blank(),
            panel.grid.minor.y=element_blank()) +
      ggtitle(paste(plot_board_name, "noise"),
              subtitle=paste(c(common_conditions_pre,
                               noise_chan_list,
                               common_conditions_post,
                               n_sample_details),
                             collapse=", ")) +
      ##geom_violin(adjust=2.5) +
      geom_violin(##scale="width",
        width=wider,
        alpha=0.7,
        bw=but_adc_actual_lsb_v / 2.5 * 1000.0,
        linewidth=1.5, show.legend=FALSE) +
      geom_boxplot(orientation="y",
                   width=wider*0.07,
                   fill=NA, color="black", alpha=0.5,
                   linewidth=1,
                   outlier.size=4, show.legend=FALSE) +
      stat_summary(orientation="y",
                   fun=mean, geom="point", shape=18, size=4, color="grey85",
                   show.legend=FALSE) +
      #geom_violin(#position=position_dodge(1),
      #             scale="width", linewidth=1.5, show.legend=FALSE) +
      # geom_boxplot(#position=position_dodge(1),
      #              width=0.20, color="white", linewidth=1,
      #              outlier.color="grey35", outlier.size=1.8, show.legend=FALSE) +
      # stat_summary(#position=position_dodge(1),
      #              fun=mean, geom="point", shape=18, size=5, color="grey65",
      #              show.legend=FALSE) +
      scale_y_discrete(sec.axis = dup_axis()) +
      labs(x="BUT ADC nominal offset (mV)", y="ADS1115 measurement (V)") +
      #guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
      #        shape=guide_legend(override.aes=list(size=5, alpha=1)))
      coord_cartesian(ylim=c(min(as.numeric(as.character(noise_data_df$v_idx))),
                             max(as.numeric(as.character(noise_data_df$v_idx)))))
    
    chart_filename <- sprintf("%s-%s-g6a", plot_name, myversion)
    ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
           g6a,
           dpi=100, height=15, width=20, units="in", limitsize = FALSE) 
    
    g6b <- ggplot(noise_data_df,
                  aes(x=(but_actual_v - mean_adc_v) * 1000.0,
                      y=mean_adc_v_fac,
                      group=mean_adc_v_fac,
                      fill=vref_map[dac_vref_mode])) +
      custom_theme + 
      theme(panel.grid.major.y=element_blank(),
            panel.grid.minor.y=element_blank()) +
      ggtitle(paste(plot_board_name, "noise"),
              subtitle=paste(c(common_conditions_pre,
                               noise_chan_list,
                               common_conditions_post,
                               n_sample_details),
                             collapse=", ")) +
      geom_violin(##scale="width",
        width=wider,
        alpha=0.7,
        bw=but_adc_actual_lsb_v / 2.5 * 1000.0,
        linewidth=1.5, show.legend=FALSE) +
      geom_boxplot(orientation="y",
                   width=wider*0.07,
                   fill=NA, color="black", alpha=0.5,
                   linewidth=1,
                   outlier.size=4, show.legend=FALSE) +
      stat_summary(orientation="y",
                   fun=mean, geom="point", shape=18, size=4, color="grey85",
                   show.legend=FALSE) +
      scale_y_discrete(sec.axis = dup_axis()) +
      labs(x="BUT ADC actual offset (mV)", y="ADS1115 measurement (V)") +
      coord_cartesian(ylim=c(min(as.numeric(as.character(noise_data_df$v_idx))),
                             max(as.numeric(as.character(noise_data_df$v_idx)))))
    
    chart_filename <- sprintf("%s-%s-g6b", plot_name, myversion)
    ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
           g6b,
           dpi=100, height=15, width=20, units="in", limitsize = FALSE) 
    
    badness_scale_fill_gradientn <- scale_fill_gradientn(colors=c("white", "green2", "green2", "yellow2", "orange2", "red2", "red3"),
                                                         values=scales::rescale(c(-1, 0, 1, 3, 6, 10, 30, 10000)),
                                                         limits=c(-1, 10000))
    
    g6c <- ggplot(noise_data_df,
                  aes(x=(but_nominal_v - mean_but_nominal_v) * 1000.0,
                      y=mean_adc_v_fac,
                      group=mean_adc_v_fac,
                      fill=vref_map[dac_vref_mode])) +
      custom_theme + 
      theme(panel.grid.major.y=element_blank(),
            panel.grid.minor.y=element_blank()) +
      ggtitle(paste(plot_board_name, "noise"),
              subtitle=paste(c(common_conditions_pre,
                               noise_chan_list,
                               common_conditions_post,
                               n_sample_details),
                             collapse=", ")) +
      ##geom_violin(adjust=2.5) +
      geom_violin(##scale="width",
        position=position_identity(),
        width=wider,
        alpha=0.8,
        bw=but_adc_actual_lsb_v / 2.5 * 1000.0,   ### prevent the dumbbell look
        linewidth=1.5, show.legend=FALSE) +
      geom_boxplot(orientation="y",
                   position=position_identity(),
                   width=wider*0.08,
                   fill=NA, color="black", alpha=0.5,
                   linewidth=1,
                   outlier.size=4, show.legend=FALSE) +
      new_scale_fill() +
      geom_label(data=noise_stats_df,
                 mapping=aes(x=n_abs_mv * 1.08,
                             y=mean_adc_v_fac,
                             ### A composite badness value or -1 for clear
                             fill=ifelse(mean_adc_v > but_v * 1.0125,
                                         -1,
                                         pmax(sd_actual_v * 1000,
                                              actual_range90_v * 1000 / 3,
                                              actual_range_v * 1000 / 8)),
                             hjust=1,
                             label=summary_ac1_txt),
                 position=position_identity(),
                 alpha=0.7,
                 #fill=rag20[ifelse(round(noise_stats_df$sd_actual_v) <= 19,
                 #                  round(noise_stats_df$sd_actual_v) + 1,
                 #                  20 - 1)],
                 size=5.25,
                 lineheight=0.8,
                 family="mono", fontface="bold",
                 inherit.aes=FALSE,
                 show.legend=FALSE) +
      badness_scale_fill_gradientn + 
      scale_x_continuous(breaks=seq(-100, 100, by=10),
                         minor_breaks=seq(-100, 100, by=5)) +
      scale_y_discrete(sec.axis = dup_axis()) +
      labs(x="mean-centred BUT ADC nominal (mV)", y="ADS1115 measurement (V)") +
      #scale_color_discrete(name="DAC chan") +
      #scale_shape_discrete(name="DAC\nVref") +
      #guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
      #        shape=guide_legend(override.aes=list(size=5, alpha=1))) +
      coord_cartesian(xlim=c(0.0 - n_abs_mv, n_abs_mv),
                      ylim=c(min(as.numeric(as.character(noise_data_df$v_idx))),
                             max(as.numeric(as.character(noise_data_df$v_idx)))))
    
    #coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
    #                 ylim=c(plot_lower_v, plot_upper_v))
    
    chart_filename <- sprintf("%s-%s-g6c", plot_name, myversion)
    ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
           g6c,
           dpi=100, height=15, width=20, units="in", limitsize = FALSE) 
    
    g6d <- ggplot(noise_data_df,
                  aes(x=(but_corrected_v - mean_but_corrected_v) * 1000.0,
                      y=mean_adc_v_fac,
                      group=mean_adc_v_fac,
                      fill=vref_map[dac_vref_mode])) +
      custom_theme + 
      theme(panel.grid.major.y=element_blank(),
            panel.grid.minor.y=element_blank()) +
      ggtitle(paste(plot_board_name, "noise (linear correction)"),
              subtitle=paste(paste(c(common_conditions_pre,
                                     noise_chan_list,
                                     common_conditions_post,
                                     n_sample_details),
                                   collapse=", "),
                             ### "labelling" hack!
                             paste0("distortion (mV)", strrep(" ", 116), "noise (mV)"),
                             sep="\n")) +
      ##geom_violin(adjust=2.5) +
      geom_violin(##scale="width",
        position=position_identity(),
        width=wider,
        alpha=0.8,
        bw=but_adc_actual_lsb_v / 2.5 * 1000.0,   ### prevent the dumbbell look
        linewidth=1.5, show.legend=FALSE) +
      geom_boxplot(orientation="y",
                   position=position_identity(),
                   width=wider*0.08,
                   fill=NA, color="black", alpha=0.5,
                   linewidth=1,
                   outlier.size=4, show.legend=FALSE) +
      new_scale_fill() +
      ### distortion on the left
      geom_label(data=noise_stats_df,
                 mapping=aes(x=0 - n_abs_mv * 1.08,
                             y=mean_adc_v_fac,
                             ### A composite badness value or -1 for clear (white) background
                             fill=ifelse(mean_adc_v > but_v * 1.0125,
                                         -1,
                                         pmax(abs(mean_corrected_offset_v) * 1000.0 * 1.25, 
                                              abs(iqrmean_corrected_offset_v) * 1000.0 * 1.5)),
                             #alpha=0.7,
                             hjust=0,
                             label=summary_co2_txt),
                 position=position_identity(),
                 alpha=0.7,
                 #fill=rag20[ifelse(round(noise_stats_df$sd_actual_v) <= 19,
                 #                  round(noise_stats_df$sd_actual_v) + 1,
                 #                  20 - 1)],
                 size=5.25,
                 lineheight=0.8,
                 family="mono", fontface="bold",
                 inherit.aes=FALSE,
                 show.legend=FALSE) +
      ### noise on the right
      geom_label(data=noise_stats_df,
                 mapping=aes(x=n_abs_mv * 1.08,
                             y=mean_adc_v_fac,
                             ### A composite badness value or -1 for clear
                             fill=ifelse(mean_adc_v > but_v * 1.0125,
                                         -1,
                                         pmax(sd_actual_v * 1000,
                                              actual_range90_v * 1000 / 3,
                                              actual_range_v * 1000 / 8)),
                             #alpha=0.7,
                             hjust=1,
                             label=summary_co1_txt),
                 position=position_identity(),
                 alpha=0.7,
                 #fill=rag20[ifelse(round(noise_stats_df$sd_actual_v) <= 19,
                 #                  round(noise_stats_df$sd_actual_v) + 1,
                 #                  20 - 1)],
                 size=5.25,
                 lineheight=0.8,
                 family="mono", fontface="bold",
                 inherit.aes=FALSE,
                 show.legend=FALSE) +
      badness_scale_fill_gradientn +
      scale_x_continuous(breaks=seq(-100, 100, by=10),
                         minor_breaks=seq(-100, 100, by=5)) +
      scale_y_discrete(sec.axis = dup_axis()) +
      labs(x="mean-centred BUT ADC corrected (mV)", y="ADS1115 measurement (V)") +
      #scale_color_discrete(name="DAC chan") +
      #scale_shape_discrete(name="DAC\nVref") +
      #guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
      #        shape=guide_legend(override.aes=list(size=5, alpha=1))) +
      coord_cartesian(xlim=c(0.0 - n_abs_mv, n_abs_mv),
                      ylim=c(min(as.numeric(as.character(noise_data_df$v_idx))),
                             max(as.numeric(as.character(noise_data_df$v_idx)))))
    
    #coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
    #                 ylim=c(plot_lower_v, plot_upper_v))
    
    chart_filename <- sprintf("%s-%s-g6d", plot_name, myversion)
    ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
           g6d,
           dpi=100, height=15, width=20, units="in", limitsize = FALSE) 
  }
  
  
  
  ### 40mV looks better but 50mV has better coverage of shenanigans at
  ### top end
  lowvolt_zoom_v <- 0.040
  highvolt_zoom_v <- 0.100
  
  ### For things which can't measure low voltages like ESP32
  codestart_lower_v <- lowest_v - highvolt_zoom_v * 0.3
  codestart_upper_v <- lowest_v + highvolt_zoom_v * 0.7
  
  lowest_trio_v <- min(lc_sat_v, but_nominal_v, but_v)
  fsrtop3_upper_v <- lowest_trio_v + highvolt_zoom_v * 0.2
  fsrtop3_lower_v <- lowest_trio_v - highvolt_zoom_v * 0.8  
  
  
  if (abs(sat_v - but_nominal_v) < highvolt_zoom_v * 0.4) {
    ### if they are close together then use upper voltage, an ADC
    ### can capture values a tiny bit above the aref in some cases
    ### even without a PGA
    fsrtop4_upper_v <- max(lc_sat_v, but_nominal_v) + highvolt_zoom_v * 0.1
    fsrtop4_lower_v <- max(lc_sat_v, but_nominal_v) - highvolt_zoom_v * 0.9
  } else {
    fsrtop4_upper_v <- min(lc_sat_v, but_nominal_v) + highvolt_zoom_v * 0.3
    fsrtop4_lower_v <- min(lc_sat_v, but_nominal_v) - highvolt_zoom_v * 0.7     
  }
  
  highest_trio_v <- max(lc_sat_v, but_nominal_v, but_v)
  fsrtop5_upper_v <- highest_trio_v + highvolt_zoom_v * 0.2
  fsrtop5_lower_v <- highest_trio_v - highvolt_zoom_v * 0.8     
  
  voltages_text <- sprintf(paste("nominal voltage                %sV",
                                 "MCU voltage                    %.3fV",
                                 "voltage at ADC min             %.3fV",
                                 "voltage at ADC max             %.3fV",
                                 "calc. ADC voltage at max (lc)  %.3fV",
                                 sep="\n"),
                           as.character(but_nominal_v),
                           but_v,
                           lowest_v,
                           sat_v,
                           lc_sat_v)
  
  code_summary_text <- sprintf(paste("%s",
                                     "min code           %6d",
                                     "max code           %6d",
                                     "absent codes       %6d",
                                     "absent code groups %6d",
                                     sep="\n"),
                               paste0(adc_read_function_name,
                                      strrep(" ", max(0, (25 - str_length(adc_read_function_name)) / 2))),
                               but_min_code,
                               but_max_code,
                               but_absent_code_count,
                               but_absent_codegroup_count)
  
  enob_text <- sprintf(paste("ADC code size         %2dbits",
                             "ENOB (t+g+vl)      %5sbits",
                             "ENOB (n)           %5sbits",
                             "RMS Noise (n)      %5smV  ",
                             "RMS Dist. (t+g+vl) %5smV  ",
                             sep="\n"),
                       but_adc_bits,
                       sprintf("%.2f", most_enob),
                       sprintf("%.2f", noise_enob),
                       sprintf("%.2f", noise_n_rms_v * 1e3),
                       sprintf("%.2f", most_d_rms_v * 1e3)
  )
  
  g7a <- ggplot(most_data_df %>% arrange(idx),
                aes(x=adc_v,
                    y=but_corrected_v, 
                    color=vref_map[dac_vref_mode], shape=adc_mode)) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "linear correction with best fit"),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    geom_rect(data=abovevref_corner_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=fill, alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    geom_label(data=data.frame(x=plot_lower_v, y=plot_upper_v, text=voltages_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=0, vjust=1,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=plot_upper_v, y=plot_lower_v, text=enob_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=plot_upper_v, y=plot_upper_v / 2.8, text=code_summary_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0.5,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_point(size=1,
               alpha=0.5) +
    geom_line(data=data.frame(x=c(0, 1000),
                              y=c(0, 1000)),
              aes(x=x, y=y),
              color="grey20",
              inherit.aes=FALSE,
              show.legend=FALSE)  +
    #geom_line(aes(x=adc_v, y=model1_v),
    #             color="black") + 
    labs(x="ADS1115 measurement (V)", y="BUT ADC corrected (V)") +
    scale_color_discrete(name="DAC\nVref") +
    scale_shape_discrete(name="ADS1115\nmode") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1))) +
    coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                    ylim=c(plot_lower_v, plot_upper_v))
  
  chart_filename <- sprintf("%s-%s-g7a", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g7a,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)  
  
  zoomed_voltage_breaks <- (seq(-10 * 100, 10 * 100) / 100)
  zoomed_voltage_minor_breaks <- (seq(-10 * 200, 10 * 200) / 200)
  g7b <- ggplot(most_data_df %>% arrange(idx),
                aes(x=adc_v,
                    y=but_corrected_v, 
                    color=vref_map[dac_vref_mode], shape=adc_mode)) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "linear correction with best fit"),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    geom_rect(data=abovevref_corner_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=fill, alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    geom_point(size=3, alpha=1) +
    geom_step(aes(x=x,y=y),
              data=actual_perfect_adc_steps,
              linewidth=1,
              color="grey20",
              inherit.aes=FALSE) +
    stat_summary(aes(x=adc_v,
                     y=but_corrected_v),
                 orientation="x",
                 fun=iqrmean,
                 geom="point",
                 shape=13,
                 size=1.8,
                 color="black",
                 show.legend=FALSE,
                 inherit.aes=FALSE) +
    labs(x="ADS1115 measurement (V)", y="BUT ADC corrected (V)") +
    scale_x_continuous(breaks=zoomed_voltage_breaks,
                       minor_breaks=zoomed_voltage_minor_breaks) +
    scale_y_continuous(breaks=zoomed_voltage_breaks,
                       minor_breaks=zoomed_voltage_minor_breaks) +
    scale_color_discrete(name="DAC\nVref") +
    scale_shape_discrete(name="ADS1115\nmode") +
    guides(color=guide_legend(override.aes=list(size=5, alpha=1)),
           shape=guide_legend(override.aes=list(size=5, alpha=1)))
  
  g7b1 <- g7b +
    geom_label(data=data.frame(x=0, y=lowvolt_zoom_v, text=voltages_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=0, vjust=1,
               size=8,                
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=lowvolt_zoom_v, y=0, text=enob_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    coord_cartesian(xlim=c(0, lowvolt_zoom_v),
                    ylim=c(0, lowvolt_zoom_v))
  
  chart_filename <- sprintf("%s-%s-g7b1", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g7b1,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)  
  
  g7b2 <- g7b +
    geom_label(data=data.frame(x=codestart_lower_v, y=codestart_upper_v, text=voltages_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=0, vjust=1,
               size=8,                
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=codestart_upper_v, y=codestart_lower_v, text=enob_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    coord_cartesian(xlim=c(codestart_lower_v, codestart_upper_v),
                    ylim=c(codestart_lower_v, codestart_upper_v))
  
  chart_filename <- sprintf("%s-%s-g7b2", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g7b2,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)     
  
  g7b3 <- g7b +
    geom_label(data=data.frame(x=fsrtop3_lower_v, y=fsrtop3_upper_v, text=voltages_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=0, vjust=1,
               size=8,                
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=fsrtop3_upper_v, y=fsrtop3_lower_v, text=enob_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    coord_cartesian(xlim=c(fsrtop3_lower_v, fsrtop3_upper_v),
                    ylim=c(fsrtop3_lower_v, fsrtop3_upper_v))
  
  chart_filename <- sprintf("%s-%s-g7b3", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g7b3,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  g7b4 <- g7b +
    geom_label(data=data.frame(x=fsrtop4_lower_v, y=fsrtop4_upper_v, text=voltages_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=0, vjust=1,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=fsrtop4_upper_v, y=fsrtop4_lower_v, text=enob_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    coord_cartesian(xlim=c(fsrtop4_lower_v, fsrtop4_upper_v),
                    ylim=c(fsrtop4_lower_v, fsrtop4_upper_v))
  
  chart_filename <- sprintf("%s-%s-g7b4", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g7b4,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  g7b5 <- g7b +
    geom_label(data=data.frame(x=fsrtop5_lower_v, y=fsrtop5_upper_v, text=voltages_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=0, vjust=1,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=fsrtop5_upper_v, y=fsrtop5_lower_v, text=enob_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    coord_cartesian(xlim=c(fsrtop5_lower_v, fsrtop5_upper_v),
                    ylim=c(fsrtop5_lower_v, fsrtop5_upper_v))
  
  chart_filename <- sprintf("%s-%s-g7b5", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g7b5,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  lines_most_df <- most_data_df %>%
    subset(!is.na(but_adc_ideal_code)) %>%
    group_by(but_adc_ideal_code) %>%
    summarise(mean_adc_v=mean(adc_v),
              iqrmean_but_actual_v=iqrmean(but_actual_v),
              iqrmean_but_corrected_v=iqrmean(but_corrected_v)) %>%
    melt(id.vars=c("but_adc_ideal_code", "mean_adc_v"),
         variable.name="line",
         value.name="voltage")
  
  bg_fill_values <- alpha("blue", seq(0, 6)/20.0 + c(0,rep(0.05, 6)))
  names(bg_fill_values) <- seq(0, 6)
  
  ### This is similar to plot a but shows best fit as a line
  ### experimenting with  stat_summary_2d() to show actual data as
  ### faint, slightly blocky background to plot
  g7c <- ggplot() +
    custom_theme + 
    ggtitle(paste(plot_board_name, "ideal, actual and lc best fit"),
            subtitle=paste(c(common_conditions_pre,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    geom_rect(data=abovevref_corner_df,
              aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
                  fill=fill, alpha=alpha),
              color=NA,
              inherit.aes=FALSE,
              show.legend=FALSE) +
    scale_fill_identity() +
    scale_alpha_identity() +
    new_scale_fill() +
    stat_summary_2d(data=most_data_df,
                    mapping=aes(x=adc_v,
                                y=but_actual_v,
                                z=1,
                                fill=after_stat(value)),
                    fun=function(x) { return(as.factor(min(6, (sum(x) + 3) %/% 6))) },
                    binwidth=(plot_upper_v - plot_lower_v) / 200,
                    show.legend=FALSE
    ) +
    geom_line(size=0.8) +
    scale_fill_manual(values=bg_fill_values) + 
    geom_label(data=data.frame(x=plot_lower_v, y=plot_upper_v, text=voltages_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=0, vjust=1,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=plot_upper_v, y=plot_lower_v, text=enob_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_label(data=data.frame(x=plot_upper_v, y=plot_upper_v / 2.8, text=code_summary_text),
               mapping=aes(x=x, y=y, label=text),
               hjust=1, vjust=0.5,
               size=8,
               family="mono", fontface="bold",
               alpha=0.5,
               inherit.aes=FALSE,
               show.legend=FALSE) +
    geom_line(data=rbind(data.frame(but_adc_ideal_code=NA,
                                    mean_adc_v=c(0, 10),
                                    line="ideal",
                                    voltage=c(0, 10)),
                         lines_most_df),
              mapping=aes(x=mean_adc_v,
                          y=voltage, 
                          color=line,
                          group=line),
              size=0.8) +
    labs(x="ADS1115 measurement (V)", y="BUT ADC (V)") +
    scale_color_manual(name="line\n(IQRmean)",
                       labels=c("ideal"="ideal",
                                "iqrmean_but_actual_v"="actual",
                                "iqrmean_but_corrected_v"="best fit"
                       ),
                       values=c("ideal"="grey20",
                                "iqrmean_but_actual_v"=alpha("blue",0.6),
                                "iqrmean_but_corrected_v"=alpha("red", 0.8))) +
    guides(color=guide_legend(override.aes=list(linewidth=5))) +
    coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                    ylim=c(plot_lower_v, plot_upper_v))
  
  chart_filename <- sprintf("%s-%s-g7c", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g7c,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)
  
  
  
  
  if (badc_codesize <= 8192) {
    cc_ac_breaks <- seq(0, badc_codesize, by=128)
    cc_divisor <- 1024
    local_theme <- theme(strip.background=element_blank(),
                         strip.text=element_blank())
  } else {
    ### For Uno R4 14bit ADC use values for 8 facets
    cc_ac_breaks <- seq(0, badc_codesize, by=256)
    cc_divisor <- 2048
    local_theme <- theme(strip.background=element_blank(),
                         strip.text=element_blank(),
                         # cram them in
                         axis.text.x = element_text(hjust=0.5, size=title_size/4),
                         panel.spacing.y = unit(0.4, "lines"))
  }
  
  ### A look at DAC code coverage
  p99h <- quantile((code_coverage_df %>% group_by(names(bac_count)) %>% mutate(total = sum(bac_count)))$total, 0.99)
  maxh <- max((code_coverage_df %>% group_by(names(bac_count)) %>% mutate(total = sum(bac_count)))$total)
  ##maxh_nolow <- max((subset(code_coverage_df, test_name != "verylow") %>% group_by(names(bac_count)) %>% mutate(total = sum(bac_count)))$total)
  bar_height <- min(p99h * 1.4, maxh)
  chunked_c_c_df <- code_coverage_df %>%
    mutate(code=as.integer(names(bac_count)),
           code_lower=code %% cc_divisor,
           code_upper=factor(code %/% cc_divisor,
                             levels=seq(badc_codesize %/% cc_divisor, 1) - 1)) %>%
    group_by(code) %>%
    mutate(total=sum(bac_count)) %>%
    ungroup()
  
  ### This will spit a warning message "No shared levels found between `names(values)` of the manual scale and the data's colour values."
  ### for any ADCs with no absent codes
  g8a <- ggplot(chunked_c_c_df,
                aes(x=code,
                    y=as.integer(bac_count),
                    fill=test_name)) +
    custom_theme + 
    local_theme +
    ggtitle(paste(plot_board_name, "code coverage"),
            subtitle=paste(na.omit(c(common_conditions_pre,
                                     paste(na.omit(c(converted_text,
                                                     adc_read_function_name)),
                                           collapse=" "),
                                     tri_chan_list,
                                     common_conditions_post)),
                           collapse=", ")) +
    geom_bar(position="stack", stat="identity", width=1) +
    geom_point(data=chunked_c_c_df %>%
                 subset(total == 0) %>%
                 group_by(code, code_lower, code_upper)
               %>% summarise(pos=0 - bar_height/40),
               aes(x=code, y=pos, color="absent"),
               size=0.5,
               inherit.aes=FALSE) +
    labs(x="BUT ADC code", y="count") +
    scale_x_continuous(breaks=cc_ac_breaks,
                       minor_breaks=NULL) +
    scale_y_continuous(minor_breaks=NULL) +
    scale_fill_discrete(name="test") + 
    scale_color_manual(name=NULL,
                       values=c("absent"="maroon3")) + 
    guides(fill=guide_legend(order=1),
           color=guide_legend(order=2, override.aes=list(size=5))) +
    coord_cartesian(ylim=c(0, bar_height)) +
    facet_wrap(vars(code_upper),
               ncol=1,
               dir="v",
               scale="free_x")
  
  chart_filename <- sprintf("%s-%s-g8a", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g8a,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)  
  
  ### Same again with the unprocessed codes (for ESP32)
  p99h <- quantile((code_coverage_df %>% group_by(names(bac_unproc_count)) %>% mutate(total = sum(bac_unproc_count)))$total, 0.99)
  maxh <- max((code_coverage_df %>% group_by(names(bac_unproc_count)) %>% mutate(total = sum(bac_unproc_count)))$total)
  ##maxh_nolow <- max((subset(code_coverage_df, test_name != "verylow") %>% group_by(names(bac_count)) %>% mutate(total = sum(bac_count)))$total)
  bar_height <- min(p99h * 1.4, maxh)
  chunked_c_c_df <- code_coverage_df %>%
    mutate(code=as.integer(names(bac_unproc_count)),
           code_lower=code %% cc_divisor,
           code_upper=factor(code %/% cc_divisor,
                             levels=seq(badc_codesize %/% cc_divisor, 1) - 1)) %>%
    group_by(code) %>%
    mutate(total=sum(bac_unproc_count)) %>%
    ungroup()
  
  g8b <- ggplot(chunked_c_c_df,
                aes(x=code,
                    y=as.integer(bac_unproc_count),
                    fill=test_name)) +
    custom_theme + 
    local_theme + 
    ggtitle(paste(plot_board_name, "unprocessed code coverage"),
            subtitle=paste(c(common_conditions_pre,
                             adc_read_function_name,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    geom_bar(position="stack", stat="identity", width=1) +
    geom_point(data=chunked_c_c_df %>%
                 subset(total == 0) %>%
                 group_by(code, code_lower, code_upper)
               %>% summarise(pos=0 - bar_height/40),
               aes(x=code, y=pos, color="absent"),
               size=0.5,
               inherit.aes=FALSE) +
    labs(x="BUT ADC code", y="count") +
    scale_x_continuous(breaks=cc_ac_breaks,
                       minor_breaks=NULL) +
    scale_y_continuous(minor_breaks=NULL) +
    scale_fill_discrete(name="test") + 
    scale_color_manual(name=NULL,
                       values=c("absent"="maroon3")) + 
    guides(fill=guide_legend(order=1),
           color=guide_legend(order=2, override.aes=list(size=5))) +
    coord_cartesian(ylim=c(0, bar_height)) +
    facet_wrap(vars(code_upper),
               ncol=1,
               dir="v",
               scale="free_x")
  
  chart_filename <- sprintf("%s-%s-g8b", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g8b,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)     
  
  
  ### noise correlation with voltage
  
  # bucket_most_data_df <- most_data_df %>%
  #   subset(adc_v < lc_sat_v) %>%
  #   mutate(bucket_v=round(adc_v / 2, 2) * 2,
  #          bucket_bucket_v=as.factor(round(bucket_v / 5, 1) * 5)) %>%
  #   arrange(bucket_v) %>%
  #   group_by(bucket_v, bucket_bucket_v) %>%
  #   summarise(bucket_mean_adc_v=mean(adc_v),
  #             bucket_offsetm_corrected_diff_v=mean(but_corrected_v - bucket_mean_adc_v),
  #             bucket_sd_corrected_diff_v=sd(but_corrected_v - bucket_offsetm_corrected_diff_v),
  #             count=length(adc_v)) %>%
  #   group_by(bucket_bucket_v) %>%
  #   mutate(bb_group_id=cur_group_id()) %>%
  #   ungroup()
  
  bucket_most_data_df <- most_data_df %>%
    subset(adc_v < lc_sat_v) %>%
    group_by(adc_v) %>%
    mutate(bucket_v=round(adc_v / 2, 2) * 2,
           bucket_bucket_v=as.factor(round(bucket_v / 5, 1) * 5),
           corrected_diff_v=but_corrected_v - adc_v) %>%
    ungroup() %>%
    arrange(bucket_v) %>%
    group_by(bucket_v, bucket_bucket_v) %>%
    summarise(bucket_mean_c_diff=mean(corrected_diff_v),
              bucket_sd_offmean_corr_diff_v=sd(corrected_diff_v - bucket_mean_c_diff),
              count=length(corrected_diff_v)) %>%
    group_by(bucket_bucket_v) %>%
    mutate(bb_group_id=cur_group_id()) %>%
    ungroup()
  
  
  ### g9 data prep
  first_models <- by(bucket_most_data_df,
                     bucket_most_data_df$bucket_bucket_v,
                     function(x) { try(lm(bucket_sd_offmean_corr_diff_v~bucket_v,
                                          data=x)) })
  
  ### Pondering how to downplay outliers - unfinished...
  bucket_most_data_df <- bucket_most_data_df %>%
    group_by(bb_group_id) %>%
    mutate(f_model_v=predict(first_models[[bb_group_id[1]]]),
           sd_group=sd(bucket_sd_offmean_corr_diff_v - f_model_v))
  
  models <- first_models
  
  ### Some experimental stuff to see if the noise has a near constant level
  ### transitioning to a proportional to Vdd (upward slope)
  ### called "kinked" here
  line_function <- function(a, b, c, l_t, v) {
    if (l_t == "flat") {
      return(rep(a, length(v)))
    } else if (l_t == "straight") {
      return(b + c * v)
    } else if (l_t == "kinked") {
      return(pmax(a, b + c * v))
    }
  }
  line_function_text <- function(a, b, c, l_t) {
    a <- a * 1000.0
    b <- b * 1000.0
    c <- c * 1000.0
    if (l_t == "flat") {
      return(sprintf('noise_mV==%.2f',
                     a))
    } else if (l_t == "straight") {
      return(sprintf('noise_mV==%.2f + %.2f %%*%% voltage',
                     b, c))
    } else if (l_t == "kinked") {
      return(sprintf('noise_mV==maximum(%.2f, %.2f + %.2f %%*%% voltage)',
                     a, b, c))
    }
  }
  rabs_function <- function(params, data, line_type) {
    a <- params[1]
    b <- params[2]
    c <- params[3]
    
    # Calculate predictions
    bucket_sd_offmean_corr_diff_v_pred <- line_function(a, b, c, line_type, data$bucket_v)
    
    # Return sum of error magnitudes (not squared)
    return(sum(abs(data$bucket_sd_offmean_corr_diff_v - bucket_sd_offmean_corr_diff_v_pred)))
  }
  
  nv_models <- list()
  for (line_type in c("flat", "straight", "kinked")) {
    nv_models[[line_type]] <- optim(par=c(a=0.005, b=0.0, c=0.003), 
                                    fn=rabs_function, 
                                    data=subset(bucket_most_data_df,
                                                between(bb_group_id,
                                                        min(bb_group_id) + 1,
                                                        max(bb_group_id) - 1)),
                                    line_type=line_type)
  }
  
  nv_line_type <- "kinked"
  nv_params <- nv_models[[nv_line_type]]$par
  nv_error <- nv_models[[nv_line_type]]$value
  noise_transition_v <- (nv_params["a"] - nv_params["b"]) / nv_params["c"]
  noise_transition_v <- ifelse(between(noise_transition_v, 0, lc_sat_v),
                               noise_transition_v,
                               NA)
  
  ### If the noise transition is > 90% then replace it with a straight line fit
  ### as it's unlikely to be genuine Vdd related noise
  if (is.na(noise_transition_v)
      | noise_transition_v / lc_sat_v > 0.825 | noise_transition_v / lc_sat_v < 0.075
      | nv_models[["straight"]]$value < nv_error * 0.95) {
    nv_line_type <- "straight"
    nv_params <- nv_models[[nv_line_type]]$par
    nv_error <- nv_models[[nv_line_type]]$value
    noise_transition_v <- 0.0
    
    ### for tiny gradients (or bizarre negative ones?!) let's go flat
    if (nv_params["c"] < 0.0001 | nv_models[["flat"]]$value < nv_error) {
      nv_line_type <- "flat"
      nv_params <- nv_models[[nv_line_type]]$par
      nv_error <- nv_models[[nv_line_type]]$value
      noise_transition_v <- NA
    }
    
  }
  nv_const1 <- ifelse(nv_line_type != "straight", nv_params["a"], NA)
  nv_const2 <- ifelse(nv_line_type != "flat", nv_params["b"], NA)
  nv_coefv <- ifelse(nv_line_type != "flat", nv_params["c"], NA)
  nv_fitline_text <- line_function_text(nv_params["a"], nv_params["b"], nv_params["c"],
                                        nv_line_type)
  noise_vs_v_upper_mv <- 15  ### this was 20
  g9 <-  ggplot(mapping=aes(x=bucket_v,
                            y=bucket_sd_offmean_corr_diff_v * 1000),
                data=bucket_most_data_df) +
    custom_theme + 
    ggtitle(paste(plot_board_name, "noise trend with voltage"),
            subtitle=paste(c(common_conditions_pre,
                             adc_read_function_name,
                             tri_chan_list,
                             common_conditions_post),
                           collapse=", ")) +
    
    geom_point() +
    lapply(names(models),
           function(nm, mods) { geom_line(mapping=aes(x=bucket_v,
                                                      y=predict(mods[[nm]]) * 1000.0,
                                                      color="dodgerblue2"),
                                          data=subset(bucket_most_data_df, bucket_bucket_v == nm),
                                          linewidth=4) },
           models) +
    geom_line(aes(x=bucket_v,
                  y=line_function(nv_params["a"], nv_params["b"], nv_params["c"],
                                  nv_line_type, bucket_v) * 1000.0,
                  color=ifelse(is.na(noise_transition_v) | bucket_most_data_df$bucket_v < noise_transition_v,"green2", "magenta")),
              linewidth=2.5,
              alpha=0.9) + 
    geom_vline(xintercept=noise_transition_v,
               linetype="dashed",
               color="magenta",
               alpha=0.9,
               linewidth=1.5) + 
    annotate("text", parse=TRUE,
             size=9,
             x=0.1, y=0.82 * 20,
             hjust=0,
             label=nv_fitline_text) +
    labs(x="ADS1115 measurement (V)",
         y="standard deviation of mean-centred BUT ADC corrected (noise rms) (mV)") +
    scale_colour_manual(name="line fitting",
                        values=c("green2"="green2",
                                 "magenta" = "magenta",
                                 "dodgerblue2"="dodgerblue2"),
                        labels=c("green2"="constant",
                                 "magenta" = "proportional\nto Vdd",
                                 "dodgerblue2"="500mV\nsegments")) +
    coord_cartesian(ylim=c(0, noise_vs_v_upper_mv))
  
  chart_filename <- sprintf("%s-%s-g9", plot_name, myversion)
  ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
         g9,
         dpi=100, height=15, width=20, units="in", limitsize = FALSE)   
  
  coverage_pct <- min(1.0, (sat_v - lowest_v ) / but_nominal_v) * 100.0
  
  ### The data for CSV file
  csv_df <- e_csv_df
  if (!csv_skip) {
    fitfn_as_text <- gsub("maximum", "max",
                          gsub("%", "", 
                               gsub("==","=", nv_fitline_text)))  ### going to hell for this
    csv_df <- data.frame("Board_Manufacturer"=board_manu,
                         "Board_Name"=board_name,
                         "Board_Instance"=board_instance,
                         "Language"=but_language,
                         "Power"=power_source,
                         "Nominal_V"=but_nominal_v,
                         "Actual_V"=but_v,
                         "Bits"=but_adc_bits,
                         "ENOB_(t+g+vl)"=round(most_enob, 1),
                         "ENOB_(n)"=round(noise_enob, 1),
                         "Noise_(n)_mV"=round(noise_n_rms_v * 1e3, 1),
                         "Distortion (t+g+vl)_mV"=round(most_d_rms_v * 1e3, 1),
                         "Read_Function"=adc_read_function_name,
                         "Min_V"=round(lowest_v, 3),
                         "Max_V"=round(sat_v, 3),
                         "Coverage_pct"=round(coverage_pct, 2),
                         "Min_code"=but_min_code,
                         "Max_code"=but_max_code,
                         "Absent_codes"=but_absent_code_count,
                         "Absent_code_groups"=but_absent_codegroup_count,
                         "Noise_model"=fitfn_as_text,
                         "Noise_voltage_constant1"=round(nv_const1, 6),
                         "Noise_voltage_constant2"=round(nv_const2, 6),
                         "Noise_voltage_coefficient"=round(nv_coefv, 6),
                         "Ambient_temperature"=amb_temp,
                         "Software_version"=as.character(but_sw_ver),
                         "Plot_name"=plot_name,
                         "Sample_counts"=sample_count_summary_text,
                         "Notes"=csv_notes,
                         row.names=NULL)
  }
  ### Anything that is used for combined plots after the loop
  stats <- mget(c("most_enob", "noise_enob",
                  "noise_n_rms_v", "most_d_rms_v"))

  return(list(csv_df=csv_df, lines_most_df=lines_most_df, stats=stats))
}




print("==== Generating plots ====")
extra_calculated_data <- list()  ### for any data that's created here and needed afterwards for combined plots
for (name in test_list_names) {
  print(name)
  extra_calculated_data[[name]] <- process_and_plot(adc_data[[name]])
}  

### TODO - this needs to deal with empty data.frames in some way
all_csv_df <- rbind(all_csv_df, Reduce(full_join, lapply(extra_calculated_data, function(x) { x$csv_df })))
### Write the CSV statistics file
#write.csv(all_csv_df, eol = "n", na="", row.names=FALSE, file=stdout())
write.csv(all_csv_df, eol = "\n", na="", row.names=FALSE, file=csv_filename)


### Extra plots of interesting combinations/comparisons
### CPX K1 vs K3 - INL? On same plot or facets?
### MCP IC1 vs IC2 - INL?
### Espressif linearity - uncalibrated vs calibrated
### Some way of showing the USB and battery power together? some combined noise plots?

### Espressif linearity g7c variants

#noncalib isodac-mcp4728-ads1115-adctesting-esp32cloneraw-nimh-1-v9-g7c.png  as crazy as you'd expect
#noncalib isodac-mcp4728-ads1115-adctesting-feathers2raw-lipo-1-v9-g7c.png   visually not different to calib version bar linear correction


#calib isodac-mcp4728-ads1115-adctesting-esp32clone-nimh-1-v9-g7c.png
#calib isodac-mcp4728-ads1115-adctesting-feathers2-lipo-1-v9-g7c.png




### Espressif raw together in g7c style
plot_lower_v <- 0.0
plot_upper_v <- 3.4

bg_alpha_values <- seq(0, 6)/20.0 + c(0,rep(0.05, 6))
names(bg_alpha_values) <- seq(0, 6)

names_for_plot <- c("ESP32 DevKitC Clone {NOCAL} : NiMH t1"="DevKitC Clone\nESP32",
                    "FeatherS2 {NOCAL} : LiPo t1"="UM FeatherS2\nESP32-S2",
                    "Xiao ESP32C6 {NOCAL} : NiMH t1"="SS Xiao ESP32C6",
                    "Xiao ESP32C5 {NOCAL} : NiMH t1"="SS Xiao ESP32C5")

espressif_raw_lines_most_df <- bind_rows(lapply(names(names_for_plot),
                                                function(nm) { return(data.frame(extra_calculated_data[[nm]]$lines_most_df %>%
                                                                                   subset(line == "iqrmean_but_actual_v"),
                                                                                 shortname=names_for_plot[nm]))}))
espressif_raw_lines_most_df$shortname <- factor(espressif_raw_lines_most_df$shortname,
                                                levels=names_for_plot)

most_set <- c("triangle", "gaps", "verylow")
espressif_raw_most_data_df <- bind_rows(lapply(names(names_for_plot),
                                                function(nm) { return(data.frame(adc_data[[nm]]$expanded_data_df %>%
                                                                                   subset(test_name %in% most_set),
                                                                                 shortname=names_for_plot[nm]) ) }))
espressif_raw_most_data_df$shortname <- factor(espressif_raw_most_data_df$shortname,
                                               levels=names_for_plot)


enob_list_text <- paste(c("Board                ENOB (t+g+vl) (bits)",
                          sapply(names(names_for_plot),
                                 function(nm) { return(sprintf("%-21s        %5s",
                                                               gsub("\n", " ",
                                                                    names_for_plot[nm]),
                                                               sprintf("%.2f", extra_calculated_data[[nm]]$stats$most_enob))) })),
                        collapse="\n")

rms_list_text <- paste(c("Board                 RMS Dist.   RMS Noise (mV)",
                         sapply(names(names_for_plot),
                                function(nm) { return(sprintf("%-21s      %5s        %5s     ",
                                                              gsub("\n", " ",
                                                                   names_for_plot[nm]),
                                                              sprintf("%.2f", extra_calculated_data[[nm]]$stats$most_d_rms_v * 1e3),
                                                              sprintf("%.2f", extra_calculated_data[[nm]]$stats$noise_n_rms_v * 1e3))) })),
                       collapse="\n")

### This is similar to plot a but shows best fit as a line
### experimenting with  stat_summary_2d() to show actual data as
### faint, slightly blocky background to plot
espressif_raw_g7c <- ggplot() +
  custom_theme + 
  ggtitle("Comparison of boards with Espressif MCUs",
          subtitle="Arduino AnalogRead(), battery powered") +
  stat_summary_2d(data=espressif_raw_most_data_df,
                  mapping=aes(x=adc_v,
                              y=but_actual_v,
                              z=1,
                              alpha=after_stat(value),
                              fill=shortname,
                               ),
                   fun=function(x) { return(as.factor(min(6, (sum(x) + 3) %/% 6))) },
                   binwidth=(plot_upper_v - plot_lower_v) / 200,
                   show.legend=FALSE
  ) +
  geom_line(espressif_raw_lines_most_df,
            mapping=aes(x=mean_adc_v,
                        y=voltage,
                        color=shortname,
                        group=shortname),
            size=0.8) +
  geom_label(data=data.frame(x=plot_lower_v, y=plot_upper_v, text=enob_list_text),
             mapping=aes(x=x, y=y, label=text),
             hjust=0, vjust=1,
             size=8,                
             family="mono", fontface="bold",
             alpha=0.5,
             inherit.aes=FALSE,
             show.legend=FALSE) +
  geom_label(data=data.frame(x=plot_upper_v, y=plot_lower_v, text=rms_list_text),
             mapping=aes(x=x, y=y, label=text),
             hjust=1, vjust=0,
             size=8,
             family="mono", fontface="bold",
             alpha=0.5,
             inherit.aes=FALSE,
             show.legend=FALSE) +
  labs(x="ADS1115 measurement (V)", y="BUT ADC actual (V)") +
  scale_alpha_manual(values=bg_alpha_values) + 
  guides(color=guide_legend(title="board",
                            override.aes=list(linewidth=5))) +
  coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                  ylim=c(plot_lower_v, plot_upper_v))

chart_filename <- sprintf("%s-espressifraw-g7c", myversion)
ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
       espressif_raw_g7c,
       dpi=100, height=15, width=20, units="in", limitsize = FALSE)




names_for_plot <- c("ESP32 DevKitC Clone : NiMH t1"="DevKitC Clone\nESP32",
                    "FeatherS2 : LiPo t1"="UM FeatherS2\nESP32-S2",
                    "Xiao ESP32C6 : NiMH t1"="SS Xiao ESP32C6",
                    "Xiao ESP32C5 : NiMH t1"="SS Xiao ESP32C5")

### Espressif calibrated together in g7c style
espressif_calib_lines_most_df <- bind_rows(lapply(names(names_for_plot),
                                                  function(nm) { return(data.frame(extra_calculated_data[[nm]]$lines_most_df %>%
                                                                                     subset(line == "iqrmean_but_actual_v"),
                                                                                   shortname=names_for_plot[nm]))}))
espressif_calib_lines_most_df$shortname <- factor(espressif_calib_lines_most_df$shortname,
                                                  levels=names_for_plot)

most_set <- c("triangle", "gaps", "verylow")
espressif_calib_most_data_df <- bind_rows(lapply(names(names_for_plot),
                                                 function(nm) { return(data.frame(adc_data[[nm]]$expanded_data_df %>%
                                                                                    subset(test_name %in% most_set),
                                                                                  shortname=names_for_plot[nm]) ) }))
espressif_calib_most_data_df$shortname <- factor(espressif_calib_most_data_df$shortname,
                                                 levels=names_for_plot)
  
enob_list_text <- paste(c("Board                ENOB (t+g+vl) (bits)",
                          sapply(names(names_for_plot),
                                 function(nm) { return(sprintf("%-21s        %5s",
                                                               gsub("\n", " ",
                                                                    names_for_plot[nm]),
                                                               sprintf("%.2f", extra_calculated_data[[nm]]$stats$most_enob))) })),
                        collapse="\n")

rms_list_text <- paste(c("Board                 RMS Dist.   RMS Noise (mV)",
                         sapply(names(names_for_plot),
                                function(nm) { return(sprintf("%-21s      %5s        %5s     ",
                                                              gsub("\n", " ",
                                                                   names_for_plot[nm]),
                                                              sprintf("%.2f", extra_calculated_data[[nm]]$stats$most_d_rms_v * 1e3),
                                                              sprintf("%.2f", extra_calculated_data[[nm]]$stats$noise_n_rms_v * 1e3))) })),
                       collapse="\n")

### This is similar to plot a but shows best fit as a line
### experimenting with  stat_summary_2d() to show actual data as
### faint, slightly blocky background to plot
espressif_calib_g7c <- ggplot() +
  custom_theme + 
  ggtitle("Comparison of boards with Espressif MCUs",
          subtitle="Arduino analogReadMilliVolts(), battery powered") +
  stat_summary_2d(data=espressif_calib_most_data_df,
                  mapping=aes(x=adc_v,
                              y=but_actual_v,
                              z=1,
                              alpha=after_stat(value),
                              fill=shortname,
                  ),
                  fun=function(x) { return(as.factor(min(6, (sum(x) + 3) %/% 6))) },
                  binwidth=(plot_upper_v - plot_lower_v) / 200,
                  show.legend=FALSE
  ) +
  geom_line(espressif_calib_lines_most_df,
            mapping=aes(x=mean_adc_v,
                        y=voltage,
                        color=shortname,
                        group=shortname),
            size=0.8) +
  geom_label(data=data.frame(x=plot_lower_v, y=plot_upper_v, text=enob_list_text),
             mapping=aes(x=x, y=y, label=text),
             hjust=0, vjust=1,
             size=8,                
             family="mono", fontface="bold",
             alpha=0.5,
             inherit.aes=FALSE,
             show.legend=FALSE) +
  geom_label(data=data.frame(x=plot_upper_v, y=plot_lower_v, text=rms_list_text),
             mapping=aes(x=x, y=y, label=text),
             hjust=1, vjust=0,
             size=8,
             family="mono", fontface="bold",
             alpha=0.5,
             inherit.aes=FALSE,
             show.legend=FALSE) +
  labs(x="ADS1115 measurement (V)", y="BUT ADC actual (V)") +
  scale_alpha_manual(values=bg_alpha_values) + 
  guides(color=guide_legend(title="board",
                            override.aes=list(linewidth=5))) +
  coord_cartesian(xlim=c(plot_lower_v, plot_upper_v),
                  ylim=c(plot_lower_v, plot_upper_v))

chart_filename <- sprintf("%s-espressifcalib-g7c", myversion)
ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
       espressif_calib_g7c,
       dpi=100, height=15, width=20, units="in", limitsize = FALSE)




### Simulated sine plot looking like g8a to aid discussion about
### conventional ENOB testing with
ssw_codesize <- 4096
if (ssw_codesize <= 8192) {
  cc_ac_breaks <- seq(0, ssw_codesize, by=128)
  cc_divisor <- 1024
  local_theme <- theme(strip.background=element_blank(),
                       strip.text=element_blank())
} else {
  ### For >= 14bit ADC use values for 8 facets
  cc_ac_breaks <- seq(0, ssw_codesize, by=256)
  cc_divisor <- 2048
  local_theme <- theme(strip.background=element_blank(),
                       strip.text=element_blank(),
                       # cram them in
                       axis.text.x = element_text(hjust=0.5, size=title_size/4),
                       panel.spacing.y = unit(0.4, "lines"))
}
samples <- 300 * 1000
cycles <- 37.01
synthsinewavedata_data_df <- data.frame(test_name="sin",
                                        adc_code=pmax(0,
                                                      pmin(ssw_codesize - 1,
                                                           round((sin(seq(0, samples - 1) * 2 * pi /
                                                                        (samples / cycles)) + 1) * (ssw_codesize / 2) - 0.5))))

synthsinewavedata_coverage_df <- synthsinewavedata_data_df %>%
  group_by(test_name) %>%
  reframe(count=table(factor(round(adc_code), levels=seq(0, ssw_codesize - 1))))

synthsinewavedata_c_c_df <- synthsinewavedata_coverage_df %>%
  mutate(code=as.integer(names(count)),
         code_lower=code %% cc_divisor,
         code_upper=factor(code %/% cc_divisor,
                           levels=seq(ssw_codesize %/% cc_divisor, 1) - 1)) %>%
  group_by(code) %>%
  mutate(total=sum(count)) %>%
  ungroup()




### This will spit a warning message "No shared levels found between `names(values)` of the manual scale and the data's colour values."
### for any ADCs with no absent codes
g8a <- ggplot(synthsinewavedata_c_c_df,
              aes(x=code,
                  y=as.integer(count),
                  fill=test_name)) +
  custom_theme + 
  local_theme +
  ggtitle(paste("Perfect sine wave", "code coverage"),
          subtitle=paste(sprintf("%.2f cycles over %d samples", cycles, samples),
                         collapse=", ")) +
  geom_bar(position="stack", stat="identity", width=1) +
  geom_point(data=synthsinewavedata_c_c_df %>%
               subset(total == 0) %>%
               group_by(code, code_lower, code_upper)
             %>% summarise(pos=0 - max(synthsinewavedata_c_c_df$total)/40),
             aes(x=code, y=pos, color="absent"),
             size=0.5,
             inherit.aes=FALSE) +
  labs(x="ADC code", y="count") +
  scale_x_continuous(breaks=cc_ac_breaks,
                     minor_breaks=NULL) +
  scale_y_continuous(minor_breaks=NULL) +
  scale_fill_discrete(name="test") + 
  scale_color_manual(name=NULL,
                     values=c("absent"="maroon3")) + 
  guides(fill=guide_legend(order=1),
         color=guide_legend(order=2, override.aes=list(size=5))) +
  ##coord_cartesian(ylim=c(0, bar_height)) +
  facet_wrap(vars(code_upper),
             ncol=1,
             dir="v",
             scale="free_x")

chart_filename <- sprintf("%s-sinewave-g8a", myversion)
ggsave(paste(chart_filebase, chart_filename, ".png", sep=""),
       g8a,
       dpi=100, height=15, width=20, units="in", limitsize = FALSE) 
