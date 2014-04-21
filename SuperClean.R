#22-March-2014
#Follow these instructions to clean behavioral data for Culture Seat Task


##############cvs##############


```{r ReadinCSV}
AEK0317 <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv")

###########csv#########################################################

#### Read in the CSV file
AEK0317 <- read.csv("/Volumes/NO NAME/Csvgroup3-27/Csvgroup3-27/CSEyeAEK0317.csv")


### create empty marker column
AEK0317$Markers <- NA

### check/count number of markers
table(AEK0317$DataType) ###should be 200 (w/ practice intro)

### shift 'Start' by one row down, store it in the new column
AEK0317$Markers [which(AEK0317$DataType == 'Start') + 1] <- 'Start'
### shift 'Stop' by one row up, store it in the new column
AEK0317$Markers [which(AEK0317$DataType == 'Stop') - 1] <- 'Stop'

### Save Csv to clean in excel
write.table(AEK0317,file="/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv",sep=",",row.names=F)

### Go into Excel. Sort/Delete Column "DataType". Create columns Delta, TimeMs, and TimeS

#### Read in the CSV file
AEK0317 <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv")

#### For CultureSEAT, markers must be moved .5 seconds based on how the task blocks are written

### Create new Marker column that will replace old Markercolumn
AEK0317$MarkersNew <- NA

### Start markers are placed in data 0.5 seconds before stimulus onset, so move it down 0.5s.
all.positionsStart <- sapply(which(AEK0317$Markers == "Start"), function(x) with(AEK0317, max(which(TimeS <= TimeS[[x]] + 0.5))))
AEK0317$MarkersNew[all.positionsStart] <- "Start"


### Stop markers are placed in data 0.5 seconds after stimulus offset, so move it up 0.5s.
all.positionsStop <- sapply(which(AEK0317$Markers == "Stop"), function(x) with (AEK0317, max(which(TimeS <= TimeS[[x]] - 0.5))))
AEK0317$MarkersNew[all.positionsStop] <- "Stop"

#######TEST######

AEK0317$MarkersNewNew <- NA
AEK0317$MarkersNewNew [which(AEK0317$MarkersNew == "Start")] <- "Start"

all.positionsStop <- sapply(which(AEK0317$MarkersNew == "Start"), function(x) with(AEK0317, max(which(TimeMs <= TimeMs[[x]] + 1500))))
AEK0317$MarkersNewNew[all.positionsStop] <- "Stop"

table(AEK0317$MarkersNewNew)
#########EndTest#########

### The next step is to fill the cells between the Starts and Stops in a way that allows us to sort by stimulus or block.
### To accomplish this, first sequentially number the Starts/Stops

### make a new column for stimlus numbering
AEK0317$MarkerNumber <- NA

### In new column$Marker, label by sequential number the start markers from column$DataType
AEK0317$MarkerNumber [which(AEK0317$MarkersNewNew == 'Start')] <- c(1:200)


### In new column$Marker, label by sequential number the stop markers from column$DataType
AEK0317$MarkerNumber [which(AEK0317$MarkersNewNew == 'Stop')] <- c(1:200)

#check that number is 200 twice
table(AEK0317$MarkerNumber)

###### Save Csv to clean in excel
### In excel, delete the rows of data before the first and after the last stimulus
write.table(AEK0317,file="/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv",sep=",",row.names=F)

#### Read in the CSV file
AEK0317 <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv")

### Now fill the space in between the numbers with their respective stimuli number
### Where (1, NA, NA, NA, 1) turns into (1, 1, 1, 1, 1)

library(zoo)

a <- na.approx(AEK0317$MarkerNumber)
la <- na.locf(AEK0317$MarkerNumber)
data1 <- transform(AEK0317, MarkerNumber = ifelse(a == la,  a, NA))


###### Save Csv 
write.table(data1,file="/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv",sep=",",row.names=F)

#### Read in the CSV file
AEK0317 <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv")

#This next step you are creating a new column that calculates in milliseconds from the stimulus onset to offset for each trial
#It's important to do this ste before deleting any rows that would throw off the calculation
AEK0317$group <- cumsum(AEK0317$MarkersNewNew=="Start" & !is.na(AEK0317$MarkersNewNew))
AEK0317$PupilaryLatencyAll <- unlist(aggregate(TimeMs~group,AEK0317,function(x)cumsum(c(0,diff(x))))$TimeMs)
AEK0317[,"group"] <- NULL


#alter pupil latency
AEK0317$attempt <- 2500
AEK0317 <- transform(AEK0317, PupilaryLatencyAll = ifelse(PupilaryLatencyAll>=2000 & AEK0317$PupilaryLatencyAll<=2500, PupilaryLatencyAll - attempt, PupilaryLatencyAll))
#####################



###Label dependent variables: TaskCue, ValenceType, FaceType
###Use Task csv file to check block order and task version

AEK0317$TaskCue <- NA
AEK0317$FaceType <- NA
AEK0317$ValenceType <- NA




### TaskCue
#practice
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=1 & AEK0317$MarkerNumber<=8)] <- "InOut"
###run1
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=9 & AEK0317$MarkerNumber<=16)] <- "InOut"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=17 & AEK0317$MarkerNumber<=24)] <- "LikeDislike"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=25 & AEK0317$MarkerNumber<=32)] <- "InOut"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=33 & AEK0317$MarkerNumber<=40)] <- "MaleFemale"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=41 & AEK0317$MarkerNumber<=48)] <- "LikeDislike"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=49 & AEK0317$MarkerNumber<=56)] <- "MaleFemale"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=57 & AEK0317$MarkerNumber<=64)] <- "LikeDislike"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=65 & AEK0317$MarkerNumber<=72)] <- "MaleFemale"

### run2
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=73 & AEK0317$MarkerNumber<=80)] <- "InOut"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=81 & AEK0317$MarkerNumber<=88)] <- "MaleFemale"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=89 & AEK0317$MarkerNumber<=96)] <- "InOut"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=97 & AEK0317$MarkerNumber<=104)] <- "MaleFemale"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=105 & AEK0317$MarkerNumber<=112)] <- "LikeDislike"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=113 & AEK0317$MarkerNumber<=120)] <- "MaleFemale"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=121 & AEK0317$MarkerNumber<=128)] <- "InOut"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=129 & AEK0317$MarkerNumber<=136)] <- "LikeDislike"

### run3

AEK0317$TaskCue [which(AEK0317$MarkerNumber>=137 & AEK0317$MarkerNumber<=144)] <- "InOut"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=145 & AEK0317$MarkerNumber<=152)] <- "LikeDislike"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=153 & AEK0317$MarkerNumber<=160)] <- "MaleFemale"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=161 & AEK0317$MarkerNumber<=168)] <- "InOut"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=169 & AEK0317$MarkerNumber<=176)] <- "MaleFemale"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=177 & AEK0317$MarkerNumber<=184)] <- "LikeDislike"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=185 & AEK0317$MarkerNumber<=192)] <- "InOut"
AEK0317$TaskCue [which(AEK0317$MarkerNumber>=193 & AEK0317$MarkerNumber<=200)] <- "LikeDislike"
### FaceType

#practice
AEK0317$FaceType [which(AEK0317$MarkerNumber>=1 & AEK0317$MarkerNumber<=8)] <- "Caucasion"
###run1
AEK0317$FaceType [which(AEK0317$MarkerNumber>=9 & AEK0317$MarkerNumber<=16)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=17 & AEK0317$MarkerNumber<=24)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=25 & AEK0317$MarkerNumber<=32)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=33 & AEK0317$MarkerNumber<=40)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=41 & AEK0317$MarkerNumber<=48)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=49 & AEK0317$MarkerNumber<=56)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=57 & AEK0317$MarkerNumber<=64)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=65 & AEK0317$MarkerNumber<=72)] <- "Asian"

### run2
AEK0317$FaceType [which(AEK0317$MarkerNumber>=73 & AEK0317$MarkerNumber<=80)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=81 & AEK0317$MarkerNumber<=88)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=89 & AEK0317$MarkerNumber<=96)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=97 & AEK0317$MarkerNumber<=104)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=105 & AEK0317$MarkerNumber<=112)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=113 & AEK0317$MarkerNumber<=120)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=121 & AEK0317$MarkerNumber<=128)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=129 & AEK0317$MarkerNumber<=136)] <- "Caucasion"

### run3


AEK0317$FaceType [which(AEK0317$MarkerNumber>=137 & AEK0317$MarkerNumber<=144)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=145 & AEK0317$MarkerNumber<=152)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=153 & AEK0317$MarkerNumber<=160)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=161 & AEK0317$MarkerNumber<=168)] <- "Caucasion"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=169 & AEK0317$MarkerNumber<=176)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=177 & AEK0317$MarkerNumber<=184)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=185 & AEK0317$MarkerNumber<=192)] <- "Asian"
AEK0317$FaceType [which(AEK0317$MarkerNumber>=193 & AEK0317$MarkerNumber<=200)] <- "Asian"

### ValenceType
#practice
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=1 & AEK0317$MarkerNumber<=8)] <- "Fear"
###run1
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=9 & AEK0317$MarkerNumber<=16)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=17 & AEK0317$MarkerNumber<=24)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=25 & AEK0317$MarkerNumber<=32)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=33 & AEK0317$MarkerNumber<=40)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=41 & AEK0317$MarkerNumber<=48)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=49 & AEK0317$MarkerNumber<=56)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=57 & AEK0317$MarkerNumber<=64)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=65 & AEK0317$MarkerNumber<=72)] <- "Neutral"

### run2
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=73 & AEK0317$MarkerNumber<=80)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=81 & AEK0317$MarkerNumber<=88)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=89 & AEK0317$MarkerNumber<=96)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=97 & AEK0317$MarkerNumber<=104)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=105 & AEK0317$MarkerNumber<=112)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=113 & AEK0317$MarkerNumber<=120)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=121 & AEK0317$MarkerNumber<=128)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=129 & AEK0317$MarkerNumber<=136)] <- "Neutral"

### run3
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=137 & AEK0317$MarkerNumber<=144)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=145 & AEK0317$MarkerNumber<=152)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=153 & AEK0317$MarkerNumber<=160)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=161 & AEK0317$MarkerNumber<=168)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=169 & AEK0317$MarkerNumber<=176)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=177 & AEK0317$MarkerNumber<=184)] <- "Fear"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=185 & AEK0317$MarkerNumber<=192)] <- "Neutral"
AEK0317$ValenceType [which(AEK0317$MarkerNumber>=193 & AEK0317$MarkerNumber<=200)] <- "Fear"
#don't save yet


#Label fixations
#Found it necessary to calculuate based on dependent variable to see pertinent trends
#Redundent, look for cleaner way

outVec <- na.approx(AEK0317$MarkerNumber, na.rm=F)
AEK0317$MarkerNumber <- outVec


#POST INOUT FIXATION
AEK0317$TaskCue [which(AEK0317$MarkerNumber>1 & AEK0317$MarkerNumber<2 | AEK0317$MarkerNumber>2 & AEK0317$MarkerNumber<3 | AEK0317$MarkerNumber>3 & AEK0317$MarkerNumber<4 | AEK0317$MarkerNumber>4 & AEK0317$MarkerNumber<5 | AEK0317$MarkerNumber>5 & AEK0317$MarkerNumber<6 | AEK0317$MarkerNumber>6 & AEK0317$MarkerNumber<7 | AEK0317$MarkerNumber>7 & AEK0317$MarkerNumber<8)] <- "PostInOutFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>9 & AEK0317$MarkerNumber<10 | AEK0317$MarkerNumber>10 & AEK0317$MarkerNumber<11 | AEK0317$MarkerNumber>11 & AEK0317$MarkerNumber<12 | AEK0317$MarkerNumber>12 & AEK0317$MarkerNumber<13 | AEK0317$MarkerNumber>13 & AEK0317$MarkerNumber<14 | AEK0317$MarkerNumber>14 & AEK0317$MarkerNumber<15 | AEK0317$MarkerNumber>15 & AEK0317$MarkerNumber<16)] <- "PostInOutFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>25 & AEK0317$MarkerNumber<26 | AEK0317$MarkerNumber>26 & AEK0317$MarkerNumber<27 | AEK0317$MarkerNumber>27 & AEK0317$MarkerNumber<28 | AEK0317$MarkerNumber>28 & AEK0317$MarkerNumber<29 | AEK0317$MarkerNumber>29 & AEK0317$MarkerNumber<30 | AEK0317$MarkerNumber>30 & AEK0317$MarkerNumber<31 | AEK0317$MarkerNumber>31 & AEK0317$MarkerNumber<32)] <- "PostInOutFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>73 & AEK0317$MarkerNumber<74 | AEK0317$MarkerNumber>74 & AEK0317$MarkerNumber<75 | AEK0317$MarkerNumber>75 & AEK0317$MarkerNumber<76 | AEK0317$MarkerNumber>76 & AEK0317$MarkerNumber<77 | AEK0317$MarkerNumber>77 & AEK0317$MarkerNumber<78 | AEK0317$MarkerNumber>78 & AEK0317$MarkerNumber<79 | AEK0317$MarkerNumber>79 & AEK0317$MarkerNumber<80)] <- "PostInOutFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>89 & AEK0317$MarkerNumber<90 | AEK0317$MarkerNumber>90 & AEK0317$MarkerNumber<91 | AEK0317$MarkerNumber>91 & AEK0317$MarkerNumber<92 | AEK0317$MarkerNumber>92 & AEK0317$MarkerNumber<93 | AEK0317$MarkerNumber>93 & AEK0317$MarkerNumber<94 | AEK0317$MarkerNumber>94 & AEK0317$MarkerNumber<95 | AEK0317$MarkerNumber>95 & AEK0317$MarkerNumber<96)] <- "PostInOutFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>121 & AEK0317$MarkerNumber<122 | AEK0317$MarkerNumber>122 & AEK0317$MarkerNumber<123 | AEK0317$MarkerNumber>123 & AEK0317$MarkerNumber<124 | AEK0317$MarkerNumber>124 & AEK0317$MarkerNumber<125 | AEK0317$MarkerNumber>125 & AEK0317$MarkerNumber<126 | AEK0317$MarkerNumber>126 & AEK0317$MarkerNumber<127 | AEK0317$MarkerNumber>127 & AEK0317$MarkerNumber<128)] <- "PostInOutFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>137 & AEK0317$MarkerNumber<138 | AEK0317$MarkerNumber>138 & AEK0317$MarkerNumber<139 | AEK0317$MarkerNumber>139 & AEK0317$MarkerNumber<140 | AEK0317$MarkerNumber>140 & AEK0317$MarkerNumber<141 | AEK0317$MarkerNumber>141 & AEK0317$MarkerNumber<142 | AEK0317$MarkerNumber>142 & AEK0317$MarkerNumber<143 | AEK0317$MarkerNumber>143 & AEK0317$MarkerNumber<144)] <- "PostInOutFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>161 & AEK0317$MarkerNumber<162 | AEK0317$MarkerNumber>162 & AEK0317$MarkerNumber<163 | AEK0317$MarkerNumber>163 & AEK0317$MarkerNumber<164 | AEK0317$MarkerNumber>164 & AEK0317$MarkerNumber<165 | AEK0317$MarkerNumber>165 & AEK0317$MarkerNumber<166 | AEK0317$MarkerNumber>166 & AEK0317$MarkerNumber<167 | AEK0317$MarkerNumber>167 & AEK0317$MarkerNumber<168)] <- "PostInOutFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>185 & AEK0317$MarkerNumber<186 | AEK0317$MarkerNumber>186 & AEK0317$MarkerNumber<187 | AEK0317$MarkerNumber>187 & AEK0317$MarkerNumber<188 | AEK0317$MarkerNumber>188 & AEK0317$MarkerNumber<189 | AEK0317$MarkerNumber>189 & AEK0317$MarkerNumber<190 | AEK0317$MarkerNumber>190 & AEK0317$MarkerNumber<191 | AEK0317$MarkerNumber>191 & AEK0317$MarkerNumber<192)] <- 'PostInOutFixation'

#POST LIKEDISLIKE FIXATION
AEK0317$TaskCue [which(AEK0317$MarkerNumber>17 & AEK0317$MarkerNumber<18 | AEK0317$MarkerNumber>18 & AEK0317$MarkerNumber<19 | AEK0317$MarkerNumber>19 & AEK0317$MarkerNumber<20 | AEK0317$MarkerNumber>20 & AEK0317$MarkerNumber<21 | AEK0317$MarkerNumber>21 & AEK0317$MarkerNumber<22 | AEK0317$MarkerNumber>22 & AEK0317$MarkerNumber<23 | AEK0317$MarkerNumber>23 & AEK0317$MarkerNumber<24)] <- 'PostLikeDislikeFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>41 & AEK0317$MarkerNumber<42 | AEK0317$MarkerNumber>42 & AEK0317$MarkerNumber<43 | AEK0317$MarkerNumber>43 & AEK0317$MarkerNumber<44 | AEK0317$MarkerNumber>44 & AEK0317$MarkerNumber<45 | AEK0317$MarkerNumber>45 & AEK0317$MarkerNumber<46 | AEK0317$MarkerNumber>46 & AEK0317$MarkerNumber<47 | AEK0317$MarkerNumber>47 & AEK0317$MarkerNumber<48)] <- 'PostLikeDislikeFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>57 & AEK0317$MarkerNumber<58 | AEK0317$MarkerNumber>58 & AEK0317$MarkerNumber<59 | AEK0317$MarkerNumber>59 & AEK0317$MarkerNumber<60 | AEK0317$MarkerNumber>60 & AEK0317$MarkerNumber<61 | AEK0317$MarkerNumber>61 & AEK0317$MarkerNumber<62 | AEK0317$MarkerNumber>62 & AEK0317$MarkerNumber<63 | AEK0317$MarkerNumber>63 & AEK0317$MarkerNumber<64)] <- 'PostLikeDislikeFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>105 & AEK0317$MarkerNumber<106 | AEK0317$MarkerNumber>106 & AEK0317$MarkerNumber<107 | AEK0317$MarkerNumber>107 & AEK0317$MarkerNumber<108 | AEK0317$MarkerNumber>108 & AEK0317$MarkerNumber<109 | AEK0317$MarkerNumber>109 & AEK0317$MarkerNumber<110 | AEK0317$MarkerNumber>110 & AEK0317$MarkerNumber<111 | AEK0317$MarkerNumber>111 & AEK0317$MarkerNumber<112)] <- 'PostLikeDislikeFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>129 & AEK0317$MarkerNumber<130 | AEK0317$MarkerNumber>130 & AEK0317$MarkerNumber<131 | AEK0317$MarkerNumber>131 & AEK0317$MarkerNumber<132 | AEK0317$MarkerNumber>132 & AEK0317$MarkerNumber<133 | AEK0317$MarkerNumber>133 & AEK0317$MarkerNumber<134 | AEK0317$MarkerNumber>134 & AEK0317$MarkerNumber<135 | AEK0317$MarkerNumber>135 & AEK0317$MarkerNumber<136)] <- 'PostLikeDislikeFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>145 & AEK0317$MarkerNumber<146 | AEK0317$MarkerNumber>146 & AEK0317$MarkerNumber<147 | AEK0317$MarkerNumber>147 & AEK0317$MarkerNumber<148 | AEK0317$MarkerNumber>148 & AEK0317$MarkerNumber<149 | AEK0317$MarkerNumber>149 & AEK0317$MarkerNumber<150 | AEK0317$MarkerNumber>150 & AEK0317$MarkerNumber<151 | AEK0317$MarkerNumber>151 & AEK0317$MarkerNumber<152)] <- 'PostLikeDislikeFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>177 & AEK0317$MarkerNumber<178 | AEK0317$MarkerNumber>178 & AEK0317$MarkerNumber<179 | AEK0317$MarkerNumber>179 & AEK0317$MarkerNumber<180 | AEK0317$MarkerNumber>180 & AEK0317$MarkerNumber<181 | AEK0317$MarkerNumber>181 & AEK0317$MarkerNumber<182 | AEK0317$MarkerNumber>182 & AEK0317$MarkerNumber<183 | AEK0317$MarkerNumber>183 & AEK0317$MarkerNumber<184)] <- 'PostLikeDislikeFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>193 & AEK0317$MarkerNumber<194 | AEK0317$MarkerNumber>194 & AEK0317$MarkerNumber<195 | AEK0317$MarkerNumber>195 & AEK0317$MarkerNumber<196 | AEK0317$MarkerNumber>196 & AEK0317$MarkerNumber<197 | AEK0317$MarkerNumber>197 & AEK0317$MarkerNumber<198 | AEK0317$MarkerNumber>198 & AEK0317$MarkerNumber<199 | AEK0317$MarkerNumber>200 & AEK0317$MarkerNumber<200)] <- 'PostLikeDislikeFixation'


#POST MALEFEMALE FIXATION
AEK0317$TaskCue [which(AEK0317$MarkerNumber>33 & AEK0317$MarkerNumber<34 | AEK0317$MarkerNumber>34 & AEK0317$MarkerNumber<35 | AEK0317$MarkerNumber>35 & AEK0317$MarkerNumber<36 | AEK0317$MarkerNumber>36 & AEK0317$MarkerNumber<37 | AEK0317$MarkerNumber>37 & AEK0317$MarkerNumber<38 | AEK0317$MarkerNumber>38 & AEK0317$MarkerNumber<39 | AEK0317$MarkerNumber>39 & AEK0317$MarkerNumber<40)] <- 'PostMaleFemaleFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>49 & AEK0317$MarkerNumber<50 | AEK0317$MarkerNumber>50 & AEK0317$MarkerNumber<51 | AEK0317$MarkerNumber>51 & AEK0317$MarkerNumber<52 | AEK0317$MarkerNumber>52 & AEK0317$MarkerNumber<53 | AEK0317$MarkerNumber>53 & AEK0317$MarkerNumber<54 | AEK0317$MarkerNumber>54 & AEK0317$MarkerNumber<55 | AEK0317$MarkerNumber>55 & AEK0317$MarkerNumber<56)] <- 'PostMaleFemaleFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>65 & AEK0317$MarkerNumber<66 | AEK0317$MarkerNumber>66 & AEK0317$MarkerNumber<67 | AEK0317$MarkerNumber>67 & AEK0317$MarkerNumber<68 | AEK0317$MarkerNumber>68 & AEK0317$MarkerNumber<69 | AEK0317$MarkerNumber>69 & AEK0317$MarkerNumber<70 | AEK0317$MarkerNumber>70 & AEK0317$MarkerNumber<71 | AEK0317$MarkerNumber>71 & AEK0317$MarkerNumber<72)] <- 'PostMaleFemaleFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>81 & AEK0317$MarkerNumber<82 | AEK0317$MarkerNumber>82 & AEK0317$MarkerNumber<83 | AEK0317$MarkerNumber>83 & AEK0317$MarkerNumber<84 | AEK0317$MarkerNumber>84 & AEK0317$MarkerNumber<85 | AEK0317$MarkerNumber>85 & AEK0317$MarkerNumber<86 | AEK0317$MarkerNumber>86 & AEK0317$MarkerNumber<87 | AEK0317$MarkerNumber>87 & AEK0317$MarkerNumber<88)] <- 'PostMaleFemaleFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>97 & AEK0317$MarkerNumber<98 | AEK0317$MarkerNumber>98 & AEK0317$MarkerNumber<99 | AEK0317$MarkerNumber>99 & AEK0317$MarkerNumber<100 | AEK0317$MarkerNumber>100 & AEK0317$MarkerNumber<101 | AEK0317$MarkerNumber>101 & AEK0317$MarkerNumber<102 | AEK0317$MarkerNumber>102 & AEK0317$MarkerNumber<103 | AEK0317$MarkerNumber>103 & AEK0317$MarkerNumber<104)] <- 'PostMaleFemaleFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>113 & AEK0317$MarkerNumber<114 | AEK0317$MarkerNumber>114 & AEK0317$MarkerNumber<115 | AEK0317$MarkerNumber>115 & AEK0317$MarkerNumber<116 | AEK0317$MarkerNumber>116 & AEK0317$MarkerNumber<117 | AEK0317$MarkerNumber>117 & AEK0317$MarkerNumber<118 | AEK0317$MarkerNumber>118 & AEK0317$MarkerNumber<119 | AEK0317$MarkerNumber>119 & AEK0317$MarkerNumber<120)] <- 'PostMaleFemaleFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>153 & AEK0317$MarkerNumber<154 | AEK0317$MarkerNumber>154 & AEK0317$MarkerNumber<155 | AEK0317$MarkerNumber>155 & AEK0317$MarkerNumber<156 | AEK0317$MarkerNumber>156 & AEK0317$MarkerNumber<157 | AEK0317$MarkerNumber>157 & AEK0317$MarkerNumber<158 | AEK0317$MarkerNumber>158 & AEK0317$MarkerNumber<159 | AEK0317$MarkerNumber>159 & AEK0317$MarkerNumber<160)] <- 'PostMaleFemaleFixation'

AEK0317$TaskCue [which(AEK0317$MarkerNumber>169 & AEK0317$MarkerNumber<170 | AEK0317$MarkerNumber>170 & AEK0317$MarkerNumber<171 | AEK0317$MarkerNumber>171 & AEK0317$MarkerNumber<172 | AEK0317$MarkerNumber>172 & AEK0317$MarkerNumber<173 | AEK0317$MarkerNumber>173 & AEK0317$MarkerNumber<174 | AEK0317$MarkerNumber>174 & AEK0317$MarkerNumber<175 | AEK0317$MarkerNumber>175 & AEK0317$MarkerNumber<176)] <- 'PostMaleFemaleFixation'



#POST FEAR FIXATION

AEK0317$ValenceType [which(AEK0317$MarkerNumber>1 & AEK0317$MarkerNumber<2 | AEK0317$MarkerNumber>2 & AEK0317$MarkerNumber<3 | AEK0317$MarkerNumber>3 & AEK0317$MarkerNumber<4 | AEK0317$MarkerNumber>4 & AEK0317$MarkerNumber<5 | AEK0317$MarkerNumber>5 & AEK0317$MarkerNumber<6 | AEK0317$MarkerNumber>6 & AEK0317$MarkerNumber<7 | AEK0317$MarkerNumber>7 & AEK0317$MarkerNumber<8)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>9 & AEK0317$MarkerNumber<10 | AEK0317$MarkerNumber>10 & AEK0317$MarkerNumber<11 | AEK0317$MarkerNumber>11 & AEK0317$MarkerNumber<12 | AEK0317$MarkerNumber>12 & AEK0317$MarkerNumber<13 | AEK0317$MarkerNumber>13 & AEK0317$MarkerNumber<14 | AEK0317$MarkerNumber>14 & AEK0317$MarkerNumber<15 | AEK0317$MarkerNumber>15 & AEK0317$MarkerNumber<16)] <- 'PostFearFixation'


AEK0317$ValenceType [which(AEK0317$MarkerNumber>33 & AEK0317$MarkerNumber<34 | AEK0317$MarkerNumber>34 & AEK0317$MarkerNumber<35 | AEK0317$MarkerNumber>35 & AEK0317$MarkerNumber<36 | AEK0317$MarkerNumber>36 & AEK0317$MarkerNumber<37 | AEK0317$MarkerNumber>37 & AEK0317$MarkerNumber<38 | AEK0317$MarkerNumber>38 & AEK0317$MarkerNumber<39 | AEK0317$MarkerNumber>39 & AEK0317$MarkerNumber<40)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>41 & AEK0317$MarkerNumber<42 | AEK0317$MarkerNumber>42 & AEK0317$MarkerNumber<43 | AEK0317$MarkerNumber>43 & AEK0317$MarkerNumber<44 | AEK0317$MarkerNumber>44 & AEK0317$MarkerNumber<45 | AEK0317$MarkerNumber>45 & AEK0317$MarkerNumber<46 | AEK0317$MarkerNumber>46 & AEK0317$MarkerNumber<47 | AEK0317$MarkerNumber>47 & AEK0317$MarkerNumber<48)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>73 & AEK0317$MarkerNumber<74 | AEK0317$MarkerNumber>74 & AEK0317$MarkerNumber<75 | AEK0317$MarkerNumber>75 & AEK0317$MarkerNumber<76 | AEK0317$MarkerNumber>76 & AEK0317$MarkerNumber<77 | AEK0317$MarkerNumber>77 & AEK0317$MarkerNumber<78 | AEK0317$MarkerNumber>78 & AEK0317$MarkerNumber<79 | AEK0317$MarkerNumber>79 & AEK0317$MarkerNumber<80)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>97 & AEK0317$MarkerNumber<98 | AEK0317$MarkerNumber>98 & AEK0317$MarkerNumber<99 | AEK0317$MarkerNumber>99 & AEK0317$MarkerNumber<100 | AEK0317$MarkerNumber>100 & AEK0317$MarkerNumber<101 | AEK0317$MarkerNumber>101 & AEK0317$MarkerNumber<102 | AEK0317$MarkerNumber>102 & AEK0317$MarkerNumber<103 | AEK0317$MarkerNumber>103 & AEK0317$MarkerNumber<104)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>113 & AEK0317$MarkerNumber<114 | AEK0317$MarkerNumber>114 & AEK0317$MarkerNumber<115 | AEK0317$MarkerNumber>115 & AEK0317$MarkerNumber<116 | AEK0317$MarkerNumber>116 & AEK0317$MarkerNumber<117 | AEK0317$MarkerNumber>117 & AEK0317$MarkerNumber<118 | AEK0317$MarkerNumber>118 & AEK0317$MarkerNumber<119 | AEK0317$MarkerNumber>119 & AEK0317$MarkerNumber<120)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>121 & AEK0317$MarkerNumber<122 | AEK0317$MarkerNumber>122 & AEK0317$MarkerNumber<123 | AEK0317$MarkerNumber>123 & AEK0317$MarkerNumber<124 | AEK0317$MarkerNumber>124 & AEK0317$MarkerNumber<125 | AEK0317$MarkerNumber>125 & AEK0317$MarkerNumber<126 | AEK0317$MarkerNumber>126 & AEK0317$MarkerNumber<127 | AEK0317$MarkerNumber>127 & AEK0317$MarkerNumber<128)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>145 & AEK0317$MarkerNumber<146 | AEK0317$MarkerNumber>146 & AEK0317$MarkerNumber<147 | AEK0317$MarkerNumber>147 & AEK0317$MarkerNumber<148 | AEK0317$MarkerNumber>148 & AEK0317$MarkerNumber<149 | AEK0317$MarkerNumber>149 & AEK0317$MarkerNumber<150 | AEK0317$MarkerNumber>150 & AEK0317$MarkerNumber<151 | AEK0317$MarkerNumber>151 & AEK0317$MarkerNumber<152)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>153 & AEK0317$MarkerNumber<154 | AEK0317$MarkerNumber>154 & AEK0317$MarkerNumber<155 | AEK0317$MarkerNumber>155 & AEK0317$MarkerNumber<156 | AEK0317$MarkerNumber>156 & AEK0317$MarkerNumber<157 | AEK0317$MarkerNumber>157 & AEK0317$MarkerNumber<158 | AEK0317$MarkerNumber>158 & AEK0317$MarkerNumber<159 | AEK0317$MarkerNumber>159 & AEK0317$MarkerNumber<160)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>161 & AEK0317$MarkerNumber<162 | AEK0317$MarkerNumber>162 & AEK0317$MarkerNumber<163 | AEK0317$MarkerNumber>163 & AEK0317$MarkerNumber<164 | AEK0317$MarkerNumber>164 & AEK0317$MarkerNumber<165 | AEK0317$MarkerNumber>165 & AEK0317$MarkerNumber<166 | AEK0317$MarkerNumber>166 & AEK0317$MarkerNumber<167 | AEK0317$MarkerNumber>167 & AEK0317$MarkerNumber<168)] <- "PostFearFixation"

AEK0317$ValenceType [which(AEK0317$MarkerNumber>177 & AEK0317$MarkerNumber<178 | AEK0317$MarkerNumber>178 & AEK0317$MarkerNumber<179 | AEK0317$MarkerNumber>179 & AEK0317$MarkerNumber<180 | AEK0317$MarkerNumber>180 & AEK0317$MarkerNumber<181 | AEK0317$MarkerNumber>181 & AEK0317$MarkerNumber<182 | AEK0317$MarkerNumber>182 & AEK0317$MarkerNumber<183 | AEK0317$MarkerNumber>183 & AEK0317$MarkerNumber<184)] <- 'PostFearFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>193 & AEK0317$MarkerNumber<194 | AEK0317$MarkerNumber>194 & AEK0317$MarkerNumber<195 | AEK0317$MarkerNumber>195 & AEK0317$MarkerNumber<196 | AEK0317$MarkerNumber>196 & AEK0317$MarkerNumber<197 | AEK0317$MarkerNumber>197 & AEK0317$MarkerNumber<198 | AEK0317$MarkerNumber>198 & AEK0317$MarkerNumber<199 | AEK0317$MarkerNumber>200 & AEK0317$MarkerNumber<200)] <- 'PostFearFixation'

#POST NEUTRAL FIXATION
AEK0317$ValenceType [which(AEK0317$MarkerNumber>17 & AEK0317$MarkerNumber<18 | AEK0317$MarkerNumber>18 & AEK0317$MarkerNumber<19 | AEK0317$MarkerNumber>19 & AEK0317$MarkerNumber<20 | AEK0317$MarkerNumber>20 & AEK0317$MarkerNumber<21 | AEK0317$MarkerNumber>21 & AEK0317$MarkerNumber<22 | AEK0317$MarkerNumber>22 & AEK0317$MarkerNumber<23 | AEK0317$MarkerNumber>23 & AEK0317$MarkerNumber<24)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>25 & AEK0317$MarkerNumber<26 | AEK0317$MarkerNumber>26 & AEK0317$MarkerNumber<27 | AEK0317$MarkerNumber>27 & AEK0317$MarkerNumber<28 | AEK0317$MarkerNumber>28 & AEK0317$MarkerNumber<29 | AEK0317$MarkerNumber>29 & AEK0317$MarkerNumber<30 | AEK0317$MarkerNumber>30 & AEK0317$MarkerNumber<31 | AEK0317$MarkerNumber>31 & AEK0317$MarkerNumber<32)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>49 & AEK0317$MarkerNumber<50 | AEK0317$MarkerNumber>50 & AEK0317$MarkerNumber<51 | AEK0317$MarkerNumber>51 & AEK0317$MarkerNumber<52 | AEK0317$MarkerNumber>52 & AEK0317$MarkerNumber<53 | AEK0317$MarkerNumber>53 & AEK0317$MarkerNumber<54 | AEK0317$MarkerNumber>54 & AEK0317$MarkerNumber<55 | AEK0317$MarkerNumber>55 & AEK0317$MarkerNumber<56)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>57 & AEK0317$MarkerNumber<58 | AEK0317$MarkerNumber>58 & AEK0317$MarkerNumber<59 | AEK0317$MarkerNumber>59 & AEK0317$MarkerNumber<60 | AEK0317$MarkerNumber>60 & AEK0317$MarkerNumber<61 | AEK0317$MarkerNumber>61 & AEK0317$MarkerNumber<62 | AEK0317$MarkerNumber>62 & AEK0317$MarkerNumber<63 | AEK0317$MarkerNumber>63 & AEK0317$MarkerNumber<64)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>65 & AEK0317$MarkerNumber<66 | AEK0317$MarkerNumber>66 & AEK0317$MarkerNumber<67 | AEK0317$MarkerNumber>67 & AEK0317$MarkerNumber<68 | AEK0317$MarkerNumber>68 & AEK0317$MarkerNumber<69 | AEK0317$MarkerNumber>69 & AEK0317$MarkerNumber<70 | AEK0317$MarkerNumber>70 & AEK0317$MarkerNumber<71 | AEK0317$MarkerNumber>71 & AEK0317$MarkerNumber<72)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>81 & AEK0317$MarkerNumber<82 | AEK0317$MarkerNumber>82 & AEK0317$MarkerNumber<83 | AEK0317$MarkerNumber>83 & AEK0317$MarkerNumber<84 | AEK0317$MarkerNumber>84 & AEK0317$MarkerNumber<85 | AEK0317$MarkerNumber>85 & AEK0317$MarkerNumber<86 | AEK0317$MarkerNumber>86 & AEK0317$MarkerNumber<87 | AEK0317$MarkerNumber>87 & AEK0317$MarkerNumber<88)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>89 & AEK0317$MarkerNumber<90 | AEK0317$MarkerNumber>90 & AEK0317$MarkerNumber<91 | AEK0317$MarkerNumber>91 & AEK0317$MarkerNumber<92 | AEK0317$MarkerNumber>92 & AEK0317$MarkerNumber<93 | AEK0317$MarkerNumber>93 & AEK0317$MarkerNumber<94 | AEK0317$MarkerNumber>94 & AEK0317$MarkerNumber<95 | AEK0317$MarkerNumber>95 & AEK0317$MarkerNumber<96)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>105 & AEK0317$MarkerNumber<106 | AEK0317$MarkerNumber>106 & AEK0317$MarkerNumber<107 | AEK0317$MarkerNumber>107 & AEK0317$MarkerNumber<108 | AEK0317$MarkerNumber>108 & AEK0317$MarkerNumber<109 | AEK0317$MarkerNumber>109 & AEK0317$MarkerNumber<110 | AEK0317$MarkerNumber>110 & AEK0317$MarkerNumber<111 | AEK0317$MarkerNumber>111 & AEK0317$MarkerNumber<112)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>129 & AEK0317$MarkerNumber<130 | AEK0317$MarkerNumber>130 & AEK0317$MarkerNumber<131 | AEK0317$MarkerNumber>131 & AEK0317$MarkerNumber<132 | AEK0317$MarkerNumber>132 & AEK0317$MarkerNumber<133 | AEK0317$MarkerNumber>133 & AEK0317$MarkerNumber<134 | AEK0317$MarkerNumber>134 & AEK0317$MarkerNumber<135 | AEK0317$MarkerNumber>135 & AEK0317$MarkerNumber<136)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>137 & AEK0317$MarkerNumber<138 | AEK0317$MarkerNumber>138 & AEK0317$MarkerNumber<139 | AEK0317$MarkerNumber>139 & AEK0317$MarkerNumber<140 | AEK0317$MarkerNumber>140 & AEK0317$MarkerNumber<141 | AEK0317$MarkerNumber>141 & AEK0317$MarkerNumber<142 | AEK0317$MarkerNumber>142 & AEK0317$MarkerNumber<143 | AEK0317$MarkerNumber>143 & AEK0317$MarkerNumber<144)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>169 & AEK0317$MarkerNumber<170 | AEK0317$MarkerNumber>170 & AEK0317$MarkerNumber<171 | AEK0317$MarkerNumber>171 & AEK0317$MarkerNumber<172 | AEK0317$MarkerNumber>172 & AEK0317$MarkerNumber<173 | AEK0317$MarkerNumber>173 & AEK0317$MarkerNumber<174 | AEK0317$MarkerNumber>174 & AEK0317$MarkerNumber<175 | AEK0317$MarkerNumber>175 & AEK0317$MarkerNumber<176)] <- 'PostNeutralFixation'

AEK0317$ValenceType [which(AEK0317$MarkerNumber>185 & AEK0317$MarkerNumber<186 | AEK0317$MarkerNumber>186 & AEK0317$MarkerNumber<187 | AEK0317$MarkerNumber>187 & AEK0317$MarkerNumber<188 | AEK0317$MarkerNumber>188 & AEK0317$MarkerNumber<189 | AEK0317$MarkerNumber>189 & AEK0317$MarkerNumber<190 | AEK0317$MarkerNumber>190 & AEK0317$MarkerNumber<191 | AEK0317$MarkerNumber>191 & AEK0317$MarkerNumber<192)] <- 'PostNeutralFixation'

#POST ASIAN FIXATION
AEK0317$FaceType [which(AEK0317$MarkerNumber>9 & AEK0317$MarkerNumber<10 | AEK0317$MarkerNumber>10 & AEK0317$MarkerNumber<11 | AEK0317$MarkerNumber>11 & AEK0317$MarkerNumber<12 | AEK0317$MarkerNumber>12 & AEK0317$MarkerNumber<13 | AEK0317$MarkerNumber>13 & AEK0317$MarkerNumber<14 | AEK0317$MarkerNumber>14 & AEK0317$MarkerNumber<15 | AEK0317$MarkerNumber>15 & AEK0317$MarkerNumber<16)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>17 & AEK0317$MarkerNumber<18 | AEK0317$MarkerNumber>18 & AEK0317$MarkerNumber<19 | AEK0317$MarkerNumber>19 & AEK0317$MarkerNumber<20 | AEK0317$MarkerNumber>20 & AEK0317$MarkerNumber<21 | AEK0317$MarkerNumber>21 & AEK0317$MarkerNumber<22 | AEK0317$MarkerNumber>22 & AEK0317$MarkerNumber<23 | AEK0317$MarkerNumber>23 & AEK0317$MarkerNumber<24)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>41 & AEK0317$MarkerNumber<42 | AEK0317$MarkerNumber>42 & AEK0317$MarkerNumber<43 | AEK0317$MarkerNumber>43 & AEK0317$MarkerNumber<44 | AEK0317$MarkerNumber>44 & AEK0317$MarkerNumber<45 | AEK0317$MarkerNumber>45 & AEK0317$MarkerNumber<46 | AEK0317$MarkerNumber>46 & AEK0317$MarkerNumber<47 | AEK0317$MarkerNumber>47 & AEK0317$MarkerNumber<48)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>65 & AEK0317$MarkerNumber<66 | AEK0317$MarkerNumber>66 & AEK0317$MarkerNumber<67 | AEK0317$MarkerNumber>67 & AEK0317$MarkerNumber<68 | AEK0317$MarkerNumber>68 & AEK0317$MarkerNumber<69 | AEK0317$MarkerNumber>69 & AEK0317$MarkerNumber<70 | AEK0317$MarkerNumber>70 & AEK0317$MarkerNumber<71 | AEK0317$MarkerNumber>71 & AEK0317$MarkerNumber<72)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>89 & AEK0317$MarkerNumber<90 | AEK0317$MarkerNumber>90 & AEK0317$MarkerNumber<91 | AEK0317$MarkerNumber>91 & AEK0317$MarkerNumber<92 | AEK0317$MarkerNumber>92 & AEK0317$MarkerNumber<93 | AEK0317$MarkerNumber>93 & AEK0317$MarkerNumber<94 | AEK0317$MarkerNumber>94 & AEK0317$MarkerNumber<95 | AEK0317$MarkerNumber>95 & AEK0317$MarkerNumber<96)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>97 & AEK0317$MarkerNumber<98 | AEK0317$MarkerNumber>98 & AEK0317$MarkerNumber<99 | AEK0317$MarkerNumber>99 & AEK0317$MarkerNumber<100 | AEK0317$MarkerNumber>100 & AEK0317$MarkerNumber<101 | AEK0317$MarkerNumber>101 & AEK0317$MarkerNumber<102 | AEK0317$MarkerNumber>102 & AEK0317$MarkerNumber<103 | AEK0317$MarkerNumber>103 & AEK0317$MarkerNumber<104)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>105 & AEK0317$MarkerNumber<106 | AEK0317$MarkerNumber>106 & AEK0317$MarkerNumber<107 | AEK0317$MarkerNumber>107 & AEK0317$MarkerNumber<108 | AEK0317$MarkerNumber>108 & AEK0317$MarkerNumber<109 | AEK0317$MarkerNumber>109 & AEK0317$MarkerNumber<110 | AEK0317$MarkerNumber>110 & AEK0317$MarkerNumber<111 | AEK0317$MarkerNumber>111 & AEK0317$MarkerNumber<112)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>121 & AEK0317$MarkerNumber<122 | AEK0317$MarkerNumber>122 & AEK0317$MarkerNumber<123 | AEK0317$MarkerNumber>123 & AEK0317$MarkerNumber<124 | AEK0317$MarkerNumber>124 & AEK0317$MarkerNumber<125 | AEK0317$MarkerNumber>125 & AEK0317$MarkerNumber<126 | AEK0317$MarkerNumber>126 & AEK0317$MarkerNumber<127 | AEK0317$MarkerNumber>127 & AEK0317$MarkerNumber<128)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>153 & AEK0317$MarkerNumber<154 | AEK0317$MarkerNumber>154 & AEK0317$MarkerNumber<155 | AEK0317$MarkerNumber>155 & AEK0317$MarkerNumber<156 | AEK0317$MarkerNumber>156 & AEK0317$MarkerNumber<157 | AEK0317$MarkerNumber>157 & AEK0317$MarkerNumber<158 | AEK0317$MarkerNumber>158 & AEK0317$MarkerNumber<159 | AEK0317$MarkerNumber>159 & AEK0317$MarkerNumber<160)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>169 & AEK0317$MarkerNumber<170 | AEK0317$MarkerNumber>170 & AEK0317$MarkerNumber<171 | AEK0317$MarkerNumber>171 & AEK0317$MarkerNumber<172 | AEK0317$MarkerNumber>172 & AEK0317$MarkerNumber<173 | AEK0317$MarkerNumber>173 & AEK0317$MarkerNumber<174 | AEK0317$MarkerNumber>174 & AEK0317$MarkerNumber<175 | AEK0317$MarkerNumber>175 & AEK0317$MarkerNumber<176)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>177 & AEK0317$MarkerNumber<178 | AEK0317$MarkerNumber>178 & AEK0317$MarkerNumber<179 | AEK0317$MarkerNumber>179 & AEK0317$MarkerNumber<180 | AEK0317$MarkerNumber>180 & AEK0317$MarkerNumber<181 | AEK0317$MarkerNumber>181 & AEK0317$MarkerNumber<182 | AEK0317$MarkerNumber>182 & AEK0317$MarkerNumber<183 | AEK0317$MarkerNumber>183 & AEK0317$MarkerNumber<184)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>185 & AEK0317$MarkerNumber<186 | AEK0317$MarkerNumber>186 & AEK0317$MarkerNumber<187 | AEK0317$MarkerNumber>187 & AEK0317$MarkerNumber<188 | AEK0317$MarkerNumber>188 & AEK0317$MarkerNumber<189 | AEK0317$MarkerNumber>189 & AEK0317$MarkerNumber<190 | AEK0317$MarkerNumber>190 & AEK0317$MarkerNumber<191 | AEK0317$MarkerNumber>191 & AEK0317$MarkerNumber<192)] <- 'PostAsianFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>193 & AEK0317$MarkerNumber<194 | AEK0317$MarkerNumber>194 & AEK0317$MarkerNumber<195 | AEK0317$MarkerNumber>195 & AEK0317$MarkerNumber<196 | AEK0317$MarkerNumber>196 & AEK0317$MarkerNumber<197 | AEK0317$MarkerNumber>197 & AEK0317$MarkerNumber<198 | AEK0317$MarkerNumber>198 & AEK0317$MarkerNumber<199 | AEK0317$MarkerNumber>200 & AEK0317$MarkerNumber<200)] <- 'PostAsianFixation'

#POST CAUCASION FIXATION
AEK0317$FaceType [which(AEK0317$MarkerNumber>1 & AEK0317$MarkerNumber<2 | AEK0317$MarkerNumber>2 & AEK0317$MarkerNumber<3 | AEK0317$MarkerNumber>3 & AEK0317$MarkerNumber<4 | AEK0317$MarkerNumber>4 & AEK0317$MarkerNumber<5 | AEK0317$MarkerNumber>5 & AEK0317$MarkerNumber<6 | AEK0317$MarkerNumber>6 & AEK0317$MarkerNumber<7 | AEK0317$MarkerNumber>7 & AEK0317$MarkerNumber<8)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>25 & AEK0317$MarkerNumber<26 | AEK0317$MarkerNumber>26 & AEK0317$MarkerNumber<27 | AEK0317$MarkerNumber>27 & AEK0317$MarkerNumber<28 | AEK0317$MarkerNumber>28 & AEK0317$MarkerNumber<29 | AEK0317$MarkerNumber>29 & AEK0317$MarkerNumber<30 | AEK0317$MarkerNumber>30 & AEK0317$MarkerNumber<31 | AEK0317$MarkerNumber>31 & AEK0317$MarkerNumber<32)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>33 & AEK0317$MarkerNumber<34 | AEK0317$MarkerNumber>34 & AEK0317$MarkerNumber<35 | AEK0317$MarkerNumber>35 & AEK0317$MarkerNumber<36 | AEK0317$MarkerNumber>36 & AEK0317$MarkerNumber<37 | AEK0317$MarkerNumber>37 & AEK0317$MarkerNumber<38 | AEK0317$MarkerNumber>38 & AEK0317$MarkerNumber<39 | AEK0317$MarkerNumber>39 & AEK0317$MarkerNumber<40)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>49 & AEK0317$MarkerNumber<50 | AEK0317$MarkerNumber>50 & AEK0317$MarkerNumber<51 | AEK0317$MarkerNumber>51 & AEK0317$MarkerNumber<52 | AEK0317$MarkerNumber>52 & AEK0317$MarkerNumber<53 | AEK0317$MarkerNumber>53 & AEK0317$MarkerNumber<54 | AEK0317$MarkerNumber>54 & AEK0317$MarkerNumber<55 | AEK0317$MarkerNumber>55 & AEK0317$MarkerNumber<56)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>57 & AEK0317$MarkerNumber<58 | AEK0317$MarkerNumber>58 & AEK0317$MarkerNumber<59 | AEK0317$MarkerNumber>59 & AEK0317$MarkerNumber<60 | AEK0317$MarkerNumber>60 & AEK0317$MarkerNumber<61 | AEK0317$MarkerNumber>61 & AEK0317$MarkerNumber<62 | AEK0317$MarkerNumber>62 & AEK0317$MarkerNumber<63 | AEK0317$MarkerNumber>63 & AEK0317$MarkerNumber<64)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>73 & AEK0317$MarkerNumber<74 | AEK0317$MarkerNumber>74 & AEK0317$MarkerNumber<75 | AEK0317$MarkerNumber>75 & AEK0317$MarkerNumber<76 | AEK0317$MarkerNumber>76 & AEK0317$MarkerNumber<77 | AEK0317$MarkerNumber>77 & AEK0317$MarkerNumber<78 | AEK0317$MarkerNumber>78 & AEK0317$MarkerNumber<79 | AEK0317$MarkerNumber>79 & AEK0317$MarkerNumber<80)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>81 & AEK0317$MarkerNumber<82 | AEK0317$MarkerNumber>82 & AEK0317$MarkerNumber<83 | AEK0317$MarkerNumber>83 & AEK0317$MarkerNumber<84 | AEK0317$MarkerNumber>84 & AEK0317$MarkerNumber<85 | AEK0317$MarkerNumber>85 & AEK0317$MarkerNumber<86 | AEK0317$MarkerNumber>86 & AEK0317$MarkerNumber<87 | AEK0317$MarkerNumber>87 & AEK0317$MarkerNumber<88)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>113 & AEK0317$MarkerNumber<114 | AEK0317$MarkerNumber>114 & AEK0317$MarkerNumber<115 | AEK0317$MarkerNumber>115 & AEK0317$MarkerNumber<116 | AEK0317$MarkerNumber>116 & AEK0317$MarkerNumber<117 | AEK0317$MarkerNumber>117 & AEK0317$MarkerNumber<118 | AEK0317$MarkerNumber>118 & AEK0317$MarkerNumber<119 | AEK0317$MarkerNumber>119 & AEK0317$MarkerNumber<120)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>129 & AEK0317$MarkerNumber<130 | AEK0317$MarkerNumber>130 & AEK0317$MarkerNumber<131 | AEK0317$MarkerNumber>131 & AEK0317$MarkerNumber<132 | AEK0317$MarkerNumber>132 & AEK0317$MarkerNumber<133 | AEK0317$MarkerNumber>133 & AEK0317$MarkerNumber<134 | AEK0317$MarkerNumber>134 & AEK0317$MarkerNumber<135 | AEK0317$MarkerNumber>135 & AEK0317$MarkerNumber<136)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>137 & AEK0317$MarkerNumber<138 | AEK0317$MarkerNumber>138 & AEK0317$MarkerNumber<139 | AEK0317$MarkerNumber>139 & AEK0317$MarkerNumber<140 | AEK0317$MarkerNumber>140 & AEK0317$MarkerNumber<141 | AEK0317$MarkerNumber>141 & AEK0317$MarkerNumber<142 | AEK0317$MarkerNumber>142 & AEK0317$MarkerNumber<143 | AEK0317$MarkerNumber>143 & AEK0317$MarkerNumber<144)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>145 & AEK0317$MarkerNumber<146 | AEK0317$MarkerNumber>146 & AEK0317$MarkerNumber<147 | AEK0317$MarkerNumber>147 & AEK0317$MarkerNumber<148 | AEK0317$MarkerNumber>148 & AEK0317$MarkerNumber<149 | AEK0317$MarkerNumber>149 & AEK0317$MarkerNumber<150 | AEK0317$MarkerNumber>150 & AEK0317$MarkerNumber<151 | AEK0317$MarkerNumber>151 & AEK0317$MarkerNumber<152)] <- 'PostCaucasionFixation'

AEK0317$FaceType [which(AEK0317$MarkerNumber>161 & AEK0317$MarkerNumber<162 | AEK0317$MarkerNumber>162 & AEK0317$MarkerNumber<163 | AEK0317$MarkerNumber>163 & AEK0317$MarkerNumber<164 | AEK0317$MarkerNumber>164 & AEK0317$MarkerNumber<165 | AEK0317$MarkerNumber>165 & AEK0317$MarkerNumber<166 | AEK0317$MarkerNumber>166 & AEK0317$MarkerNumber<167 | AEK0317$MarkerNumber>167 & AEK0317$MarkerNumber<168)] <- 'PostCaucasionFixation'

#Also label the interblock fixations as baseline fixations
AEK0317$TaskCue [which(AEK0317$MarkerNumber>16 & AEK0317$MarkerNumber<17 | AEK0317$MarkerNumber>24 & AEK0317$MarkerNumber<25 | AEK0317$MarkerNumber>32 & AEK0317$MarkerNumber<33 | AEK0317$MarkerNumber>40 & AEK0317$MarkerNumber<41 | AEK0317$MarkerNumber>48 & AEK0317$MarkerNumber<49 | AEK0317$MarkerNumber>56 & AEK0317$MarkerNumber<57 | AEK0317$MarkerNumber>64 & AEK0317$MarkerNumber<65)] <- "BaselineFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>80 & AEK0317$MarkerNumber<81 | AEK0317$MarkerNumber>88 & AEK0317$MarkerNumber<89 | AEK0317$MarkerNumber>96 & AEK0317$MarkerNumber<97 | AEK0317$MarkerNumber>104 & AEK0317$MarkerNumber<105 | AEK0317$MarkerNumber>112 & AEK0317$MarkerNumber<113 | AEK0317$MarkerNumber>120 & AEK0317$MarkerNumber<121 | AEK0317$MarkerNumber>128 & AEK0317$MarkerNumber<129)] <- "BaselineFixation"

AEK0317$TaskCue [which(AEK0317$MarkerNumber>144 & AEK0317$MarkerNumber<145 | AEK0317$MarkerNumber>152 & AEK0317$MarkerNumber<153 | AEK0317$MarkerNumber>160 & AEK0317$MarkerNumber<161 | AEK0317$MarkerNumber>168 & AEK0317$MarkerNumber<169 | AEK0317$MarkerNumber>176 & AEK0317$MarkerNumber<177 | AEK0317$MarkerNumber>184 & AEK0317$MarkerNumber<185 | AEK0317$MarkerNumber>192 & AEK0317$MarkerNumber<193)] <- "BaselineFixation"

AEK0317$ValenceType [which(AEK0317$MarkerNumber>16 & AEK0317$MarkerNumber<17 | AEK0317$MarkerNumber>24 & AEK0317$MarkerNumber<25 | AEK0317$MarkerNumber>32 & AEK0317$MarkerNumber<33 | AEK0317$MarkerNumber>40 & AEK0317$MarkerNumber<41 | AEK0317$MarkerNumber>48 & AEK0317$MarkerNumber<49 | AEK0317$MarkerNumber>56 & AEK0317$MarkerNumber<57 | AEK0317$MarkerNumber>64 & AEK0317$MarkerNumber<65)] <- "BaselineFixation"

AEK0317$ValenceType [which(AEK0317$MarkerNumber>80 & AEK0317$MarkerNumber<81 | AEK0317$MarkerNumber>88 & AEK0317$MarkerNumber<89 | AEK0317$MarkerNumber>96 & AEK0317$MarkerNumber<97 | AEK0317$MarkerNumber>104 & AEK0317$MarkerNumber<105 | AEK0317$MarkerNumber>112 & AEK0317$MarkerNumber<113 | AEK0317$MarkerNumber>120 & AEK0317$MarkerNumber<121 | AEK0317$MarkerNumber>128 & AEK0317$MarkerNumber<129)] <- "BaselineFixation"

AEK0317$ValenceType [which(AEK0317$MarkerNumber>144 & AEK0317$MarkerNumber<145 | AEK0317$MarkerNumber>152 & AEK0317$MarkerNumber<153 | AEK0317$MarkerNumber>160 & AEK0317$MarkerNumber<161 | AEK0317$MarkerNumber>168 & AEK0317$MarkerNumber<169 | AEK0317$MarkerNumber>176 & AEK0317$MarkerNumber<177 | AEK0317$MarkerNumber>184 & AEK0317$MarkerNumber<185 | AEK0317$MarkerNumber>192 & AEK0317$MarkerNumber<193)] <- "BaselineFixation"

AEK0317$FaceType [which(AEK0317$MarkerNumber>16 & AEK0317$MarkerNumber<17 | AEK0317$MarkerNumber>24 & AEK0317$MarkerNumber<25 | AEK0317$MarkerNumber>32 & AEK0317$MarkerNumber<33 | AEK0317$MarkerNumber>40 & AEK0317$MarkerNumber<41 | AEK0317$MarkerNumber>48 & AEK0317$MarkerNumber<49 | AEK0317$MarkerNumber>56 & AEK0317$MarkerNumber<57 | AEK0317$MarkerNumber>64 & AEK0317$MarkerNumber<65)] <- "BaselineFixation"

AEK0317$FaceType [which(AEK0317$MarkerNumber>80 & AEK0317$MarkerNumber<81 | AEK0317$MarkerNumber>88 & AEK0317$MarkerNumber<89 | AEK0317$MarkerNumber>96 & AEK0317$MarkerNumber<97 | AEK0317$MarkerNumber>104 & AEK0317$MarkerNumber<105 | AEK0317$MarkerNumber>112 & AEK0317$MarkerNumber<113 | AEK0317$MarkerNumber>120 & AEK0317$MarkerNumber<121 | AEK0317$MarkerNumber>128 & AEK0317$MarkerNumber<129)] <- "BaselineFixation"

AEK0317$FaceType [which(AEK0317$MarkerNumber>144 & AEK0317$MarkerNumber<145 | AEK0317$MarkerNumber>152 & AEK0317$MarkerNumber<153 | AEK0317$MarkerNumber>160 & AEK0317$MarkerNumber<161 | AEK0317$MarkerNumber>168 & AEK0317$MarkerNumber<169 | AEK0317$MarkerNumber>176 & AEK0317$MarkerNumber<177 | AEK0317$MarkerNumber>184 & AEK0317$MarkerNumber<185 | AEK0317$MarkerNumber>192 & AEK0317$MarkerNumber<193)] <- "BaselineFixation"





#re-label fixations to reflect pre vs post stimulus fixations
AEK0317$TaskCue [which(AEK0317$TaskCue == "PostInOutFixation" & AEK0317$PupilaryLatencyAll>=-2500 & AEK0317$PupilaryLatencyAll<=-.001)] <- "PreInOutFixation" 

AEK0317$TaskCue [which(AEK0317$TaskCue == "PostMaleFemaleFixation" & AEK0317$PupilaryLatencyAll>=-2500 & AEK0317$PupilaryLatencyAll<=-.001)] <- "PreMaleFemaleFixation" 

AEK0317$TaskCue [which(AEK0317$TaskCue == "PostLikeDislikeFixation" & AEK0317$PupilaryLatencyAll>=-2500 & AEK0317$PupilaryLatencyAll<=-.001)] <- "PreLikeDislikeFixation" 

AEK0317$ValenceType [which(AEK0317$ValenceType == "PostFearFixation" & AEK0317$PupilaryLatencyAll>=-2500 & AEK0317$PupilaryLatencyAll<=-.001)] <- "PreFearFixation" 

AEK0317$ValenceType [which(AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$PupilaryLatencyAll>=-2500 & AEK0317$PupilaryLatencyAll<=-.001)] <- "PreNeutralFixation" 

AEK0317$FaceType [which(AEK0317$FaceType == "PostAsianFixation" & AEK0317$PupilaryLatencyAll>=-2500 & AEK0317$PupilaryLatencyAll<=-.001)] <- "PreAsianFixation" 

AEK0317$FaceType [which(AEK0317$FaceType == "PostCaucasionFixation" & AEK0317$PupilaryLatencyAll>=-2500 & AEK0317$PupilaryLatencyAll<=-.001)] <- "PreCaucasionFixation" 





write.table(AEK0317,file="/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv",sep=",",row.names=F)



#AEK0317
AEK0317$FactorsAll <- NA

AEK0317$FactorsAll [which(AEK0317$TaskCue == "InOut" & AEK0317$FaceType == "Asian" & AEK0317$ValenceType == "Fear")] <- "AI/OF" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "MaleFemale" & AEK0317$FaceType == "Asian" & AEK0317$ValenceType == "Fear")] <- "AM/FF" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "LikeDislike" & AEK0317$FaceType == "Asian" & AEK0317$ValenceType == "Fear")] <- "AL/DF" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "InOut" & AEK0317$FaceType == "Asian" & AEK0317$ValenceType == "Neutral")] <- "AI/ON" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "MaleFemale" & AEK0317$FaceType == "Asian" & AEK0317$ValenceType == "Neutral")] <- "AM/FN" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "LikeDislike" & AEK0317$FaceType == "Asian" & AEK0317$ValenceType == "Neutral")] <- "AL/DN" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "InOut" & AEK0317$FaceType == "Caucasion" & AEK0317$ValenceType == "Fear")] <- "CI/OF" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "MaleFemale" & AEK0317$FaceType == "Caucasion" & AEK0317$ValenceType == "Fear")] <- "CM/FF" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "LikeDislike" & AEK0317$FaceType == "Caucasion" & AEK0317$ValenceType == "Fear")] <- "CL/DF" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "InOut" & AEK0317$FaceType == "Caucasion" & AEK0317$ValenceType == "Neutral")] <- "CI/ON" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "MaleFemale" & AEK0317$FaceType == "Caucasion" & AEK0317$ValenceType == "Neutral")] <- "CM/FN" 
AEK0317$FactorsAll [which(AEK0317$TaskCue == "LikeDislike" & AEK0317$FaceType == "Caucasion" & AEK0317$ValenceType == "Neutral")] <- "CL/DN" 


#Also make columns for 2 of the 3 dependent factors
AEK0317$ValenceFace <- NA
AEK0317$ValenceTaskCue <- NA
AEK0317$FaceTaskCue <- NA

AEK0317$ValenceFace [which(AEK0317$ValenceType == "Fear" & AEK0317$FaceType == "Asian")] <- "AsianFear"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "Fear" & AEK0317$FaceType == "Caucasion")] <- "CaucasionFear"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "Neutral" & AEK0317$FaceType == "Asian")] <- "AsianNeutral"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "Neutral" & AEK0317$FaceType == "Caucasion")] <- "CaucasionNeutral"
#break
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "Fear" & AEK0317$TaskCue == "InOut")] <- "InOutFear"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "Fear" & AEK0317$TaskCue == "MaleFemale")] <- "MaleFemaleFear"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "Fear" & AEK0317$TaskCue == "LikeDislike")] <- "LikeDislikeFear"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "Neutral" & AEK0317$TaskCue == "InOut")] <- "InOutNeutral"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "Neutral" & AEK0317$TaskCue == "MaleFemale")] <- "MaleFemaleNeutral"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "Neutral" & AEK0317$TaskCue == "LikeDislike")] <- "LikeDislikeNeutral"
#break
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "Asian" & AEK0317$TaskCue == "InOut")] <- "InOutAsian"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "Asian" & AEK0317$TaskCue == "MaleFemale")] <- "MaleFemaleAsian"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "Asian" & AEK0317$TaskCue == "LikeDislike")] <- "LikeDislikeAsian"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "Caucasion" & AEK0317$TaskCue == "InOut")] <- "InOutCaucasion"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "Caucasion" & AEK0317$TaskCue == "MaleFemale")] <- "MaleFemaleCaucasion"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "Caucasion" & AEK0317$TaskCue == "LikeDislike")] <- "LikeDislikeCaucasion"

##########
#Label Fixations!
#FactorsAll
#MaleFemale
AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostMaleFemaleFixation" & AEK0317$ValenceType == "PostFearFixation" & AEK0317$FaceType == "PostAsianFixation")] <- "PostMF-F-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostMaleFemaleFixation" & AEK0317$ValenceType == "PostFearFixation" & AEK0317$FaceType == "PostCaucasionFixation")] <- "PostMF-F-C-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostMaleFemaleFixation" & AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$FaceType == "PostAsianFixation")] <- "PostMF-N-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostMaleFemaleFixation" & AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$FaceType == "PostCaucasionFixation")] <- "PostMF-N-C-Fixation"
#LikeDislike
AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostLikeDislikeFixation" & AEK0317$ValenceType == "PostFearFixation" & AEK0317$FaceType == "PostAsianFixation")] <- "PostLD-F-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostLikeDislikeFixation" & AEK0317$ValenceType == "PostFearFixation" & AEK0317$FaceType == "PostCaucasionFixation")] <- "PostLD-F-C-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostLikeDislikeFixation" & AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$FaceType == "PostAsianFixation")] <- "PostLD-N-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostLikeDislikeFixation" & AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$FaceType == "PostCaucasionFixation")] <- "PostLD-N-C-Fixation"
#InOut
AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostInOutFixation" & AEK0317$ValenceType == "PostFearFixation" & AEK0317$FaceType == "PostAsianFixation")] <- "PostIO-F-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostInOutFixation" & AEK0317$ValenceType == "PostFearFixation" & AEK0317$FaceType == "PostCaucasionFixation")] <- "PostIO-F-C-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostInOutFixation" & AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$FaceType == "PostAsianFixation")] <- "PostIO-N-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PostInOutFixation" & AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$FaceType == "PostCaucasionFixation")] <- "PostIO-N-C-Fixation"
#ValenceFace Only
AEK0317$ValenceFace [which(AEK0317$ValenceType == "PostFearFixation" & AEK0317$FaceType == "PostAsianFixation")] <- "Post-F-A-Fixation"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "PostFearFixation" & AEK0317$FaceType == "PostCaucasionFixation")] <- "Post-F-C-Fixation"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$FaceType == "PostAsianFixation")] <- "Post-N-A-Fixation"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$FaceType == "PostCaucasionFixation")] <- "Post-N-C-Fixation"

#ValenceTaskCue
#MaleFemale
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PostFearFixation" & AEK0317$TaskCue == "PostMaleFemaleFixation")] <- "Post-F-MF-Fixation"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$TaskCue == "PostMaleFemaleFixation")] <- "Post-N-MF-Fixation"
#InOut
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PostFearFixation" & AEK0317$TaskCue == "PostInOutFixation")] <- "Post-F-IO-Fixation"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$TaskCue == "PostInOutFixation")] <- "Post-N-IO-Fixation"
#LikeDislike
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PostFearFixation" & AEK0317$TaskCue == "PostLikeDislikeFixation")] <- "Post-F-LD-Fixation"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PostNeutralFixation" & AEK0317$TaskCue == "PostLikeDislikeFixation")] <- "Post-N-LD-Fixation"

#FaceTaskCue
#MaleFemale
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PostAsianFixation" & AEK0317$TaskCue == "PostMaleFemaleFixation")] <- "Post-A-MF-Fixation"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PostCaucasionFixation" & AEK0317$TaskCue == "PostMaleFemaleFixation")] <- "Post-C-MF-Fixation"
#InOut
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PostAsianFixation" & AEK0317$TaskCue == "PostInOutFixation")] <- "Post-A-IO-Fixation"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PostCaucasionFixation" & AEK0317$TaskCue == "PostInOutFixation")] <- "Post-C-IO-Fixation"
#LikeDislike
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PostAsianFixation" & AEK0317$TaskCue == "PostLikeDislikeFixation")] <- "Post-A-LD-Fixation"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PostCaucasionFixation" & AEK0317$TaskCue == "PostLikeDislikeFixation")] <- "Post-C-LD-Fixation"


#label pre stimuli fixations too
AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreMaleFemaleFixation" & AEK0317$ValenceType == "PreFearFixation" & AEK0317$FaceType == "PreAsianFixation")] <- "PreMF-F-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreMaleFemaleFixation" & AEK0317$ValenceType == "PreFearFixation" & AEK0317$FaceType == "PreCaucasionFixation")] <- "PreMF-F-C-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreMaleFemaleFixation" & AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$FaceType == "PreAsianFixation")] <- "PreMF-N-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreMaleFemaleFixation" & AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$FaceType == "PreCaucasionFixation")] <- "PreMF-N-C-Fixation"
#LikeDislike
AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreLikeDislikeFixation" & AEK0317$ValenceType == "PreFearFixation" & AEK0317$FaceType == "PreAsianFixation")] <- "PreLD-F-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreLikeDislikeFixation" & AEK0317$ValenceType == "PreFearFixation" & AEK0317$FaceType == "PreCaucasionFixation")] <- "PreLD-F-C-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreLikeDislikeFixation" & AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$FaceType == "PreAsianFixation")] <- "PreLD-N-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreLikeDislikeFixation" & AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$FaceType == "PreCaucasionFixation")] <- "PreLD-N-C-Fixation"
#InOut
AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreInOutFixation" & AEK0317$ValenceType == "PreFearFixation" & AEK0317$FaceType == "PreAsianFixation")] <- "PreIO-F-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreInOutFixation" & AEK0317$ValenceType == "PreFearFixation" & AEK0317$FaceType == "PreCaucasionFixation")] <- "PreIO-F-C-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreInOutFixation" & AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$FaceType == "PreAsianFixation")] <- "PreIO-N-A-Fixation"

AEK0317$FactorsAll [which(AEK0317$TaskCue == "PreInOutFixation" & AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$FaceType == "PreCaucasionFixation")] <- "PreIO-N-C-Fixation"
#ValenceFace Only
AEK0317$ValenceFace [which(AEK0317$ValenceType == "PreFearFixation" & AEK0317$FaceType == "PreAsianFixation")] <- "Pre-F-A-Fixation"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "PreFearFixation" & AEK0317$FaceType == "PreCaucasionFixation")] <- "Pre-F-C-Fixation"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$FaceType == "PreAsianFixation")] <- "Pre-N-A-Fixation"
AEK0317$ValenceFace [which(AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$FaceType == "PreCaucasionFixation")] <- "Pre-N-C-Fixation"
#ValenceTaskCue
#MaleFemale
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PreFearFixation" & AEK0317$TaskCue == "PreMaleFemaleFixation")] <- "Pre-F-MF-Fixation"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$TaskCue == "PreMaleFemaleFixation")] <- "Pre-N-MF-Fixation"
#InOut
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PreFearFixation" & AEK0317$TaskCue == "PreInOutFixation")] <- "Pre-F-IO-Fixation"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$TaskCue == "PreInOutFixation")] <- "Pre-N-IO-Fixation"
#LikeDislike
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PreFearFixation" & AEK0317$TaskCue == "PreLikeDislikeFixation")] <- "Pre-F-LD-Fixation"
AEK0317$ValenceTaskCue [which(AEK0317$ValenceType == "PreNeutralFixation" & AEK0317$TaskCue == "PreLikeDislikeFixation")] <- "Pre-N-LD-Fixation"

#FaceTaskCue
#MaleFemale
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PreAsianFixation" & AEK0317$TaskCue == "PreMaleFemaleFixation")] <- "Pre-A-MF-Fixation"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PreCaucasionFixation" & AEK0317$TaskCue == "PreMaleFemaleFixation")] <- "Pre-C-MF-Fixation"
#InOut
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PreAsianFixation" & AEK0317$TaskCue == "PreInOutFixation")] <- "Pre-A-IO-Fixation"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PreCaucasionFixation" & AEK0317$TaskCue == "PreInOutFixation")] <- "Pre-C-IO-Fixation"
#LikeDislike
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PreAsianFixation" & AEK0317$TaskCue == "PreLikeDislikeFixation")] <- "Pre-A-LD-Fixation"
AEK0317$FaceTaskCue [which(AEK0317$FaceType == "PreCaucasionFixation" & AEK0317$TaskCue == "PreLikeDislikeFixation")] <- "Pre-C-LD-Fixation"

#Copy baseline fixations as well

AEK0317$FactorsAll [which(is.na(AEK0317$FactorsAll))] <- "BaselineFixation"
AEK0317$ValenceTaskCue [which(is.na(AEK0317$ValenceTaskCue))] <- "BaselineFixation"
AEK0317$ValenceFace [which(is.na(AEK0317$ValenceFace))] <- "BaselineFixation"
AEK0317$FaceTaskCue [which(is.na(AEK0317$FaceTaskCue))] <- "BaselineFixation"

#save csv
write.table(AEK0317,file="/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv",sep=",",row.names=F)

#Now clean rows with data data
library(plyr)
library(reshape)
##########
#count total number of rows, keep a tally so you can calculate the percentages of usable data
count(AEK0317$DataType) 
#count total during stimulus viewing
count(AEK0317$TaskCue)
count(AEK0317$ValenceType)
count(AEK0317$FaceType)
#delete rows with contain any data point that is outside of bounds
AEK0317 <- AEK0317[!(AEK0317$X_Gaze>1024 | AEK0317$X_Gaze<0 | AEK0317$Y_Gaze>768 | AEK0317$Y_Gaze<0),]
#recount so you can calculate percentage of data used
count(AEK0317$DataType)
count(AEK0317$TaskCue )

#delete rows in .csv is value in column X_Gaze is sequentially repreated
AEK0317v2 <- AEK0317[which(!duplicated(AEK0317$X_Gaze)] <- 
  AEK0317v3 <- AEK0317v2[which(!duplicated(AEK0317v2$Y_Gaze)] <- 
  #double check
  which(duplicated(AEK0317v3$X_Gaze),)
#count one last time
count(AEK0317v3$DataType)
count(AEK0317v3$TaskCue )

write.table(AEK0317v3,file="/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv",sep=",",row.names=F)

#delete rows with NAs in TaskCue/ValenceType/FaceType
#These are inter block fixation where the subjects were instructed to rest. Some stretched, or left the chin rest in general
AEK0317 <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv")

AEK0317 <- AEK0317[complete.cases(AEK0317[,12:14]),]
count(AEK0317$DataType)
count(AEK0317$TaskCue)
#Make new columns based on combined dependent factors


#After creating columns distributing the fixations and the stimuli, it is helpful for visualization to inspect them as one variable. 

AEK0317 <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv")

AEK0317$TaskCueNew <- NA
AEK0317$ValenceTypeNew <- NA
AEK0317$FaceTypeNew <- NA
AEK0317$ValenceFaceNew <- NA

AEK0317$TaskCueNew [which(AEK0317$TaskCue == "InOut" | AEK0317$TaskCue == "PostInOutFixation" | AEK0317$TaskCue == "PreInOutFixation")] <- 'InOut'

AEK0317$TaskCueNew [which(AEK0317$TaskCue == "LikeDislike" | AEK0317$TaskCue == "PostLikeDislikeFixation" | AEK0317$TaskCue == "PreLikeDislikeFixation")] <- 'LikeDislike'

AEK0317$TaskCueNew [which(AEK0317$TaskCue == "MaleFemale" | AEK0317$TaskCue == "PostMaleFemaleFixation" | AEK0317$TaskCue == "PreMaleFemaleFixation")] <- 'MaleFemale'


AEK0317$ValenceTypeNew [which(AEK0317$ValenceType == "Fear" | AEK0317$ValenceType == "PostFearFixation" | AEK0317$ValenceType == "PreFearFixation")] <- 'Fear'

AEK0317$ValenceTypeNew [which(AEK0317$ValenceType == "Neutral" | AEK0317$ValenceType == "PostNeutralFixation" | AEK0317$ValenceType == "PreNeutralFixation")] <- 'Neutral'


AEK0317$FaceTypeNew [which(AEK0317$FaceType == "Asian" | AEK0317$FaceType == "PostAsianFixation" | AEK0317$FaceType == "PreAsianFixation")] <- 'Asian'


AEK0317$FaceTypeNew [which(AEK0317$FaceType == "Caucasion" | AEK0317$FaceType == "PostCaucasionFixation" | AEK0317$FaceType == "PreCaucasionFixation")] <- 'Caucasion'


AEK0317$ValenceFaceNew [which(AEK0317$ValenceFace == "CaucasionFear" | AEK0317$ValenceFace == "Post-F-C-Fixation" | AEK0317$ValenceFace == "Pre-F-C-Fixation")] <- 'CaucasionFear'

AEK0317$ValenceFaceNew [which(AEK0317$ValenceFace == "CaucasionNeutral" | AEK0317$ValenceFace == "Post-N-C-Fixation" | AEK0317$ValenceFace == "Pre-N-C-Fixation")] <- 'CaucasionNeutral'

AEK0317$ValenceFaceNew [which(AEK0317$ValenceFace == "AsianFear" | AEK0317$ValenceFace == "Post-F-A-Fixation" | AEK0317$ValenceFace == "Pre-F-A-Fixation")] <- 'AsianFear'

AEK0317$ValenceFaceNew [which(AEK0317$ValenceFace == "AsianNeutral" | AEK0317$ValenceFace == "Post-N-A-Fixation" | AEK0317$ValenceFace == "Pre-N-A-Fixation")] <- 'AsianNeutral'



write.table(AEK0317,file="/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv",sep=",",row.names=F)

AEK0317 <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyeAEK0317.csv")


#make a new column that defines subject
AEK0317$SubjectId <- "AEK0317"
AEK0317$Trial <- NA


AEK0317$Trial [which(AEK0317$MarkerNumber>=1 & AEK0317$MarkerNumber<2)] <- 1
AEK0317$Trial [which(AEK0317$MarkerNumber>=2 & AEK0317$MarkerNumber<3)] <- 2
AEK0317$Trial [which(AEK0317$MarkerNumber>=3 & AEK0317$MarkerNumber<4)] <- 3
AEK0317$Trial [which(AEK0317$MarkerNumber>=4 & AEK0317$MarkerNumber<5)] <- 4
AEK0317$Trial [which(AEK0317$MarkerNumber>=5 & AEK0317$MarkerNumber<6)] <- 5
AEK0317$Trial [which(AEK0317$MarkerNumber>=6 & AEK0317$MarkerNumber<7)] <- 6
AEK0317$Trial [which(AEK0317$MarkerNumber>=7 & AEK0317$MarkerNumber<8)] <- 7
AEK0317$Trial [which(AEK0317$MarkerNumber>=8 & AEK0317$MarkerNumber<9)] <- 8
AEK0317$Trial [which(AEK0317$MarkerNumber>=9 & AEK0317$MarkerNumber<10)] <- 9
AEK0317$Trial [which(AEK0317$MarkerNumber>=10 & AEK0317$MarkerNumber<11)] <- 10
AEK0317$Trial [which(AEK0317$MarkerNumber>=11 & AEK0317$MarkerNumber<12)] <- 11
AEK0317$Trial [which(AEK0317$MarkerNumber>=12 & AEK0317$MarkerNumber<13)] <- 12
AEK0317$Trial [which(AEK0317$MarkerNumber>=13 & AEK0317$MarkerNumber<14)] <- 13
AEK0317$Trial [which(AEK0317$MarkerNumber>=14 & AEK0317$MarkerNumber<15)] <- 14
AEK0317$Trial [which(AEK0317$MarkerNumber>=15 & AEK0317$MarkerNumber<16)] <- 15
AEK0317$Trial [which(AEK0317$MarkerNumber>=16 & AEK0317$MarkerNumber<17)] <- 16
AEK0317$Trial [which(AEK0317$MarkerNumber>=17 & AEK0317$MarkerNumber<18)] <- 17
AEK0317$Trial [which(AEK0317$MarkerNumber>=18 & AEK0317$MarkerNumber<19)] <- 18
AEK0317$Trial [which(AEK0317$MarkerNumber>=19 & AEK0317$MarkerNumber<20)] <- 19
AEK0317$Trial [which(AEK0317$MarkerNumber>=20 & AEK0317$MarkerNumber<21)] <- 20
AEK0317$Trial [which(AEK0317$MarkerNumber>=21 & AEK0317$MarkerNumber<22)] <- 21
AEK0317$Trial [which(AEK0317$MarkerNumber>=22 & AEK0317$MarkerNumber<23)] <- 22
AEK0317$Trial [which(AEK0317$MarkerNumber>=23 & AEK0317$MarkerNumber<24)] <- 23
AEK0317$Trial [which(AEK0317$MarkerNumber>=24 & AEK0317$MarkerNumber<25)] <- 24
AEK0317$Trial [which(AEK0317$MarkerNumber>=25 & AEK0317$MarkerNumber<26)] <- 25
AEK0317$Trial [which(AEK0317$MarkerNumber>=26 & AEK0317$MarkerNumber<27)] <- 26
AEK0317$Trial [which(AEK0317$MarkerNumber>=27 & AEK0317$MarkerNumber<28)] <- 27
AEK0317$Trial [which(AEK0317$MarkerNumber>=28 & AEK0317$MarkerNumber<29)] <- 28
AEK0317$Trial [which(AEK0317$MarkerNumber>=29 & AEK0317$MarkerNumber<30)] <- 29
AEK0317$Trial [which(AEK0317$MarkerNumber>=30 & AEK0317$MarkerNumber<31)] <- 30
AEK0317$Trial [which(AEK0317$MarkerNumber>=31 & AEK0317$MarkerNumber<32)] <- 31
AEK0317$Trial [which(AEK0317$MarkerNumber>=32 & AEK0317$MarkerNumber<33)] <- 32
AEK0317$Trial [which(AEK0317$MarkerNumber>=33 & AEK0317$MarkerNumber<34)] <- 33
AEK0317$Trial [which(AEK0317$MarkerNumber>=34 & AEK0317$MarkerNumber<35)] <- 34
AEK0317$Trial [which(AEK0317$MarkerNumber>=35 & AEK0317$MarkerNumber<36)] <- 35
AEK0317$Trial [which(AEK0317$MarkerNumber>=36 & AEK0317$MarkerNumber<37)] <- 36
AEK0317$Trial [which(AEK0317$MarkerNumber>=37 & AEK0317$MarkerNumber<38)] <- 37
AEK0317$Trial [which(AEK0317$MarkerNumber>=38 & AEK0317$MarkerNumber<39)] <- 38
AEK0317$Trial [which(AEK0317$MarkerNumber>=39 & AEK0317$MarkerNumber<40)] <- 39
AEK0317$Trial [which(AEK0317$MarkerNumber>=40 & AEK0317$MarkerNumber<41)] <- 40
AEK0317$Trial [which(AEK0317$MarkerNumber>=41 & AEK0317$MarkerNumber<42)] <- 41
AEK0317$Trial [which(AEK0317$MarkerNumber>=42 & AEK0317$MarkerNumber<43)] <- 42
AEK0317$Trial [which(AEK0317$MarkerNumber>=43 & AEK0317$MarkerNumber<44)] <- 43
AEK0317$Trial [which(AEK0317$MarkerNumber>=44 & AEK0317$MarkerNumber<45)] <- 44
AEK0317$Trial [which(AEK0317$MarkerNumber>=45 & AEK0317$MarkerNumber<46)] <- 45
AEK0317$Trial [which(AEK0317$MarkerNumber>=46 & AEK0317$MarkerNumber<47)] <- 46
AEK0317$Trial [which(AEK0317$MarkerNumber>=47 & AEK0317$MarkerNumber<48)] <- 47
AEK0317$Trial [which(AEK0317$MarkerNumber>=48 & AEK0317$MarkerNumber<49)] <- 48
AEK0317$Trial [which(AEK0317$MarkerNumber>=49 & AEK0317$MarkerNumber<50)] <- 49
AEK0317$Trial [which(AEK0317$MarkerNumber>=50 & AEK0317$MarkerNumber<51)] <- 50
AEK0317$Trial [which(AEK0317$MarkerNumber>=51 & AEK0317$MarkerNumber<52)] <- 51
AEK0317$Trial [which(AEK0317$MarkerNumber>=52 & AEK0317$MarkerNumber<53)] <- 52
AEK0317$Trial [which(AEK0317$MarkerNumber>=53 & AEK0317$MarkerNumber<54)] <- 53
AEK0317$Trial [which(AEK0317$MarkerNumber>=54 & AEK0317$MarkerNumber<55)] <- 54
AEK0317$Trial [which(AEK0317$MarkerNumber>=55 & AEK0317$MarkerNumber<56)] <- 55
AEK0317$Trial [which(AEK0317$MarkerNumber>=56 & AEK0317$MarkerNumber<57)] <- 56
AEK0317$Trial [which(AEK0317$MarkerNumber>=57 & AEK0317$MarkerNumber<58)] <- 57
AEK0317$Trial [which(AEK0317$MarkerNumber>=58 & AEK0317$MarkerNumber<59)] <- 58
AEK0317$Trial [which(AEK0317$MarkerNumber>=59 & AEK0317$MarkerNumber<60)] <- 59
AEK0317$Trial [which(AEK0317$MarkerNumber>=60 & AEK0317$MarkerNumber<61)] <- 60
AEK0317$Trial [which(AEK0317$MarkerNumber>=61 & AEK0317$MarkerNumber<62)] <- 61
AEK0317$Trial [which(AEK0317$MarkerNumber>=62 & AEK0317$MarkerNumber<63)] <- 62
AEK0317$Trial [which(AEK0317$MarkerNumber>=63 & AEK0317$MarkerNumber<64)] <- 63
AEK0317$Trial [which(AEK0317$MarkerNumber>=64 & AEK0317$MarkerNumber<65)] <- 64
AEK0317$Trial [which(AEK0317$MarkerNumber>=65 & AEK0317$MarkerNumber<66)] <- 65
AEK0317$Trial [which(AEK0317$MarkerNumber>=66 & AEK0317$MarkerNumber<67)] <- 66
AEK0317$Trial [which(AEK0317$MarkerNumber>=67 & AEK0317$MarkerNumber<68)] <- 67
AEK0317$Trial [which(AEK0317$MarkerNumber>=68 & AEK0317$MarkerNumber<69)] <- 68
AEK0317$Trial [which(AEK0317$MarkerNumber>=69 & AEK0317$MarkerNumber<70)] <- 69
AEK0317$Trial [which(AEK0317$MarkerNumber>=70 & AEK0317$MarkerNumber<71)] <- 70
AEK0317$Trial [which(AEK0317$MarkerNumber>=71 & AEK0317$MarkerNumber<72)] <- 71
AEK0317$Trial [which(AEK0317$MarkerNumber>=72 & AEK0317$MarkerNumber<73)] <- 72
AEK0317$Trial [which(AEK0317$MarkerNumber>=73 & AEK0317$MarkerNumber<74)] <- 73
AEK0317$Trial [which(AEK0317$MarkerNumber>=74 & AEK0317$MarkerNumber<75)] <- 74
AEK0317$Trial [which(AEK0317$MarkerNumber>=75 & AEK0317$MarkerNumber<76)] <- 75
AEK0317$Trial [which(AEK0317$MarkerNumber>=76 & AEK0317$MarkerNumber<77)] <- 76
AEK0317$Trial [which(AEK0317$MarkerNumber>=77 & AEK0317$MarkerNumber<78)] <- 77
AEK0317$Trial [which(AEK0317$MarkerNumber>=78 & AEK0317$MarkerNumber<79)] <- 78
AEK0317$Trial [which(AEK0317$MarkerNumber>=79 & AEK0317$MarkerNumber<80)] <- 79
AEK0317$Trial [which(AEK0317$MarkerNumber>=80 & AEK0317$MarkerNumber<81)] <- 80
AEK0317$Trial [which(AEK0317$MarkerNumber>=81 & AEK0317$MarkerNumber<82)] <- 81
AEK0317$Trial [which(AEK0317$MarkerNumber>=82 & AEK0317$MarkerNumber<83)] <- 82
AEK0317$Trial [which(AEK0317$MarkerNumber>=83 & AEK0317$MarkerNumber<84)] <- 83
AEK0317$Trial [which(AEK0317$MarkerNumber>=84 & AEK0317$MarkerNumber<85)] <- 84
AEK0317$Trial [which(AEK0317$MarkerNumber>=85 & AEK0317$MarkerNumber<86)] <- 85
AEK0317$Trial [which(AEK0317$MarkerNumber>=86 & AEK0317$MarkerNumber<87)] <- 86
AEK0317$Trial [which(AEK0317$MarkerNumber>=87 & AEK0317$MarkerNumber<88)] <- 87
AEK0317$Trial [which(AEK0317$MarkerNumber>=88 & AEK0317$MarkerNumber<89)] <- 88
AEK0317$Trial [which(AEK0317$MarkerNumber>=89 & AEK0317$MarkerNumber<90)] <- 89
AEK0317$Trial [which(AEK0317$MarkerNumber>=90 & AEK0317$MarkerNumber<91)] <- 90
AEK0317$Trial [which(AEK0317$MarkerNumber>=91 & AEK0317$MarkerNumber<92)] <- 91
AEK0317$Trial [which(AEK0317$MarkerNumber>=92 & AEK0317$MarkerNumber<93)] <- 92
AEK0317$Trial [which(AEK0317$MarkerNumber>=93 & AEK0317$MarkerNumber<94)] <- 93
AEK0317$Trial [which(AEK0317$MarkerNumber>=94 & AEK0317$MarkerNumber<95)] <- 94
AEK0317$Trial [which(AEK0317$MarkerNumber>=95 & AEK0317$MarkerNumber<96)] <- 95
AEK0317$Trial [which(AEK0317$MarkerNumber>=96 & AEK0317$MarkerNumber<97)] <- 96
AEK0317$Trial [which(AEK0317$MarkerNumber>=97 & AEK0317$MarkerNumber<98)] <- 97
AEK0317$Trial [which(AEK0317$MarkerNumber>=98 & AEK0317$MarkerNumber<99)] <- 98
AEK0317$Trial [which(AEK0317$MarkerNumber>=99 & AEK0317$MarkerNumber<100)] <- 99
AEK0317$Trial [which(AEK0317$MarkerNumber>=100 & AEK0317$MarkerNumber<101)] <- 100
AEK0317$Trial [which(AEK0317$MarkerNumber>=101 & AEK0317$MarkerNumber<102)] <- 101
AEK0317$Trial [which(AEK0317$MarkerNumber>=102 & AEK0317$MarkerNumber<103)] <- 102
AEK0317$Trial [which(AEK0317$MarkerNumber>=103 & AEK0317$MarkerNumber<104)] <- 103
AEK0317$Trial [which(AEK0317$MarkerNumber>=104 & AEK0317$MarkerNumber<105)] <- 104
AEK0317$Trial [which(AEK0317$MarkerNumber>=105 & AEK0317$MarkerNumber<106)] <- 105
AEK0317$Trial [which(AEK0317$MarkerNumber>=106 & AEK0317$MarkerNumber<107)] <- 106
AEK0317$Trial [which(AEK0317$MarkerNumber>=107 & AEK0317$MarkerNumber<108)] <- 107
AEK0317$Trial [which(AEK0317$MarkerNumber>=108 & AEK0317$MarkerNumber<109)] <- 108
AEK0317$Trial [which(AEK0317$MarkerNumber>=109 & AEK0317$MarkerNumber<110)] <- 109
AEK0317$Trial [which(AEK0317$MarkerNumber>=110 & AEK0317$MarkerNumber<111)] <- 110
AEK0317$Trial [which(AEK0317$MarkerNumber>=111 & AEK0317$MarkerNumber<112)] <- 111
AEK0317$Trial [which(AEK0317$MarkerNumber>=112 & AEK0317$MarkerNumber<113)] <- 112
AEK0317$Trial [which(AEK0317$MarkerNumber>=113 & AEK0317$MarkerNumber<114)] <- 113
AEK0317$Trial [which(AEK0317$MarkerNumber>=114 & AEK0317$MarkerNumber<115)] <- 114
AEK0317$Trial [which(AEK0317$MarkerNumber>=115 & AEK0317$MarkerNumber<116)] <- 115
AEK0317$Trial [which(AEK0317$MarkerNumber>=116 & AEK0317$MarkerNumber<117)] <- 116
AEK0317$Trial [which(AEK0317$MarkerNumber>=117 & AEK0317$MarkerNumber<118)] <- 117
AEK0317$Trial [which(AEK0317$MarkerNumber>=118 & AEK0317$MarkerNumber<119)] <- 118
AEK0317$Trial [which(AEK0317$MarkerNumber>=119 & AEK0317$MarkerNumber<120)] <- 119
AEK0317$Trial [which(AEK0317$MarkerNumber>=120 & AEK0317$MarkerNumber<121)] <- 120
AEK0317$Trial [which(AEK0317$MarkerNumber>=121 & AEK0317$MarkerNumber<122)] <- 121
AEK0317$Trial [which(AEK0317$MarkerNumber>=122 & AEK0317$MarkerNumber<123)] <- 122
AEK0317$Trial [which(AEK0317$MarkerNumber>=123 & AEK0317$MarkerNumber<124)] <- 123
AEK0317$Trial [which(AEK0317$MarkerNumber>=124 & AEK0317$MarkerNumber<125)] <- 124
AEK0317$Trial [which(AEK0317$MarkerNumber>=125 & AEK0317$MarkerNumber<126)] <- 125
AEK0317$Trial [which(AEK0317$MarkerNumber>=126 & AEK0317$MarkerNumber<127)] <- 126
AEK0317$Trial [which(AEK0317$MarkerNumber>=127 & AEK0317$MarkerNumber<128)] <- 127
AEK0317$Trial [which(AEK0317$MarkerNumber>=128 & AEK0317$MarkerNumber<129)] <- 128
AEK0317$Trial [which(AEK0317$MarkerNumber>=129 & AEK0317$MarkerNumber<130)] <- 129
AEK0317$Trial [which(AEK0317$MarkerNumber>=130 & AEK0317$MarkerNumber<131)] <- 130
AEK0317$Trial [which(AEK0317$MarkerNumber>=131 & AEK0317$MarkerNumber<132)] <- 131
AEK0317$Trial [which(AEK0317$MarkerNumber>=132 & AEK0317$MarkerNumber<133)] <- 132
AEK0317$Trial [which(AEK0317$MarkerNumber>=133 & AEK0317$MarkerNumber<134)] <- 133
AEK0317$Trial [which(AEK0317$MarkerNumber>=134 & AEK0317$MarkerNumber<135)] <- 134
AEK0317$Trial [which(AEK0317$MarkerNumber>=135 & AEK0317$MarkerNumber<136)] <- 135
AEK0317$Trial [which(AEK0317$MarkerNumber>=136 & AEK0317$MarkerNumber<137)] <- 136
AEK0317$Trial [which(AEK0317$MarkerNumber>=137 & AEK0317$MarkerNumber<138)] <- 137
AEK0317$Trial [which(AEK0317$MarkerNumber>=138 & AEK0317$MarkerNumber<139)] <- 138
AEK0317$Trial [which(AEK0317$MarkerNumber>=139 & AEK0317$MarkerNumber<140)] <- 139
AEK0317$Trial [which(AEK0317$MarkerNumber>=140 & AEK0317$MarkerNumber<141)] <- 140
AEK0317$Trial [which(AEK0317$MarkerNumber>=141 & AEK0317$MarkerNumber<142)] <- 141
AEK0317$Trial [which(AEK0317$MarkerNumber>=142 & AEK0317$MarkerNumber<143)] <- 142
AEK0317$Trial [which(AEK0317$MarkerNumber>=143 & AEK0317$MarkerNumber<144)] <- 143
AEK0317$Trial [which(AEK0317$MarkerNumber>=144 & AEK0317$MarkerNumber<145)] <- 144
AEK0317$Trial [which(AEK0317$MarkerNumber>=145 & AEK0317$MarkerNumber<146)] <- 145
AEK0317$Trial [which(AEK0317$MarkerNumber>=146 & AEK0317$MarkerNumber<147)] <- 146
AEK0317$Trial [which(AEK0317$MarkerNumber>=147 & AEK0317$MarkerNumber<148)] <- 147
AEK0317$Trial [which(AEK0317$MarkerNumber>=148 & AEK0317$MarkerNumber<149)] <- 148
AEK0317$Trial [which(AEK0317$MarkerNumber>=149 & AEK0317$MarkerNumber<150)] <- 149
AEK0317$Trial [which(AEK0317$MarkerNumber>=150 & AEK0317$MarkerNumber<151)] <- 150
AEK0317$Trial [which(AEK0317$MarkerNumber>=151 & AEK0317$MarkerNumber<152)] <- 151
AEK0317$Trial [which(AEK0317$MarkerNumber>=152 & AEK0317$MarkerNumber<153)] <- 152
AEK0317$Trial [which(AEK0317$MarkerNumber>=153 & AEK0317$MarkerNumber<154)] <- 153
AEK0317$Trial [which(AEK0317$MarkerNumber>=154 & AEK0317$MarkerNumber<155)] <- 154
AEK0317$Trial [which(AEK0317$MarkerNumber>=155 & AEK0317$MarkerNumber<156)] <- 155
AEK0317$Trial [which(AEK0317$MarkerNumber>=156 & AEK0317$MarkerNumber<157)] <- 156
AEK0317$Trial [which(AEK0317$MarkerNumber>=157 & AEK0317$MarkerNumber<158)] <- 157
AEK0317$Trial [which(AEK0317$MarkerNumber>=158 & AEK0317$MarkerNumber<159)] <- 158
AEK0317$Trial [which(AEK0317$MarkerNumber>=159 & AEK0317$MarkerNumber<160)] <- 159
AEK0317$Trial [which(AEK0317$MarkerNumber>=160 & AEK0317$MarkerNumber<161)] <- 160
AEK0317$Trial [which(AEK0317$MarkerNumber>=161 & AEK0317$MarkerNumber<162)] <- 161
AEK0317$Trial [which(AEK0317$MarkerNumber>=162 & AEK0317$MarkerNumber<163)] <- 162
AEK0317$Trial [which(AEK0317$MarkerNumber>=163 & AEK0317$MarkerNumber<164)] <- 163
AEK0317$Trial [which(AEK0317$MarkerNumber>=164 & AEK0317$MarkerNumber<165)] <- 164
AEK0317$Trial [which(AEK0317$MarkerNumber>=165 & AEK0317$MarkerNumber<166)] <- 165
AEK0317$Trial [which(AEK0317$MarkerNumber>=166 & AEK0317$MarkerNumber<167)] <- 166
AEK0317$Trial [which(AEK0317$MarkerNumber>=167 & AEK0317$MarkerNumber<168)] <- 167
AEK0317$Trial [which(AEK0317$MarkerNumber>=168 & AEK0317$MarkerNumber<169)] <- 168
AEK0317$Trial [which(AEK0317$MarkerNumber>=169 & AEK0317$MarkerNumber<170)] <- 169
AEK0317$Trial [which(AEK0317$MarkerNumber>=170 & AEK0317$MarkerNumber<171)] <- 170
AEK0317$Trial [which(AEK0317$MarkerNumber>=171 & AEK0317$MarkerNumber<172)] <- 171
AEK0317$Trial [which(AEK0317$MarkerNumber>=172 & AEK0317$MarkerNumber<173)] <- 172
AEK0317$Trial [which(AEK0317$MarkerNumber>=173 & AEK0317$MarkerNumber<174)] <- 173
AEK0317$Trial [which(AEK0317$MarkerNumber>=174 & AEK0317$MarkerNumber<175)] <- 174
AEK0317$Trial [which(AEK0317$MarkerNumber>=175 & AEK0317$MarkerNumber<176)] <- 175
AEK0317$Trial [which(AEK0317$MarkerNumber>=176 & AEK0317$MarkerNumber<177)] <- 176
AEK0317$Trial [which(AEK0317$MarkerNumber>=177 & AEK0317$MarkerNumber<178)] <- 177
AEK0317$Trial [which(AEK0317$MarkerNumber>=178 & AEK0317$MarkerNumber<179)] <- 178
AEK0317$Trial [which(AEK0317$MarkerNumber>=179 & AEK0317$MarkerNumber<180)] <- 179
AEK0317$Trial [which(AEK0317$MarkerNumber>=180 & AEK0317$MarkerNumber<181)] <- 180
AEK0317$Trial [which(AEK0317$MarkerNumber>=181 & AEK0317$MarkerNumber<182)] <- 181
AEK0317$Trial [which(AEK0317$MarkerNumber>=182 & AEK0317$MarkerNumber<183)] <- 182
AEK0317$Trial [which(AEK0317$MarkerNumber>=183 & AEK0317$MarkerNumber<184)] <- 183
AEK0317$Trial [which(AEK0317$MarkerNumber>=184 & AEK0317$MarkerNumber<185)] <- 184
AEK0317$Trial [which(AEK0317$MarkerNumber>=185 & AEK0317$MarkerNumber<186)] <- 185
AEK0317$Trial [which(AEK0317$MarkerNumber>=186 & AEK0317$MarkerNumber<187)] <- 186
AEK0317$Trial [which(AEK0317$MarkerNumber>=187 & AEK0317$MarkerNumber<188)] <- 187
AEK0317$Trial [which(AEK0317$MarkerNumber>=188 & AEK0317$MarkerNumber<189)] <- 188
AEK0317$Trial [which(AEK0317$MarkerNumber>=189 & AEK0317$MarkerNumber<190)] <- 189
AEK0317$Trial [which(AEK0317$MarkerNumber>=190 & AEK0317$MarkerNumber<191)] <- 190
AEK0317$Trial [which(AEK0317$MarkerNumber>=191 & AEK0317$MarkerNumber<192)] <- 191
AEK0317$Trial [which(AEK0317$MarkerNumber>=192 & AEK0317$MarkerNumber<193)] <- 192
AEK0317$Trial [which(AEK0317$MarkerNumber>=193 & AEK0317$MarkerNumber<194)] <- 193
AEK0317$Trial [which(AEK0317$MarkerNumber>=194 & AEK0317$MarkerNumber<195)] <- 194
AEK0317$Trial [which(AEK0317$MarkerNumber>=195 & AEK0317$MarkerNumber<196)] <- 195
AEK0317$Trial [which(AEK0317$MarkerNumber>=196 & AEK0317$MarkerNumber<197)] <- 196
AEK0317$Trial [which(AEK0317$MarkerNumber>=197 & AEK0317$MarkerNumber<198)] <- 197
AEK0317$Trial [which(AEK0317$MarkerNumber>=198 & AEK0317$MarkerNumber<199)] <- 198
AEK0317$Trial [which(AEK0317$MarkerNumber>=199 & AEK0317$MarkerNumber<200)] <- 199
AEK0317$Trial [which(AEK0317$MarkerNumber>=200 & AEK0317$MarkerNumber<200.999)] <- 200
AEK0317$Trial [which(AEK0317$MarkerNumber>=201 & AEK0317$MarkerNumber<201.999)] <- 201
write.table(AEK0317,file="/Volumes/NO NAME/Csvgroup3-28/CSEyeAEK0317.csv",sep=",",row.names=F)

#read in the files
#group into one data frame
filenames <- list.files(path="/Volumes/NO NAME/Csvgroup3-28", full.name=TRUE)
library(plyr)
import.list <- llply(filenames, read.csv)
library(reshape)
data <- merge_recurse(import.list)
write.table(data,file="/Volumes/NO NAME/GroupAnalysis3-29/data_all.csv",sep=",",row.names=F)

###Statstical Analysis
data_all <- read.csv("/Volumes/NO NAME/GroupAnalysis3-29/data_all.csv")

data_all_nobase <- data_all[!(data_all$TaskCue == "BaselineFixation" | data_all$PupilaryLatencyAll>2000),]
#look for a way to do both at once, but this works for now
data_all_nobase <- data_all_nobase[which(!is.na(data_all_nobase$ValenceFaceNew)),]
#save csv
write.table(data_all_nobase,file="/Volumes/NO NAME/GroupAnalysis3-29/data_all_nobase.csv",sep=",",row.names=F)


#allmale> path should be to all male csv

#filenames <- list.files(path="/Volumes/NO NAME/Csvgroupmale3-29", full.name=TRUE)
#library(plyr)
#import.list <- llply(filenames, read.csv)
#library(reshape)
#data <- merge_recurse(import.list)
#write.table(data,file="/Volumes/NO NAME/GroupAnalysis3-29/data_allmale.csv",sep=",",row.names=F)


###Statstical Analysis
#data_allmale <- read.csv("/Volumes/NO NAME/GroupAnalysis3-29/data_allmale.csv")

#data_allmale_nobase <- data_allmale[!(data_allmale$TaskCue == "BaselineFixation" | data_allmale$PupilaryLatencyAll>2000),]
#look for a way to do both at once, but this works for now
#data_allmale_nobase2 <- data_allmale_nobase[which(!is.na(data_allmale_nobase$ValenceFaceNew)),]
#count to make sure no NA
#library(plyr)
#count(data_allmale_nobase2$ValenceTypeNew)
#save csv
#write.table(data_allmale_nobase2,file="/Volumes/NO NAME/GroupAnalysis3-29/data_allmale_nobase.csv",sep=",",row.names=F)

#allfemale> path should be to all female csv

#filenames <- list.files(path="/Volumes/NO NAME/Csvgroupfemale3-29", full.name=TRUE)
#library(plyr)
#import.list <- llply(filenames, read.csv)
#library(reshape)
#data <- merge_recurse(import.list)
#write.table(data,file="/Volumes/NO NAME/GroupAnalysis3-29/data_allfemale.csv",sep=",",row.names=F)


###Statstical Analysis
#data_allfemale <- read.csv("/Volumes/NO NAME/GroupAnalysis3-29/data_allfemale.csv")

#data_allfemale_nobase <- data_allfemale[!(data_allfemale$TaskCue == "BaselineFixation" | data_allfemale$PupilaryLatencyAll>2000),]
#look for a way to do both at once, but this works for now
#data_allfemale_nobase2 <- data_allfemale_nobase[which(!is.na(data_allfemale_nobase$ValenceFaceNew)),]
#count to make sure no NA
#library(plyr)
#count(data_allfemale_nobase2$ValenceTypeNew)
#save csv
#save csv
#write.table(data_allfemale_nobase2,file="/Volumes/NO NAME/GroupAnalysis3-29/data_allfemale_nobase.csv",sep=",",row.names=F)

#IMPORTANT
#read in CSVs
#data_allfemale_nobase <- read.csv("/Volumes/NO NAME/GroupAnalysis3-29/data_allfemale_nobase.csv")
#data_allmale_nobase <- read.csv("/Volumes/NO NAME/GroupAnalysis3-29/data_allmale_nobase.csv")
#data_all_nobase <- read.csv("/Volumes/NO NAME/GroupAnalysis3-29/data_all_nobase.csv")

#subset away anything out of the target viewing range.
#other studies have used 0.5s viewing time for dialtion analysis

all subjects, save as is necessary
Summary_all <- data_all_nobase[which(data_all_nobase$PupilaryLatencyAll>=1000 & data_all_nobase$PupilaryLatencyAll<=1500),]
write.table(Summary_all,file="/Volumes/NO NAME/GroupAnalysis3-29/SummaryKeyTimeRange.csv",sep=",",row.names=F)
##Read csv, manually cleaned vis Bernhardt et al. 1996
Summary_all <- read.csv("/Volumes/NO NAME/GroupAnalysis3-29/SummaryKeyTimeRange.csv")
#male subjects
#Summary_allmale <- data_allmale_nobase[which(data_allmale_nobase$PupilaryLatencyAll>=1000 & data_allmale_nobase$PupilaryLatencyAll<=1500),]
#female subjects
#Summary_allfemale <- data_allfemale_nobase[which(data_allfemale_nobase$PupilaryLatencyAll>=1000 & data_allfemale_nobase$PupilaryLatencyAll<=1500),]

#######functions for analysis######

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

########graphing
library(ggplot2)
#make graphable objects
PupilGraph_all <- summarySEwithin(Summary_all, measurevar="Pupil_Dia", withinvars=c("TaskCueNew","ValenceTypeNew", "FaceTypeNew"), idvar="SubjectId")
PupilGraph_allfacevalence <- summarySEwithin(Summary_all, measurevar="Pupil_Dia", withinvars=c("ValenceTypeNew", "FaceTypeNew"), idvar="SubjectId")
PupilGraph_allface <- summarySEwithin(Summary_all, measurevar="Pupil_Dia", withinvars=c("FaceTypeNew"), idvar="SubjectId")
#PupilGraph_male <- summarySEwithin(Summary_allmale, measurevar="Pupil_Dia", withinvars=c("TaskCueNew","ValenceTypeNew", "FaceTypeNew"), idvar="SubjectId")
#PupilGraph_female <- summarySEwithin(Summary_allfemale, measurevar="Pupil_Dia", withinvars=c("TaskCueNew","ValenceTypeNew", "FaceTypeNew"), idvar="SubjectId")
#savethese, yo
write.table(PupilGraph_all,file="/Volumes/NO NAME/GroupAnalysis3-29/PupilGraph_all.csv",sep=",",row.names=F)
#write.table(PupilGraph_male,file="/Volumes/NO NAME/GroupAnalysis3-29/PupilGraph_male.csv",sep=",",row.names=F)
#write.table(PupilGraph_female,file="/Volumes/NO NAME/GroupAnalysis3-29/PupilGraph_female.csv",sep=",",row.names=F)
write.table(PupilGraph_allfacevalence,file="/home/user/Documents/BehavioralEye/DataAnalysis/FinishedCSVs/PupilGraph_allfacevalence.csv", sep=",",row.names=F)




```{r read csvs}

PupilGraph_All <- read.csv("/home/user/Documents/BehavioralEye/DataAnalysis/FinishedCSVs/PupilGraph_all.csv")
Summary_all <- read.csv("/home/user/Documents/BehavioralEye/DataAnalysis/FinishedCSVs/SummaryKeyTimeRange.csv")


```

```{r Time Latency Histogram, fig.height=6, fig.width=8}
#histogram
library(ggplot2)
histogram <- ggplot(data_all_nobase, aes(x=Delta)) + geom_histogram(binwidth=3, colour="#ffcb05", fill="#0f284a")
ggsave(histogram,file="histogram_poster.pdf")

```


#make graphing object
```{r Pupillary_Dilation_in_Response_to_Object,_Race,_and_Emotion, fig.height=9, fig.width=12}
#make graphing object
#all subjects
PupilGraph_all <- ggplot(PupilGraph_all, aes(x=TaskCueNew, y=Pupil_Dia, fill=TaskCueNew)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  scale_fill_manual(values=c("#ffcb05","#655a52", "#00274c")) +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Pupil_Dia-ci, ymax=Pupil_Dia+ci)) +
  coord_cartesian(ylim=c(20,25)) +
  scale_y_continuous(breaks=seq(1:100)) +
  theme_bw() +
  geom_hline(yintercept=38) + xlab("Task Cue") + ylab("Pupil Diameter")+ ggtitle("Pupillary Dilation in Response to Object, Race, and Emotion")

# add facet
Allall_graph <- PupilGraph_all + facet_grid(ValenceTypeNew ~ FaceTypeNew) + theme(strip.text.x = element_text(size=16, colour="white"),
                                                                  strip.text.y = element_text(size=16, colour="white"),
                                                                  strip.background = element_rect(size=2, colour="#ffcb05", fill="#00274c"))

plot(Allall_graph)
#object if to save
ggsave(Allall_graph,file="Allall_graphpdf_DATE.pdf")

```


```{r valence/emotion, fig.height=6, fig.width=8}

#make graphing object
#all subjects
#overall pupil dilation in response to valence type
ValenceFace <- ggplot(PupilGraph_allfacevalence, aes(x=ValenceTypeNew, y=Pupil_Dia, fill=FaceTypeNew)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  scale_fill_manual(values=c("#ffcb05","#00274c")) +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Pupil_Dia-ci, ymax=Pupil_Dia+ci)) +
  coord_cartesian(ylim=c(20,23)) +
  scale_y_continuous(breaks=seq(1:100)) +
  theme_bw() +
  geom_hline(yintercept=38) + xlab("Emotion") + ylab("Pupil Diameter") +
  ggtitle("Pupillary Dilation in Response to Race and Emotion")


plot(ValenceFace)
#object if to save
ggsave(ValenceFace,file="PupilGraph_allvalenceface_DATE.pdf")
```


```{r fig.height=6, fig.width=8}
#line graph showing overall pupil dilation trends in response to race


PupilRace <- ggplot(PupilGraph_allface, aes(x=FaceTypeNew, y=Pupil_Dia, group=1, fill="#00274c")) +
  geom_line(size=2) +
  geom_errorbar(width=.1, aes(ymin=Pupil_Dia-ci, ymax=Pupil_Dia+ci,)) +
  geom_point(shape=21, size=5, fill="#ffcb05") + theme_bw()  +
  geom_hline(yintercept=22.6) + xlab("Emotion") + ylab("Pupil Diameter") + 
  ggtitle("Pupillary Dilation in Response to Emotion")


plot(PupilRace)

ggsave(PupilRace, file="PupilGraph_allrace_DATE.pdf")
```
