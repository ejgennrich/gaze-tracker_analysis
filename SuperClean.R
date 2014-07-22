#19.07.14
#Follow these instructions to clean behavioral data for Culture Seat Task

#####This is being reworked for analysis with gazepoint


##############cvs##############


```{r ReadinCSV}
samplecsv <- read.csv("samplecsv.csv")

###########csv#########################################################



### create empty marker column
samplecsv$Markers <- NA

### check/count number of markers
#table(samplecsv$DataType) ###should be 200 (w/ practice intro)

### shift 'Start' by one row down, store it in the new column
#samplecsv$Markers [which(samplecsv$DataType == 'Start') + 1] <- 'Start'
### shift 'Stop' by one row up, store it in the new column
#samplecsv$Markers [which(samplecsv$DataType == 'Stop') - 1] <- 'Stop'

### Save Csv to clean in excel
#write.table(samplecsv,file="/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv",sep=",",row.names=F)

### Go into Excel. Sort/Delete Column "DataType". Create columns Delta, TimeMs, and TimeS

#### Read in the CSV file
#samplecsv <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv")

#### For CultureSEAT, markers must be moved .5 seconds based on how the task blocks are written

### Create new Marker column that will replace old Markercolumn
#samplecsv$MarkersNew <- NA

### Start markers are placed in data 0.5 seconds before stimulus onset, so move it down 0.5s.
#all.positionsStart <- sapply(which(samplecsv$Markers == "Start"), function(x) with(samplecsv, max(which(TimeS <= TimeS[[x]] + 0.5))))
samplecsv$MarkersNew[all.positionsStart] <- "Start"


### Stop markers are placed in data 0.5 seconds after stimulus offset, so move it up 0.5s.
#all.positionsStop <- sapply(which(samplecsv$Markers == "Stop"), function(x) with (samplecsv, max(which(TimeS <= TimeS[[x]] - 0.5))))
#samplecsv$MarkersNew[all.positionsStop] <- "Stop"

#######TEST######

#samplecsv$MarkersNewNew <- NA
#samplecsv$MarkersNewNew [which(samplecsv$MarkersNew == "Start")] <- "Start"

#all.positionsStop <- sapply(which(samplecsv$MarkersNew == "Start"), function(x) with(samplecsv, max(which(TimeMs <= TimeMs[[x]] + 1500))))
#samplecsv$MarkersNewNew[all.positionsStop] <- "Stop"

#table(samplecsv$MarkersNewNew)
#########EndTest#########

### The next step is to fill the cells between the Starts and Stops in a way that allows us to sort by stimulus or block.
### To accomplish this, first sequentially number the Starts/Stops

### make a new column for stimlus numbering
#samplecsv$MarkerNumber <- NA

### In new column$Marker, label by sequential number the start markers from column$DataType
#samplecsv$MarkerNumber [which(samplecsv$MarkersNewNew == 'Start')] <- c(1:200)


### In new column$Marker, label by sequential number the stop markers from column$DataType
#samplecsv$MarkerNumber [which(samplecsv$MarkersNewNew == 'Stop')] <- c(1:200)

#check that number is 200 twice
#table(samplecsv$MarkerNumber)

###### Save Csv to clean in excel
### In excel, delete the rows of data before the first and after the last stimulus
#write.table(samplecsv,file="/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv",sep=",",row.names=F)

#### Read in the CSV file
#samplecsv <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv")

### Now fill the space in between the numbers with their respective stimuli number
### Where (1, NA, NA, NA, 1) turns into (1, 1, 1, 1, 1)

#library(zoo)

#a <- na.approx(samplecsv$MarkerNumber)
#la <- na.locf(samplecsv$MarkerNumber)
#data1 <- transform(samplecsv, MarkerNumber = ifelse(a == la,  a, NA))


###### Save Csv 
#write.table(data1,file="/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv",sep=",",row.names=F)

#### Read in the CSV file
#samplecsv <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv")

#This next step you are creating a new column that calculates in milliseconds from the stimulus onset to offset for each trial
#It's important to do this ste before deleting any rows that would throw off the calculation
samplecsv$group <- cumsum(samplecsv$MarkersNewNew=="Start" & !is.na(samplecsv$MarkersNewNew))
samplecsv$PupilaryLatencyAll <- unlist(aggregate(TimeMs~group,samplecsv,function(x)cumsum(c(0,diff(x))))$TimeMs)
samplecsv[,"group"] <- NULL


#alter pupil latency
#samplecsv$attempt <- 2500
#samplecsv <- transform(samplecsv, PupilaryLatencyAll = ifelse(PupilaryLatencyAll>=2000 & samplecsv$PupilaryLatencyAll<=2500, PupilaryLatencyAll - attempt, PupilaryLatencyAll))
#####################



###Label dependent variables: TaskCue, ValenceType, FaceType
###Use Task csv file to check block order and task version

samplecsv$TaskCue <- NA
samplecsv$FaceType <- NA
samplecsv$ValenceType <- NA




### TaskCue
#practice
samplecsv$TaskCue [which(samplecsv$MarkerNumber=1)] <- "InOut"
###run1
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=9 & samplecsv$MarkerNumber<=16)] <- "InOut"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=17 & samplecsv$MarkerNumber<=24)] <- "LikeDislike"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=25 & samplecsv$MarkerNumber<=32)] <- "InOut"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=33 & samplecsv$MarkerNumber<=40)] <- "MaleFemale"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=41 & samplecsv$MarkerNumber<=48)] <- "LikeDislike"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=49 & samplecsv$MarkerNumber<=56)] <- "MaleFemale"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=57 & samplecsv$MarkerNumber<=64)] <- "LikeDislike"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=65 & samplecsv$MarkerNumber<=72)] <- "MaleFemale"

### run2
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=73 & samplecsv$MarkerNumber<=80)] <- "InOut"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=81 & samplecsv$MarkerNumber<=88)] <- "MaleFemale"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=89 & samplecsv$MarkerNumber<=96)] <- "InOut"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=97 & samplecsv$MarkerNumber<=104)] <- "MaleFemale"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=105 & samplecsv$MarkerNumber<=112)] <- "LikeDislike"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=113 & samplecsv$MarkerNumber<=120)] <- "MaleFemale"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=121 & samplecsv$MarkerNumber<=128)] <- "InOut"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=129 & samplecsv$MarkerNumber<=136)] <- "LikeDislike"

### run3

samplecsv$TaskCue [which(samplecsv$MarkerNumber>=137 & samplecsv$MarkerNumber<=144)] <- "InOut"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=145 & samplecsv$MarkerNumber<=152)] <- "LikeDislike"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=153 & samplecsv$MarkerNumber<=160)] <- "MaleFemale"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=161 & samplecsv$MarkerNumber<=168)] <- "InOut"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=169 & samplecsv$MarkerNumber<=176)] <- "MaleFemale"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=177 & samplecsv$MarkerNumber<=184)] <- "LikeDislike"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=185 & samplecsv$MarkerNumber<=192)] <- "InOut"
samplecsv$TaskCue [which(samplecsv$MarkerNumber>=193 & samplecsv$MarkerNumber<=200)] <- "LikeDislike"
### FaceType

#practice
samplecsv$FaceType [which(samplecsv$MarkerNumber>=1 & samplecsv$MarkerNumber<=8)] <- "Caucasion"
###run1
samplecsv$FaceType [which(samplecsv$MarkerNumber>=9 & samplecsv$MarkerNumber<=16)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=17 & samplecsv$MarkerNumber<=24)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=25 & samplecsv$MarkerNumber<=32)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=33 & samplecsv$MarkerNumber<=40)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=41 & samplecsv$MarkerNumber<=48)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=49 & samplecsv$MarkerNumber<=56)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=57 & samplecsv$MarkerNumber<=64)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=65 & samplecsv$MarkerNumber<=72)] <- "Asian"

### run2
samplecsv$FaceType [which(samplecsv$MarkerNumber>=73 & samplecsv$MarkerNumber<=80)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=81 & samplecsv$MarkerNumber<=88)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=89 & samplecsv$MarkerNumber<=96)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=97 & samplecsv$MarkerNumber<=104)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=105 & samplecsv$MarkerNumber<=112)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=113 & samplecsv$MarkerNumber<=120)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=121 & samplecsv$MarkerNumber<=128)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=129 & samplecsv$MarkerNumber<=136)] <- "Caucasion"

### run3


samplecsv$FaceType [which(samplecsv$MarkerNumber>=137 & samplecsv$MarkerNumber<=144)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=145 & samplecsv$MarkerNumber<=152)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=153 & samplecsv$MarkerNumber<=160)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=161 & samplecsv$MarkerNumber<=168)] <- "Caucasion"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=169 & samplecsv$MarkerNumber<=176)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=177 & samplecsv$MarkerNumber<=184)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=185 & samplecsv$MarkerNumber<=192)] <- "Asian"
samplecsv$FaceType [which(samplecsv$MarkerNumber>=193 & samplecsv$MarkerNumber<=200)] <- "Asian"

### ValenceType
#practice
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=1 & samplecsv$MarkerNumber<=8)] <- "Fear"
###run1
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=9 & samplecsv$MarkerNumber<=16)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=17 & samplecsv$MarkerNumber<=24)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=25 & samplecsv$MarkerNumber<=32)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=33 & samplecsv$MarkerNumber<=40)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=41 & samplecsv$MarkerNumber<=48)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=49 & samplecsv$MarkerNumber<=56)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=57 & samplecsv$MarkerNumber<=64)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=65 & samplecsv$MarkerNumber<=72)] <- "Neutral"

### run2
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=73 & samplecsv$MarkerNumber<=80)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=81 & samplecsv$MarkerNumber<=88)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=89 & samplecsv$MarkerNumber<=96)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=97 & samplecsv$MarkerNumber<=104)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=105 & samplecsv$MarkerNumber<=112)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=113 & samplecsv$MarkerNumber<=120)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=121 & samplecsv$MarkerNumber<=128)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=129 & samplecsv$MarkerNumber<=136)] <- "Neutral"

### run3
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=137 & samplecsv$MarkerNumber<=144)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=145 & samplecsv$MarkerNumber<=152)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=153 & samplecsv$MarkerNumber<=160)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=161 & samplecsv$MarkerNumber<=168)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=169 & samplecsv$MarkerNumber<=176)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=177 & samplecsv$MarkerNumber<=184)] <- "Fear"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=185 & samplecsv$MarkerNumber<=192)] <- "Neutral"
samplecsv$ValenceType [which(samplecsv$MarkerNumber>=193 & samplecsv$MarkerNumber<=200)] <- "Fear"
#don't save yet


#Label fixations
#Found it necessary to calculuate based on dependent variable to see pertinent trends
#Redundent, look for cleaner way

outVec <- na.approx(samplecsv$MarkerNumber, na.rm=F)
samplecsv$MarkerNumber <- outVec


#POST INOUT FIXATION
samplecsv$TaskCue [which(samplecsv$MarkerNumber>1 & samplecsv$MarkerNumber<2 | samplecsv$MarkerNumber>2 & samplecsv$MarkerNumber<3 | samplecsv$MarkerNumber>3 & samplecsv$MarkerNumber<4 | samplecsv$MarkerNumber>4 & samplecsv$MarkerNumber<5 | samplecsv$MarkerNumber>5 & samplecsv$MarkerNumber<6 | samplecsv$MarkerNumber>6 & samplecsv$MarkerNumber<7 | samplecsv$MarkerNumber>7 & samplecsv$MarkerNumber<8)] <- "PostInOutFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>9 & samplecsv$MarkerNumber<10 | samplecsv$MarkerNumber>10 & samplecsv$MarkerNumber<11 | samplecsv$MarkerNumber>11 & samplecsv$MarkerNumber<12 | samplecsv$MarkerNumber>12 & samplecsv$MarkerNumber<13 | samplecsv$MarkerNumber>13 & samplecsv$MarkerNumber<14 | samplecsv$MarkerNumber>14 & samplecsv$MarkerNumber<15 | samplecsv$MarkerNumber>15 & samplecsv$MarkerNumber<16)] <- "PostInOutFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>25 & samplecsv$MarkerNumber<26 | samplecsv$MarkerNumber>26 & samplecsv$MarkerNumber<27 | samplecsv$MarkerNumber>27 & samplecsv$MarkerNumber<28 | samplecsv$MarkerNumber>28 & samplecsv$MarkerNumber<29 | samplecsv$MarkerNumber>29 & samplecsv$MarkerNumber<30 | samplecsv$MarkerNumber>30 & samplecsv$MarkerNumber<31 | samplecsv$MarkerNumber>31 & samplecsv$MarkerNumber<32)] <- "PostInOutFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>73 & samplecsv$MarkerNumber<74 | samplecsv$MarkerNumber>74 & samplecsv$MarkerNumber<75 | samplecsv$MarkerNumber>75 & samplecsv$MarkerNumber<76 | samplecsv$MarkerNumber>76 & samplecsv$MarkerNumber<77 | samplecsv$MarkerNumber>77 & samplecsv$MarkerNumber<78 | samplecsv$MarkerNumber>78 & samplecsv$MarkerNumber<79 | samplecsv$MarkerNumber>79 & samplecsv$MarkerNumber<80)] <- "PostInOutFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>89 & samplecsv$MarkerNumber<90 | samplecsv$MarkerNumber>90 & samplecsv$MarkerNumber<91 | samplecsv$MarkerNumber>91 & samplecsv$MarkerNumber<92 | samplecsv$MarkerNumber>92 & samplecsv$MarkerNumber<93 | samplecsv$MarkerNumber>93 & samplecsv$MarkerNumber<94 | samplecsv$MarkerNumber>94 & samplecsv$MarkerNumber<95 | samplecsv$MarkerNumber>95 & samplecsv$MarkerNumber<96)] <- "PostInOutFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>121 & samplecsv$MarkerNumber<122 | samplecsv$MarkerNumber>122 & samplecsv$MarkerNumber<123 | samplecsv$MarkerNumber>123 & samplecsv$MarkerNumber<124 | samplecsv$MarkerNumber>124 & samplecsv$MarkerNumber<125 | samplecsv$MarkerNumber>125 & samplecsv$MarkerNumber<126 | samplecsv$MarkerNumber>126 & samplecsv$MarkerNumber<127 | samplecsv$MarkerNumber>127 & samplecsv$MarkerNumber<128)] <- "PostInOutFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>137 & samplecsv$MarkerNumber<138 | samplecsv$MarkerNumber>138 & samplecsv$MarkerNumber<139 | samplecsv$MarkerNumber>139 & samplecsv$MarkerNumber<140 | samplecsv$MarkerNumber>140 & samplecsv$MarkerNumber<141 | samplecsv$MarkerNumber>141 & samplecsv$MarkerNumber<142 | samplecsv$MarkerNumber>142 & samplecsv$MarkerNumber<143 | samplecsv$MarkerNumber>143 & samplecsv$MarkerNumber<144)] <- "PostInOutFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>161 & samplecsv$MarkerNumber<162 | samplecsv$MarkerNumber>162 & samplecsv$MarkerNumber<163 | samplecsv$MarkerNumber>163 & samplecsv$MarkerNumber<164 | samplecsv$MarkerNumber>164 & samplecsv$MarkerNumber<165 | samplecsv$MarkerNumber>165 & samplecsv$MarkerNumber<166 | samplecsv$MarkerNumber>166 & samplecsv$MarkerNumber<167 | samplecsv$MarkerNumber>167 & samplecsv$MarkerNumber<168)] <- "PostInOutFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>185 & samplecsv$MarkerNumber<186 | samplecsv$MarkerNumber>186 & samplecsv$MarkerNumber<187 | samplecsv$MarkerNumber>187 & samplecsv$MarkerNumber<188 | samplecsv$MarkerNumber>188 & samplecsv$MarkerNumber<189 | samplecsv$MarkerNumber>189 & samplecsv$MarkerNumber<190 | samplecsv$MarkerNumber>190 & samplecsv$MarkerNumber<191 | samplecsv$MarkerNumber>191 & samplecsv$MarkerNumber<192)] <- 'PostInOutFixation'

#POST LIKEDISLIKE FIXATION
samplecsv$TaskCue [which(samplecsv$MarkerNumber>17 & samplecsv$MarkerNumber<18 | samplecsv$MarkerNumber>18 & samplecsv$MarkerNumber<19 | samplecsv$MarkerNumber>19 & samplecsv$MarkerNumber<20 | samplecsv$MarkerNumber>20 & samplecsv$MarkerNumber<21 | samplecsv$MarkerNumber>21 & samplecsv$MarkerNumber<22 | samplecsv$MarkerNumber>22 & samplecsv$MarkerNumber<23 | samplecsv$MarkerNumber>23 & samplecsv$MarkerNumber<24)] <- 'PostLikeDislikeFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>41 & samplecsv$MarkerNumber<42 | samplecsv$MarkerNumber>42 & samplecsv$MarkerNumber<43 | samplecsv$MarkerNumber>43 & samplecsv$MarkerNumber<44 | samplecsv$MarkerNumber>44 & samplecsv$MarkerNumber<45 | samplecsv$MarkerNumber>45 & samplecsv$MarkerNumber<46 | samplecsv$MarkerNumber>46 & samplecsv$MarkerNumber<47 | samplecsv$MarkerNumber>47 & samplecsv$MarkerNumber<48)] <- 'PostLikeDislikeFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>57 & samplecsv$MarkerNumber<58 | samplecsv$MarkerNumber>58 & samplecsv$MarkerNumber<59 | samplecsv$MarkerNumber>59 & samplecsv$MarkerNumber<60 | samplecsv$MarkerNumber>60 & samplecsv$MarkerNumber<61 | samplecsv$MarkerNumber>61 & samplecsv$MarkerNumber<62 | samplecsv$MarkerNumber>62 & samplecsv$MarkerNumber<63 | samplecsv$MarkerNumber>63 & samplecsv$MarkerNumber<64)] <- 'PostLikeDislikeFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>105 & samplecsv$MarkerNumber<106 | samplecsv$MarkerNumber>106 & samplecsv$MarkerNumber<107 | samplecsv$MarkerNumber>107 & samplecsv$MarkerNumber<108 | samplecsv$MarkerNumber>108 & samplecsv$MarkerNumber<109 | samplecsv$MarkerNumber>109 & samplecsv$MarkerNumber<110 | samplecsv$MarkerNumber>110 & samplecsv$MarkerNumber<111 | samplecsv$MarkerNumber>111 & samplecsv$MarkerNumber<112)] <- 'PostLikeDislikeFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>129 & samplecsv$MarkerNumber<130 | samplecsv$MarkerNumber>130 & samplecsv$MarkerNumber<131 | samplecsv$MarkerNumber>131 & samplecsv$MarkerNumber<132 | samplecsv$MarkerNumber>132 & samplecsv$MarkerNumber<133 | samplecsv$MarkerNumber>133 & samplecsv$MarkerNumber<134 | samplecsv$MarkerNumber>134 & samplecsv$MarkerNumber<135 | samplecsv$MarkerNumber>135 & samplecsv$MarkerNumber<136)] <- 'PostLikeDislikeFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>145 & samplecsv$MarkerNumber<146 | samplecsv$MarkerNumber>146 & samplecsv$MarkerNumber<147 | samplecsv$MarkerNumber>147 & samplecsv$MarkerNumber<148 | samplecsv$MarkerNumber>148 & samplecsv$MarkerNumber<149 | samplecsv$MarkerNumber>149 & samplecsv$MarkerNumber<150 | samplecsv$MarkerNumber>150 & samplecsv$MarkerNumber<151 | samplecsv$MarkerNumber>151 & samplecsv$MarkerNumber<152)] <- 'PostLikeDislikeFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>177 & samplecsv$MarkerNumber<178 | samplecsv$MarkerNumber>178 & samplecsv$MarkerNumber<179 | samplecsv$MarkerNumber>179 & samplecsv$MarkerNumber<180 | samplecsv$MarkerNumber>180 & samplecsv$MarkerNumber<181 | samplecsv$MarkerNumber>181 & samplecsv$MarkerNumber<182 | samplecsv$MarkerNumber>182 & samplecsv$MarkerNumber<183 | samplecsv$MarkerNumber>183 & samplecsv$MarkerNumber<184)] <- 'PostLikeDislikeFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>193 & samplecsv$MarkerNumber<194 | samplecsv$MarkerNumber>194 & samplecsv$MarkerNumber<195 | samplecsv$MarkerNumber>195 & samplecsv$MarkerNumber<196 | samplecsv$MarkerNumber>196 & samplecsv$MarkerNumber<197 | samplecsv$MarkerNumber>197 & samplecsv$MarkerNumber<198 | samplecsv$MarkerNumber>198 & samplecsv$MarkerNumber<199 | samplecsv$MarkerNumber>200 & samplecsv$MarkerNumber<200)] <- 'PostLikeDislikeFixation'


#POST MALEFEMALE FIXATION
samplecsv$TaskCue [which(samplecsv$MarkerNumber>33 & samplecsv$MarkerNumber<34 | samplecsv$MarkerNumber>34 & samplecsv$MarkerNumber<35 | samplecsv$MarkerNumber>35 & samplecsv$MarkerNumber<36 | samplecsv$MarkerNumber>36 & samplecsv$MarkerNumber<37 | samplecsv$MarkerNumber>37 & samplecsv$MarkerNumber<38 | samplecsv$MarkerNumber>38 & samplecsv$MarkerNumber<39 | samplecsv$MarkerNumber>39 & samplecsv$MarkerNumber<40)] <- 'PostMaleFemaleFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>49 & samplecsv$MarkerNumber<50 | samplecsv$MarkerNumber>50 & samplecsv$MarkerNumber<51 | samplecsv$MarkerNumber>51 & samplecsv$MarkerNumber<52 | samplecsv$MarkerNumber>52 & samplecsv$MarkerNumber<53 | samplecsv$MarkerNumber>53 & samplecsv$MarkerNumber<54 | samplecsv$MarkerNumber>54 & samplecsv$MarkerNumber<55 | samplecsv$MarkerNumber>55 & samplecsv$MarkerNumber<56)] <- 'PostMaleFemaleFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>65 & samplecsv$MarkerNumber<66 | samplecsv$MarkerNumber>66 & samplecsv$MarkerNumber<67 | samplecsv$MarkerNumber>67 & samplecsv$MarkerNumber<68 | samplecsv$MarkerNumber>68 & samplecsv$MarkerNumber<69 | samplecsv$MarkerNumber>69 & samplecsv$MarkerNumber<70 | samplecsv$MarkerNumber>70 & samplecsv$MarkerNumber<71 | samplecsv$MarkerNumber>71 & samplecsv$MarkerNumber<72)] <- 'PostMaleFemaleFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>81 & samplecsv$MarkerNumber<82 | samplecsv$MarkerNumber>82 & samplecsv$MarkerNumber<83 | samplecsv$MarkerNumber>83 & samplecsv$MarkerNumber<84 | samplecsv$MarkerNumber>84 & samplecsv$MarkerNumber<85 | samplecsv$MarkerNumber>85 & samplecsv$MarkerNumber<86 | samplecsv$MarkerNumber>86 & samplecsv$MarkerNumber<87 | samplecsv$MarkerNumber>87 & samplecsv$MarkerNumber<88)] <- 'PostMaleFemaleFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>97 & samplecsv$MarkerNumber<98 | samplecsv$MarkerNumber>98 & samplecsv$MarkerNumber<99 | samplecsv$MarkerNumber>99 & samplecsv$MarkerNumber<100 | samplecsv$MarkerNumber>100 & samplecsv$MarkerNumber<101 | samplecsv$MarkerNumber>101 & samplecsv$MarkerNumber<102 | samplecsv$MarkerNumber>102 & samplecsv$MarkerNumber<103 | samplecsv$MarkerNumber>103 & samplecsv$MarkerNumber<104)] <- 'PostMaleFemaleFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>113 & samplecsv$MarkerNumber<114 | samplecsv$MarkerNumber>114 & samplecsv$MarkerNumber<115 | samplecsv$MarkerNumber>115 & samplecsv$MarkerNumber<116 | samplecsv$MarkerNumber>116 & samplecsv$MarkerNumber<117 | samplecsv$MarkerNumber>117 & samplecsv$MarkerNumber<118 | samplecsv$MarkerNumber>118 & samplecsv$MarkerNumber<119 | samplecsv$MarkerNumber>119 & samplecsv$MarkerNumber<120)] <- 'PostMaleFemaleFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>153 & samplecsv$MarkerNumber<154 | samplecsv$MarkerNumber>154 & samplecsv$MarkerNumber<155 | samplecsv$MarkerNumber>155 & samplecsv$MarkerNumber<156 | samplecsv$MarkerNumber>156 & samplecsv$MarkerNumber<157 | samplecsv$MarkerNumber>157 & samplecsv$MarkerNumber<158 | samplecsv$MarkerNumber>158 & samplecsv$MarkerNumber<159 | samplecsv$MarkerNumber>159 & samplecsv$MarkerNumber<160)] <- 'PostMaleFemaleFixation'

samplecsv$TaskCue [which(samplecsv$MarkerNumber>169 & samplecsv$MarkerNumber<170 | samplecsv$MarkerNumber>170 & samplecsv$MarkerNumber<171 | samplecsv$MarkerNumber>171 & samplecsv$MarkerNumber<172 | samplecsv$MarkerNumber>172 & samplecsv$MarkerNumber<173 | samplecsv$MarkerNumber>173 & samplecsv$MarkerNumber<174 | samplecsv$MarkerNumber>174 & samplecsv$MarkerNumber<175 | samplecsv$MarkerNumber>175 & samplecsv$MarkerNumber<176)] <- 'PostMaleFemaleFixation'



#POST FEAR FIXATION

samplecsv$ValenceType [which(samplecsv$MarkerNumber>1 & samplecsv$MarkerNumber<2 | samplecsv$MarkerNumber>2 & samplecsv$MarkerNumber<3 | samplecsv$MarkerNumber>3 & samplecsv$MarkerNumber<4 | samplecsv$MarkerNumber>4 & samplecsv$MarkerNumber<5 | samplecsv$MarkerNumber>5 & samplecsv$MarkerNumber<6 | samplecsv$MarkerNumber>6 & samplecsv$MarkerNumber<7 | samplecsv$MarkerNumber>7 & samplecsv$MarkerNumber<8)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>9 & samplecsv$MarkerNumber<10 | samplecsv$MarkerNumber>10 & samplecsv$MarkerNumber<11 | samplecsv$MarkerNumber>11 & samplecsv$MarkerNumber<12 | samplecsv$MarkerNumber>12 & samplecsv$MarkerNumber<13 | samplecsv$MarkerNumber>13 & samplecsv$MarkerNumber<14 | samplecsv$MarkerNumber>14 & samplecsv$MarkerNumber<15 | samplecsv$MarkerNumber>15 & samplecsv$MarkerNumber<16)] <- 'PostFearFixation'


samplecsv$ValenceType [which(samplecsv$MarkerNumber>33 & samplecsv$MarkerNumber<34 | samplecsv$MarkerNumber>34 & samplecsv$MarkerNumber<35 | samplecsv$MarkerNumber>35 & samplecsv$MarkerNumber<36 | samplecsv$MarkerNumber>36 & samplecsv$MarkerNumber<37 | samplecsv$MarkerNumber>37 & samplecsv$MarkerNumber<38 | samplecsv$MarkerNumber>38 & samplecsv$MarkerNumber<39 | samplecsv$MarkerNumber>39 & samplecsv$MarkerNumber<40)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>41 & samplecsv$MarkerNumber<42 | samplecsv$MarkerNumber>42 & samplecsv$MarkerNumber<43 | samplecsv$MarkerNumber>43 & samplecsv$MarkerNumber<44 | samplecsv$MarkerNumber>44 & samplecsv$MarkerNumber<45 | samplecsv$MarkerNumber>45 & samplecsv$MarkerNumber<46 | samplecsv$MarkerNumber>46 & samplecsv$MarkerNumber<47 | samplecsv$MarkerNumber>47 & samplecsv$MarkerNumber<48)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>73 & samplecsv$MarkerNumber<74 | samplecsv$MarkerNumber>74 & samplecsv$MarkerNumber<75 | samplecsv$MarkerNumber>75 & samplecsv$MarkerNumber<76 | samplecsv$MarkerNumber>76 & samplecsv$MarkerNumber<77 | samplecsv$MarkerNumber>77 & samplecsv$MarkerNumber<78 | samplecsv$MarkerNumber>78 & samplecsv$MarkerNumber<79 | samplecsv$MarkerNumber>79 & samplecsv$MarkerNumber<80)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>97 & samplecsv$MarkerNumber<98 | samplecsv$MarkerNumber>98 & samplecsv$MarkerNumber<99 | samplecsv$MarkerNumber>99 & samplecsv$MarkerNumber<100 | samplecsv$MarkerNumber>100 & samplecsv$MarkerNumber<101 | samplecsv$MarkerNumber>101 & samplecsv$MarkerNumber<102 | samplecsv$MarkerNumber>102 & samplecsv$MarkerNumber<103 | samplecsv$MarkerNumber>103 & samplecsv$MarkerNumber<104)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>113 & samplecsv$MarkerNumber<114 | samplecsv$MarkerNumber>114 & samplecsv$MarkerNumber<115 | samplecsv$MarkerNumber>115 & samplecsv$MarkerNumber<116 | samplecsv$MarkerNumber>116 & samplecsv$MarkerNumber<117 | samplecsv$MarkerNumber>117 & samplecsv$MarkerNumber<118 | samplecsv$MarkerNumber>118 & samplecsv$MarkerNumber<119 | samplecsv$MarkerNumber>119 & samplecsv$MarkerNumber<120)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>121 & samplecsv$MarkerNumber<122 | samplecsv$MarkerNumber>122 & samplecsv$MarkerNumber<123 | samplecsv$MarkerNumber>123 & samplecsv$MarkerNumber<124 | samplecsv$MarkerNumber>124 & samplecsv$MarkerNumber<125 | samplecsv$MarkerNumber>125 & samplecsv$MarkerNumber<126 | samplecsv$MarkerNumber>126 & samplecsv$MarkerNumber<127 | samplecsv$MarkerNumber>127 & samplecsv$MarkerNumber<128)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>145 & samplecsv$MarkerNumber<146 | samplecsv$MarkerNumber>146 & samplecsv$MarkerNumber<147 | samplecsv$MarkerNumber>147 & samplecsv$MarkerNumber<148 | samplecsv$MarkerNumber>148 & samplecsv$MarkerNumber<149 | samplecsv$MarkerNumber>149 & samplecsv$MarkerNumber<150 | samplecsv$MarkerNumber>150 & samplecsv$MarkerNumber<151 | samplecsv$MarkerNumber>151 & samplecsv$MarkerNumber<152)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>153 & samplecsv$MarkerNumber<154 | samplecsv$MarkerNumber>154 & samplecsv$MarkerNumber<155 | samplecsv$MarkerNumber>155 & samplecsv$MarkerNumber<156 | samplecsv$MarkerNumber>156 & samplecsv$MarkerNumber<157 | samplecsv$MarkerNumber>157 & samplecsv$MarkerNumber<158 | samplecsv$MarkerNumber>158 & samplecsv$MarkerNumber<159 | samplecsv$MarkerNumber>159 & samplecsv$MarkerNumber<160)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>161 & samplecsv$MarkerNumber<162 | samplecsv$MarkerNumber>162 & samplecsv$MarkerNumber<163 | samplecsv$MarkerNumber>163 & samplecsv$MarkerNumber<164 | samplecsv$MarkerNumber>164 & samplecsv$MarkerNumber<165 | samplecsv$MarkerNumber>165 & samplecsv$MarkerNumber<166 | samplecsv$MarkerNumber>166 & samplecsv$MarkerNumber<167 | samplecsv$MarkerNumber>167 & samplecsv$MarkerNumber<168)] <- "PostFearFixation"

samplecsv$ValenceType [which(samplecsv$MarkerNumber>177 & samplecsv$MarkerNumber<178 | samplecsv$MarkerNumber>178 & samplecsv$MarkerNumber<179 | samplecsv$MarkerNumber>179 & samplecsv$MarkerNumber<180 | samplecsv$MarkerNumber>180 & samplecsv$MarkerNumber<181 | samplecsv$MarkerNumber>181 & samplecsv$MarkerNumber<182 | samplecsv$MarkerNumber>182 & samplecsv$MarkerNumber<183 | samplecsv$MarkerNumber>183 & samplecsv$MarkerNumber<184)] <- 'PostFearFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>193 & samplecsv$MarkerNumber<194 | samplecsv$MarkerNumber>194 & samplecsv$MarkerNumber<195 | samplecsv$MarkerNumber>195 & samplecsv$MarkerNumber<196 | samplecsv$MarkerNumber>196 & samplecsv$MarkerNumber<197 | samplecsv$MarkerNumber>197 & samplecsv$MarkerNumber<198 | samplecsv$MarkerNumber>198 & samplecsv$MarkerNumber<199 | samplecsv$MarkerNumber>200 & samplecsv$MarkerNumber<200)] <- 'PostFearFixation'

#POST NEUTRAL FIXATION
samplecsv$ValenceType [which(samplecsv$MarkerNumber>17 & samplecsv$MarkerNumber<18 | samplecsv$MarkerNumber>18 & samplecsv$MarkerNumber<19 | samplecsv$MarkerNumber>19 & samplecsv$MarkerNumber<20 | samplecsv$MarkerNumber>20 & samplecsv$MarkerNumber<21 | samplecsv$MarkerNumber>21 & samplecsv$MarkerNumber<22 | samplecsv$MarkerNumber>22 & samplecsv$MarkerNumber<23 | samplecsv$MarkerNumber>23 & samplecsv$MarkerNumber<24)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>25 & samplecsv$MarkerNumber<26 | samplecsv$MarkerNumber>26 & samplecsv$MarkerNumber<27 | samplecsv$MarkerNumber>27 & samplecsv$MarkerNumber<28 | samplecsv$MarkerNumber>28 & samplecsv$MarkerNumber<29 | samplecsv$MarkerNumber>29 & samplecsv$MarkerNumber<30 | samplecsv$MarkerNumber>30 & samplecsv$MarkerNumber<31 | samplecsv$MarkerNumber>31 & samplecsv$MarkerNumber<32)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>49 & samplecsv$MarkerNumber<50 | samplecsv$MarkerNumber>50 & samplecsv$MarkerNumber<51 | samplecsv$MarkerNumber>51 & samplecsv$MarkerNumber<52 | samplecsv$MarkerNumber>52 & samplecsv$MarkerNumber<53 | samplecsv$MarkerNumber>53 & samplecsv$MarkerNumber<54 | samplecsv$MarkerNumber>54 & samplecsv$MarkerNumber<55 | samplecsv$MarkerNumber>55 & samplecsv$MarkerNumber<56)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>57 & samplecsv$MarkerNumber<58 | samplecsv$MarkerNumber>58 & samplecsv$MarkerNumber<59 | samplecsv$MarkerNumber>59 & samplecsv$MarkerNumber<60 | samplecsv$MarkerNumber>60 & samplecsv$MarkerNumber<61 | samplecsv$MarkerNumber>61 & samplecsv$MarkerNumber<62 | samplecsv$MarkerNumber>62 & samplecsv$MarkerNumber<63 | samplecsv$MarkerNumber>63 & samplecsv$MarkerNumber<64)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>65 & samplecsv$MarkerNumber<66 | samplecsv$MarkerNumber>66 & samplecsv$MarkerNumber<67 | samplecsv$MarkerNumber>67 & samplecsv$MarkerNumber<68 | samplecsv$MarkerNumber>68 & samplecsv$MarkerNumber<69 | samplecsv$MarkerNumber>69 & samplecsv$MarkerNumber<70 | samplecsv$MarkerNumber>70 & samplecsv$MarkerNumber<71 | samplecsv$MarkerNumber>71 & samplecsv$MarkerNumber<72)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>81 & samplecsv$MarkerNumber<82 | samplecsv$MarkerNumber>82 & samplecsv$MarkerNumber<83 | samplecsv$MarkerNumber>83 & samplecsv$MarkerNumber<84 | samplecsv$MarkerNumber>84 & samplecsv$MarkerNumber<85 | samplecsv$MarkerNumber>85 & samplecsv$MarkerNumber<86 | samplecsv$MarkerNumber>86 & samplecsv$MarkerNumber<87 | samplecsv$MarkerNumber>87 & samplecsv$MarkerNumber<88)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>89 & samplecsv$MarkerNumber<90 | samplecsv$MarkerNumber>90 & samplecsv$MarkerNumber<91 | samplecsv$MarkerNumber>91 & samplecsv$MarkerNumber<92 | samplecsv$MarkerNumber>92 & samplecsv$MarkerNumber<93 | samplecsv$MarkerNumber>93 & samplecsv$MarkerNumber<94 | samplecsv$MarkerNumber>94 & samplecsv$MarkerNumber<95 | samplecsv$MarkerNumber>95 & samplecsv$MarkerNumber<96)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>105 & samplecsv$MarkerNumber<106 | samplecsv$MarkerNumber>106 & samplecsv$MarkerNumber<107 | samplecsv$MarkerNumber>107 & samplecsv$MarkerNumber<108 | samplecsv$MarkerNumber>108 & samplecsv$MarkerNumber<109 | samplecsv$MarkerNumber>109 & samplecsv$MarkerNumber<110 | samplecsv$MarkerNumber>110 & samplecsv$MarkerNumber<111 | samplecsv$MarkerNumber>111 & samplecsv$MarkerNumber<112)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>129 & samplecsv$MarkerNumber<130 | samplecsv$MarkerNumber>130 & samplecsv$MarkerNumber<131 | samplecsv$MarkerNumber>131 & samplecsv$MarkerNumber<132 | samplecsv$MarkerNumber>132 & samplecsv$MarkerNumber<133 | samplecsv$MarkerNumber>133 & samplecsv$MarkerNumber<134 | samplecsv$MarkerNumber>134 & samplecsv$MarkerNumber<135 | samplecsv$MarkerNumber>135 & samplecsv$MarkerNumber<136)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>137 & samplecsv$MarkerNumber<138 | samplecsv$MarkerNumber>138 & samplecsv$MarkerNumber<139 | samplecsv$MarkerNumber>139 & samplecsv$MarkerNumber<140 | samplecsv$MarkerNumber>140 & samplecsv$MarkerNumber<141 | samplecsv$MarkerNumber>141 & samplecsv$MarkerNumber<142 | samplecsv$MarkerNumber>142 & samplecsv$MarkerNumber<143 | samplecsv$MarkerNumber>143 & samplecsv$MarkerNumber<144)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>169 & samplecsv$MarkerNumber<170 | samplecsv$MarkerNumber>170 & samplecsv$MarkerNumber<171 | samplecsv$MarkerNumber>171 & samplecsv$MarkerNumber<172 | samplecsv$MarkerNumber>172 & samplecsv$MarkerNumber<173 | samplecsv$MarkerNumber>173 & samplecsv$MarkerNumber<174 | samplecsv$MarkerNumber>174 & samplecsv$MarkerNumber<175 | samplecsv$MarkerNumber>175 & samplecsv$MarkerNumber<176)] <- 'PostNeutralFixation'

samplecsv$ValenceType [which(samplecsv$MarkerNumber>185 & samplecsv$MarkerNumber<186 | samplecsv$MarkerNumber>186 & samplecsv$MarkerNumber<187 | samplecsv$MarkerNumber>187 & samplecsv$MarkerNumber<188 | samplecsv$MarkerNumber>188 & samplecsv$MarkerNumber<189 | samplecsv$MarkerNumber>189 & samplecsv$MarkerNumber<190 | samplecsv$MarkerNumber>190 & samplecsv$MarkerNumber<191 | samplecsv$MarkerNumber>191 & samplecsv$MarkerNumber<192)] <- 'PostNeutralFixation'

#POST ASIAN FIXATION
samplecsv$FaceType [which(samplecsv$MarkerNumber>9 & samplecsv$MarkerNumber<10 | samplecsv$MarkerNumber>10 & samplecsv$MarkerNumber<11 | samplecsv$MarkerNumber>11 & samplecsv$MarkerNumber<12 | samplecsv$MarkerNumber>12 & samplecsv$MarkerNumber<13 | samplecsv$MarkerNumber>13 & samplecsv$MarkerNumber<14 | samplecsv$MarkerNumber>14 & samplecsv$MarkerNumber<15 | samplecsv$MarkerNumber>15 & samplecsv$MarkerNumber<16)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>17 & samplecsv$MarkerNumber<18 | samplecsv$MarkerNumber>18 & samplecsv$MarkerNumber<19 | samplecsv$MarkerNumber>19 & samplecsv$MarkerNumber<20 | samplecsv$MarkerNumber>20 & samplecsv$MarkerNumber<21 | samplecsv$MarkerNumber>21 & samplecsv$MarkerNumber<22 | samplecsv$MarkerNumber>22 & samplecsv$MarkerNumber<23 | samplecsv$MarkerNumber>23 & samplecsv$MarkerNumber<24)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>41 & samplecsv$MarkerNumber<42 | samplecsv$MarkerNumber>42 & samplecsv$MarkerNumber<43 | samplecsv$MarkerNumber>43 & samplecsv$MarkerNumber<44 | samplecsv$MarkerNumber>44 & samplecsv$MarkerNumber<45 | samplecsv$MarkerNumber>45 & samplecsv$MarkerNumber<46 | samplecsv$MarkerNumber>46 & samplecsv$MarkerNumber<47 | samplecsv$MarkerNumber>47 & samplecsv$MarkerNumber<48)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>65 & samplecsv$MarkerNumber<66 | samplecsv$MarkerNumber>66 & samplecsv$MarkerNumber<67 | samplecsv$MarkerNumber>67 & samplecsv$MarkerNumber<68 | samplecsv$MarkerNumber>68 & samplecsv$MarkerNumber<69 | samplecsv$MarkerNumber>69 & samplecsv$MarkerNumber<70 | samplecsv$MarkerNumber>70 & samplecsv$MarkerNumber<71 | samplecsv$MarkerNumber>71 & samplecsv$MarkerNumber<72)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>89 & samplecsv$MarkerNumber<90 | samplecsv$MarkerNumber>90 & samplecsv$MarkerNumber<91 | samplecsv$MarkerNumber>91 & samplecsv$MarkerNumber<92 | samplecsv$MarkerNumber>92 & samplecsv$MarkerNumber<93 | samplecsv$MarkerNumber>93 & samplecsv$MarkerNumber<94 | samplecsv$MarkerNumber>94 & samplecsv$MarkerNumber<95 | samplecsv$MarkerNumber>95 & samplecsv$MarkerNumber<96)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>97 & samplecsv$MarkerNumber<98 | samplecsv$MarkerNumber>98 & samplecsv$MarkerNumber<99 | samplecsv$MarkerNumber>99 & samplecsv$MarkerNumber<100 | samplecsv$MarkerNumber>100 & samplecsv$MarkerNumber<101 | samplecsv$MarkerNumber>101 & samplecsv$MarkerNumber<102 | samplecsv$MarkerNumber>102 & samplecsv$MarkerNumber<103 | samplecsv$MarkerNumber>103 & samplecsv$MarkerNumber<104)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>105 & samplecsv$MarkerNumber<106 | samplecsv$MarkerNumber>106 & samplecsv$MarkerNumber<107 | samplecsv$MarkerNumber>107 & samplecsv$MarkerNumber<108 | samplecsv$MarkerNumber>108 & samplecsv$MarkerNumber<109 | samplecsv$MarkerNumber>109 & samplecsv$MarkerNumber<110 | samplecsv$MarkerNumber>110 & samplecsv$MarkerNumber<111 | samplecsv$MarkerNumber>111 & samplecsv$MarkerNumber<112)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>121 & samplecsv$MarkerNumber<122 | samplecsv$MarkerNumber>122 & samplecsv$MarkerNumber<123 | samplecsv$MarkerNumber>123 & samplecsv$MarkerNumber<124 | samplecsv$MarkerNumber>124 & samplecsv$MarkerNumber<125 | samplecsv$MarkerNumber>125 & samplecsv$MarkerNumber<126 | samplecsv$MarkerNumber>126 & samplecsv$MarkerNumber<127 | samplecsv$MarkerNumber>127 & samplecsv$MarkerNumber<128)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>153 & samplecsv$MarkerNumber<154 | samplecsv$MarkerNumber>154 & samplecsv$MarkerNumber<155 | samplecsv$MarkerNumber>155 & samplecsv$MarkerNumber<156 | samplecsv$MarkerNumber>156 & samplecsv$MarkerNumber<157 | samplecsv$MarkerNumber>157 & samplecsv$MarkerNumber<158 | samplecsv$MarkerNumber>158 & samplecsv$MarkerNumber<159 | samplecsv$MarkerNumber>159 & samplecsv$MarkerNumber<160)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>169 & samplecsv$MarkerNumber<170 | samplecsv$MarkerNumber>170 & samplecsv$MarkerNumber<171 | samplecsv$MarkerNumber>171 & samplecsv$MarkerNumber<172 | samplecsv$MarkerNumber>172 & samplecsv$MarkerNumber<173 | samplecsv$MarkerNumber>173 & samplecsv$MarkerNumber<174 | samplecsv$MarkerNumber>174 & samplecsv$MarkerNumber<175 | samplecsv$MarkerNumber>175 & samplecsv$MarkerNumber<176)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>177 & samplecsv$MarkerNumber<178 | samplecsv$MarkerNumber>178 & samplecsv$MarkerNumber<179 | samplecsv$MarkerNumber>179 & samplecsv$MarkerNumber<180 | samplecsv$MarkerNumber>180 & samplecsv$MarkerNumber<181 | samplecsv$MarkerNumber>181 & samplecsv$MarkerNumber<182 | samplecsv$MarkerNumber>182 & samplecsv$MarkerNumber<183 | samplecsv$MarkerNumber>183 & samplecsv$MarkerNumber<184)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>185 & samplecsv$MarkerNumber<186 | samplecsv$MarkerNumber>186 & samplecsv$MarkerNumber<187 | samplecsv$MarkerNumber>187 & samplecsv$MarkerNumber<188 | samplecsv$MarkerNumber>188 & samplecsv$MarkerNumber<189 | samplecsv$MarkerNumber>189 & samplecsv$MarkerNumber<190 | samplecsv$MarkerNumber>190 & samplecsv$MarkerNumber<191 | samplecsv$MarkerNumber>191 & samplecsv$MarkerNumber<192)] <- 'PostAsianFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>193 & samplecsv$MarkerNumber<194 | samplecsv$MarkerNumber>194 & samplecsv$MarkerNumber<195 | samplecsv$MarkerNumber>195 & samplecsv$MarkerNumber<196 | samplecsv$MarkerNumber>196 & samplecsv$MarkerNumber<197 | samplecsv$MarkerNumber>197 & samplecsv$MarkerNumber<198 | samplecsv$MarkerNumber>198 & samplecsv$MarkerNumber<199 | samplecsv$MarkerNumber>200 & samplecsv$MarkerNumber<200)] <- 'PostAsianFixation'

#POST CAUCASION FIXATION
samplecsv$FaceType [which(samplecsv$MarkerNumber>1 & samplecsv$MarkerNumber<2 | samplecsv$MarkerNumber>2 & samplecsv$MarkerNumber<3 | samplecsv$MarkerNumber>3 & samplecsv$MarkerNumber<4 | samplecsv$MarkerNumber>4 & samplecsv$MarkerNumber<5 | samplecsv$MarkerNumber>5 & samplecsv$MarkerNumber<6 | samplecsv$MarkerNumber>6 & samplecsv$MarkerNumber<7 | samplecsv$MarkerNumber>7 & samplecsv$MarkerNumber<8)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>25 & samplecsv$MarkerNumber<26 | samplecsv$MarkerNumber>26 & samplecsv$MarkerNumber<27 | samplecsv$MarkerNumber>27 & samplecsv$MarkerNumber<28 | samplecsv$MarkerNumber>28 & samplecsv$MarkerNumber<29 | samplecsv$MarkerNumber>29 & samplecsv$MarkerNumber<30 | samplecsv$MarkerNumber>30 & samplecsv$MarkerNumber<31 | samplecsv$MarkerNumber>31 & samplecsv$MarkerNumber<32)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>33 & samplecsv$MarkerNumber<34 | samplecsv$MarkerNumber>34 & samplecsv$MarkerNumber<35 | samplecsv$MarkerNumber>35 & samplecsv$MarkerNumber<36 | samplecsv$MarkerNumber>36 & samplecsv$MarkerNumber<37 | samplecsv$MarkerNumber>37 & samplecsv$MarkerNumber<38 | samplecsv$MarkerNumber>38 & samplecsv$MarkerNumber<39 | samplecsv$MarkerNumber>39 & samplecsv$MarkerNumber<40)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>49 & samplecsv$MarkerNumber<50 | samplecsv$MarkerNumber>50 & samplecsv$MarkerNumber<51 | samplecsv$MarkerNumber>51 & samplecsv$MarkerNumber<52 | samplecsv$MarkerNumber>52 & samplecsv$MarkerNumber<53 | samplecsv$MarkerNumber>53 & samplecsv$MarkerNumber<54 | samplecsv$MarkerNumber>54 & samplecsv$MarkerNumber<55 | samplecsv$MarkerNumber>55 & samplecsv$MarkerNumber<56)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>57 & samplecsv$MarkerNumber<58 | samplecsv$MarkerNumber>58 & samplecsv$MarkerNumber<59 | samplecsv$MarkerNumber>59 & samplecsv$MarkerNumber<60 | samplecsv$MarkerNumber>60 & samplecsv$MarkerNumber<61 | samplecsv$MarkerNumber>61 & samplecsv$MarkerNumber<62 | samplecsv$MarkerNumber>62 & samplecsv$MarkerNumber<63 | samplecsv$MarkerNumber>63 & samplecsv$MarkerNumber<64)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>73 & samplecsv$MarkerNumber<74 | samplecsv$MarkerNumber>74 & samplecsv$MarkerNumber<75 | samplecsv$MarkerNumber>75 & samplecsv$MarkerNumber<76 | samplecsv$MarkerNumber>76 & samplecsv$MarkerNumber<77 | samplecsv$MarkerNumber>77 & samplecsv$MarkerNumber<78 | samplecsv$MarkerNumber>78 & samplecsv$MarkerNumber<79 | samplecsv$MarkerNumber>79 & samplecsv$MarkerNumber<80)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>81 & samplecsv$MarkerNumber<82 | samplecsv$MarkerNumber>82 & samplecsv$MarkerNumber<83 | samplecsv$MarkerNumber>83 & samplecsv$MarkerNumber<84 | samplecsv$MarkerNumber>84 & samplecsv$MarkerNumber<85 | samplecsv$MarkerNumber>85 & samplecsv$MarkerNumber<86 | samplecsv$MarkerNumber>86 & samplecsv$MarkerNumber<87 | samplecsv$MarkerNumber>87 & samplecsv$MarkerNumber<88)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>113 & samplecsv$MarkerNumber<114 | samplecsv$MarkerNumber>114 & samplecsv$MarkerNumber<115 | samplecsv$MarkerNumber>115 & samplecsv$MarkerNumber<116 | samplecsv$MarkerNumber>116 & samplecsv$MarkerNumber<117 | samplecsv$MarkerNumber>117 & samplecsv$MarkerNumber<118 | samplecsv$MarkerNumber>118 & samplecsv$MarkerNumber<119 | samplecsv$MarkerNumber>119 & samplecsv$MarkerNumber<120)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>129 & samplecsv$MarkerNumber<130 | samplecsv$MarkerNumber>130 & samplecsv$MarkerNumber<131 | samplecsv$MarkerNumber>131 & samplecsv$MarkerNumber<132 | samplecsv$MarkerNumber>132 & samplecsv$MarkerNumber<133 | samplecsv$MarkerNumber>133 & samplecsv$MarkerNumber<134 | samplecsv$MarkerNumber>134 & samplecsv$MarkerNumber<135 | samplecsv$MarkerNumber>135 & samplecsv$MarkerNumber<136)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>137 & samplecsv$MarkerNumber<138 | samplecsv$MarkerNumber>138 & samplecsv$MarkerNumber<139 | samplecsv$MarkerNumber>139 & samplecsv$MarkerNumber<140 | samplecsv$MarkerNumber>140 & samplecsv$MarkerNumber<141 | samplecsv$MarkerNumber>141 & samplecsv$MarkerNumber<142 | samplecsv$MarkerNumber>142 & samplecsv$MarkerNumber<143 | samplecsv$MarkerNumber>143 & samplecsv$MarkerNumber<144)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>145 & samplecsv$MarkerNumber<146 | samplecsv$MarkerNumber>146 & samplecsv$MarkerNumber<147 | samplecsv$MarkerNumber>147 & samplecsv$MarkerNumber<148 | samplecsv$MarkerNumber>148 & samplecsv$MarkerNumber<149 | samplecsv$MarkerNumber>149 & samplecsv$MarkerNumber<150 | samplecsv$MarkerNumber>150 & samplecsv$MarkerNumber<151 | samplecsv$MarkerNumber>151 & samplecsv$MarkerNumber<152)] <- 'PostCaucasionFixation'

samplecsv$FaceType [which(samplecsv$MarkerNumber>161 & samplecsv$MarkerNumber<162 | samplecsv$MarkerNumber>162 & samplecsv$MarkerNumber<163 | samplecsv$MarkerNumber>163 & samplecsv$MarkerNumber<164 | samplecsv$MarkerNumber>164 & samplecsv$MarkerNumber<165 | samplecsv$MarkerNumber>165 & samplecsv$MarkerNumber<166 | samplecsv$MarkerNumber>166 & samplecsv$MarkerNumber<167 | samplecsv$MarkerNumber>167 & samplecsv$MarkerNumber<168)] <- 'PostCaucasionFixation'

#Also label the interblock fixations as baseline fixations
samplecsv$TaskCue [which(samplecsv$MarkerNumber>16 & samplecsv$MarkerNumber<17 | samplecsv$MarkerNumber>24 & samplecsv$MarkerNumber<25 | samplecsv$MarkerNumber>32 & samplecsv$MarkerNumber<33 | samplecsv$MarkerNumber>40 & samplecsv$MarkerNumber<41 | samplecsv$MarkerNumber>48 & samplecsv$MarkerNumber<49 | samplecsv$MarkerNumber>56 & samplecsv$MarkerNumber<57 | samplecsv$MarkerNumber>64 & samplecsv$MarkerNumber<65)] <- "BaselineFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>80 & samplecsv$MarkerNumber<81 | samplecsv$MarkerNumber>88 & samplecsv$MarkerNumber<89 | samplecsv$MarkerNumber>96 & samplecsv$MarkerNumber<97 | samplecsv$MarkerNumber>104 & samplecsv$MarkerNumber<105 | samplecsv$MarkerNumber>112 & samplecsv$MarkerNumber<113 | samplecsv$MarkerNumber>120 & samplecsv$MarkerNumber<121 | samplecsv$MarkerNumber>128 & samplecsv$MarkerNumber<129)] <- "BaselineFixation"

samplecsv$TaskCue [which(samplecsv$MarkerNumber>144 & samplecsv$MarkerNumber<145 | samplecsv$MarkerNumber>152 & samplecsv$MarkerNumber<153 | samplecsv$MarkerNumber>160 & samplecsv$MarkerNumber<161 | samplecsv$MarkerNumber>168 & samplecsv$MarkerNumber<169 | samplecsv$MarkerNumber>176 & samplecsv$MarkerNumber<177 | samplecsv$MarkerNumber>184 & samplecsv$MarkerNumber<185 | samplecsv$MarkerNumber>192 & samplecsv$MarkerNumber<193)] <- "BaselineFixation"

samplecsv$ValenceType [which(samplecsv$MarkerNumber>16 & samplecsv$MarkerNumber<17 | samplecsv$MarkerNumber>24 & samplecsv$MarkerNumber<25 | samplecsv$MarkerNumber>32 & samplecsv$MarkerNumber<33 | samplecsv$MarkerNumber>40 & samplecsv$MarkerNumber<41 | samplecsv$MarkerNumber>48 & samplecsv$MarkerNumber<49 | samplecsv$MarkerNumber>56 & samplecsv$MarkerNumber<57 | samplecsv$MarkerNumber>64 & samplecsv$MarkerNumber<65)] <- "BaselineFixation"

samplecsv$ValenceType [which(samplecsv$MarkerNumber>80 & samplecsv$MarkerNumber<81 | samplecsv$MarkerNumber>88 & samplecsv$MarkerNumber<89 | samplecsv$MarkerNumber>96 & samplecsv$MarkerNumber<97 | samplecsv$MarkerNumber>104 & samplecsv$MarkerNumber<105 | samplecsv$MarkerNumber>112 & samplecsv$MarkerNumber<113 | samplecsv$MarkerNumber>120 & samplecsv$MarkerNumber<121 | samplecsv$MarkerNumber>128 & samplecsv$MarkerNumber<129)] <- "BaselineFixation"

samplecsv$ValenceType [which(samplecsv$MarkerNumber>144 & samplecsv$MarkerNumber<145 | samplecsv$MarkerNumber>152 & samplecsv$MarkerNumber<153 | samplecsv$MarkerNumber>160 & samplecsv$MarkerNumber<161 | samplecsv$MarkerNumber>168 & samplecsv$MarkerNumber<169 | samplecsv$MarkerNumber>176 & samplecsv$MarkerNumber<177 | samplecsv$MarkerNumber>184 & samplecsv$MarkerNumber<185 | samplecsv$MarkerNumber>192 & samplecsv$MarkerNumber<193)] <- "BaselineFixation"

samplecsv$FaceType [which(samplecsv$MarkerNumber>16 & samplecsv$MarkerNumber<17 | samplecsv$MarkerNumber>24 & samplecsv$MarkerNumber<25 | samplecsv$MarkerNumber>32 & samplecsv$MarkerNumber<33 | samplecsv$MarkerNumber>40 & samplecsv$MarkerNumber<41 | samplecsv$MarkerNumber>48 & samplecsv$MarkerNumber<49 | samplecsv$MarkerNumber>56 & samplecsv$MarkerNumber<57 | samplecsv$MarkerNumber>64 & samplecsv$MarkerNumber<65)] <- "BaselineFixation"

samplecsv$FaceType [which(samplecsv$MarkerNumber>80 & samplecsv$MarkerNumber<81 | samplecsv$MarkerNumber>88 & samplecsv$MarkerNumber<89 | samplecsv$MarkerNumber>96 & samplecsv$MarkerNumber<97 | samplecsv$MarkerNumber>104 & samplecsv$MarkerNumber<105 | samplecsv$MarkerNumber>112 & samplecsv$MarkerNumber<113 | samplecsv$MarkerNumber>120 & samplecsv$MarkerNumber<121 | samplecsv$MarkerNumber>128 & samplecsv$MarkerNumber<129)] <- "BaselineFixation"

samplecsv$FaceType [which(samplecsv$MarkerNumber>144 & samplecsv$MarkerNumber<145 | samplecsv$MarkerNumber>152 & samplecsv$MarkerNumber<153 | samplecsv$MarkerNumber>160 & samplecsv$MarkerNumber<161 | samplecsv$MarkerNumber>168 & samplecsv$MarkerNumber<169 | samplecsv$MarkerNumber>176 & samplecsv$MarkerNumber<177 | samplecsv$MarkerNumber>184 & samplecsv$MarkerNumber<185 | samplecsv$MarkerNumber>192 & samplecsv$MarkerNumber<193)] <- "BaselineFixation"





#re-label fixations to reflect pre vs post stimulus fixations
samplecsv$TaskCue [which(samplecsv$TaskCue == "PostInOutFixation" & samplecsv$PupilaryLatencyAll>=-2500 & samplecsv$PupilaryLatencyAll<=-.001)] <- "PreInOutFixation" 

samplecsv$TaskCue [which(samplecsv$TaskCue == "PostMaleFemaleFixation" & samplecsv$PupilaryLatencyAll>=-2500 & samplecsv$PupilaryLatencyAll<=-.001)] <- "PreMaleFemaleFixation" 

samplecsv$TaskCue [which(samplecsv$TaskCue == "PostLikeDislikeFixation" & samplecsv$PupilaryLatencyAll>=-2500 & samplecsv$PupilaryLatencyAll<=-.001)] <- "PreLikeDislikeFixation" 

samplecsv$ValenceType [which(samplecsv$ValenceType == "PostFearFixation" & samplecsv$PupilaryLatencyAll>=-2500 & samplecsv$PupilaryLatencyAll<=-.001)] <- "PreFearFixation" 

samplecsv$ValenceType [which(samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$PupilaryLatencyAll>=-2500 & samplecsv$PupilaryLatencyAll<=-.001)] <- "PreNeutralFixation" 

samplecsv$FaceType [which(samplecsv$FaceType == "PostAsianFixation" & samplecsv$PupilaryLatencyAll>=-2500 & samplecsv$PupilaryLatencyAll<=-.001)] <- "PreAsianFixation" 

samplecsv$FaceType [which(samplecsv$FaceType == "PostCaucasionFixation" & samplecsv$PupilaryLatencyAll>=-2500 & samplecsv$PupilaryLatencyAll<=-.001)] <- "PreCaucasionFixation" 





write.table(samplecsv,file="/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv",sep=",",row.names=F)



#samplecsv
samplecsv$FactorsAll <- NA

samplecsv$FactorsAll [which(samplecsv$TaskCue == "InOut" & samplecsv$FaceType == "Asian" & samplecsv$ValenceType == "Fear")] <- "AI/OF" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "MaleFemale" & samplecsv$FaceType == "Asian" & samplecsv$ValenceType == "Fear")] <- "AM/FF" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "LikeDislike" & samplecsv$FaceType == "Asian" & samplecsv$ValenceType == "Fear")] <- "AL/DF" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "InOut" & samplecsv$FaceType == "Asian" & samplecsv$ValenceType == "Neutral")] <- "AI/ON" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "MaleFemale" & samplecsv$FaceType == "Asian" & samplecsv$ValenceType == "Neutral")] <- "AM/FN" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "LikeDislike" & samplecsv$FaceType == "Asian" & samplecsv$ValenceType == "Neutral")] <- "AL/DN" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "InOut" & samplecsv$FaceType == "Caucasion" & samplecsv$ValenceType == "Fear")] <- "CI/OF" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "MaleFemale" & samplecsv$FaceType == "Caucasion" & samplecsv$ValenceType == "Fear")] <- "CM/FF" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "LikeDislike" & samplecsv$FaceType == "Caucasion" & samplecsv$ValenceType == "Fear")] <- "CL/DF" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "InOut" & samplecsv$FaceType == "Caucasion" & samplecsv$ValenceType == "Neutral")] <- "CI/ON" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "MaleFemale" & samplecsv$FaceType == "Caucasion" & samplecsv$ValenceType == "Neutral")] <- "CM/FN" 
samplecsv$FactorsAll [which(samplecsv$TaskCue == "LikeDislike" & samplecsv$FaceType == "Caucasion" & samplecsv$ValenceType == "Neutral")] <- "CL/DN" 


#Also make columns for 2 of the 3 dependent factors
samplecsv$ValenceFace <- NA
samplecsv$ValenceTaskCue <- NA
samplecsv$FaceTaskCue <- NA

samplecsv$ValenceFace [which(samplecsv$ValenceType == "Fear" & samplecsv$FaceType == "Asian")] <- "AsianFear"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "Fear" & samplecsv$FaceType == "Caucasion")] <- "CaucasionFear"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "Neutral" & samplecsv$FaceType == "Asian")] <- "AsianNeutral"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "Neutral" & samplecsv$FaceType == "Caucasion")] <- "CaucasionNeutral"
#break
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "Fear" & samplecsv$TaskCue == "InOut")] <- "InOutFear"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "Fear" & samplecsv$TaskCue == "MaleFemale")] <- "MaleFemaleFear"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "Fear" & samplecsv$TaskCue == "LikeDislike")] <- "LikeDislikeFear"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "Neutral" & samplecsv$TaskCue == "InOut")] <- "InOutNeutral"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "Neutral" & samplecsv$TaskCue == "MaleFemale")] <- "MaleFemaleNeutral"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "Neutral" & samplecsv$TaskCue == "LikeDislike")] <- "LikeDislikeNeutral"
#break
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "Asian" & samplecsv$TaskCue == "InOut")] <- "InOutAsian"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "Asian" & samplecsv$TaskCue == "MaleFemale")] <- "MaleFemaleAsian"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "Asian" & samplecsv$TaskCue == "LikeDislike")] <- "LikeDislikeAsian"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "Caucasion" & samplecsv$TaskCue == "InOut")] <- "InOutCaucasion"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "Caucasion" & samplecsv$TaskCue == "MaleFemale")] <- "MaleFemaleCaucasion"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "Caucasion" & samplecsv$TaskCue == "LikeDislike")] <- "LikeDislikeCaucasion"

##########
#Label Fixations!
#FactorsAll
#MaleFemale
samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostMaleFemaleFixation" & samplecsv$ValenceType == "PostFearFixation" & samplecsv$FaceType == "PostAsianFixation")] <- "PostMF-F-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostMaleFemaleFixation" & samplecsv$ValenceType == "PostFearFixation" & samplecsv$FaceType == "PostCaucasionFixation")] <- "PostMF-F-C-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostMaleFemaleFixation" & samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$FaceType == "PostAsianFixation")] <- "PostMF-N-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostMaleFemaleFixation" & samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$FaceType == "PostCaucasionFixation")] <- "PostMF-N-C-Fixation"
#LikeDislike
samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostLikeDislikeFixation" & samplecsv$ValenceType == "PostFearFixation" & samplecsv$FaceType == "PostAsianFixation")] <- "PostLD-F-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostLikeDislikeFixation" & samplecsv$ValenceType == "PostFearFixation" & samplecsv$FaceType == "PostCaucasionFixation")] <- "PostLD-F-C-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostLikeDislikeFixation" & samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$FaceType == "PostAsianFixation")] <- "PostLD-N-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostLikeDislikeFixation" & samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$FaceType == "PostCaucasionFixation")] <- "PostLD-N-C-Fixation"
#InOut
samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostInOutFixation" & samplecsv$ValenceType == "PostFearFixation" & samplecsv$FaceType == "PostAsianFixation")] <- "PostIO-F-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostInOutFixation" & samplecsv$ValenceType == "PostFearFixation" & samplecsv$FaceType == "PostCaucasionFixation")] <- "PostIO-F-C-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostInOutFixation" & samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$FaceType == "PostAsianFixation")] <- "PostIO-N-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PostInOutFixation" & samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$FaceType == "PostCaucasionFixation")] <- "PostIO-N-C-Fixation"
#ValenceFace Only
samplecsv$ValenceFace [which(samplecsv$ValenceType == "PostFearFixation" & samplecsv$FaceType == "PostAsianFixation")] <- "Post-F-A-Fixation"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "PostFearFixation" & samplecsv$FaceType == "PostCaucasionFixation")] <- "Post-F-C-Fixation"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$FaceType == "PostAsianFixation")] <- "Post-N-A-Fixation"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$FaceType == "PostCaucasionFixation")] <- "Post-N-C-Fixation"

#ValenceTaskCue
#MaleFemale
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PostFearFixation" & samplecsv$TaskCue == "PostMaleFemaleFixation")] <- "Post-F-MF-Fixation"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$TaskCue == "PostMaleFemaleFixation")] <- "Post-N-MF-Fixation"
#InOut
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PostFearFixation" & samplecsv$TaskCue == "PostInOutFixation")] <- "Post-F-IO-Fixation"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$TaskCue == "PostInOutFixation")] <- "Post-N-IO-Fixation"
#LikeDislike
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PostFearFixation" & samplecsv$TaskCue == "PostLikeDislikeFixation")] <- "Post-F-LD-Fixation"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PostNeutralFixation" & samplecsv$TaskCue == "PostLikeDislikeFixation")] <- "Post-N-LD-Fixation"

#FaceTaskCue
#MaleFemale
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PostAsianFixation" & samplecsv$TaskCue == "PostMaleFemaleFixation")] <- "Post-A-MF-Fixation"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PostCaucasionFixation" & samplecsv$TaskCue == "PostMaleFemaleFixation")] <- "Post-C-MF-Fixation"
#InOut
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PostAsianFixation" & samplecsv$TaskCue == "PostInOutFixation")] <- "Post-A-IO-Fixation"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PostCaucasionFixation" & samplecsv$TaskCue == "PostInOutFixation")] <- "Post-C-IO-Fixation"
#LikeDislike
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PostAsianFixation" & samplecsv$TaskCue == "PostLikeDislikeFixation")] <- "Post-A-LD-Fixation"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PostCaucasionFixation" & samplecsv$TaskCue == "PostLikeDislikeFixation")] <- "Post-C-LD-Fixation"


#label pre stimuli fixations too
samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreMaleFemaleFixation" & samplecsv$ValenceType == "PreFearFixation" & samplecsv$FaceType == "PreAsianFixation")] <- "PreMF-F-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreMaleFemaleFixation" & samplecsv$ValenceType == "PreFearFixation" & samplecsv$FaceType == "PreCaucasionFixation")] <- "PreMF-F-C-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreMaleFemaleFixation" & samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$FaceType == "PreAsianFixation")] <- "PreMF-N-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreMaleFemaleFixation" & samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$FaceType == "PreCaucasionFixation")] <- "PreMF-N-C-Fixation"
#LikeDislike
samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreLikeDislikeFixation" & samplecsv$ValenceType == "PreFearFixation" & samplecsv$FaceType == "PreAsianFixation")] <- "PreLD-F-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreLikeDislikeFixation" & samplecsv$ValenceType == "PreFearFixation" & samplecsv$FaceType == "PreCaucasionFixation")] <- "PreLD-F-C-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreLikeDislikeFixation" & samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$FaceType == "PreAsianFixation")] <- "PreLD-N-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreLikeDislikeFixation" & samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$FaceType == "PreCaucasionFixation")] <- "PreLD-N-C-Fixation"
#InOut
samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreInOutFixation" & samplecsv$ValenceType == "PreFearFixation" & samplecsv$FaceType == "PreAsianFixation")] <- "PreIO-F-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreInOutFixation" & samplecsv$ValenceType == "PreFearFixation" & samplecsv$FaceType == "PreCaucasionFixation")] <- "PreIO-F-C-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreInOutFixation" & samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$FaceType == "PreAsianFixation")] <- "PreIO-N-A-Fixation"

samplecsv$FactorsAll [which(samplecsv$TaskCue == "PreInOutFixation" & samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$FaceType == "PreCaucasionFixation")] <- "PreIO-N-C-Fixation"
#ValenceFace Only
samplecsv$ValenceFace [which(samplecsv$ValenceType == "PreFearFixation" & samplecsv$FaceType == "PreAsianFixation")] <- "Pre-F-A-Fixation"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "PreFearFixation" & samplecsv$FaceType == "PreCaucasionFixation")] <- "Pre-F-C-Fixation"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$FaceType == "PreAsianFixation")] <- "Pre-N-A-Fixation"
samplecsv$ValenceFace [which(samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$FaceType == "PreCaucasionFixation")] <- "Pre-N-C-Fixation"
#ValenceTaskCue
#MaleFemale
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PreFearFixation" & samplecsv$TaskCue == "PreMaleFemaleFixation")] <- "Pre-F-MF-Fixation"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$TaskCue == "PreMaleFemaleFixation")] <- "Pre-N-MF-Fixation"
#InOut
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PreFearFixation" & samplecsv$TaskCue == "PreInOutFixation")] <- "Pre-F-IO-Fixation"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$TaskCue == "PreInOutFixation")] <- "Pre-N-IO-Fixation"
#LikeDislike
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PreFearFixation" & samplecsv$TaskCue == "PreLikeDislikeFixation")] <- "Pre-F-LD-Fixation"
samplecsv$ValenceTaskCue [which(samplecsv$ValenceType == "PreNeutralFixation" & samplecsv$TaskCue == "PreLikeDislikeFixation")] <- "Pre-N-LD-Fixation"

#FaceTaskCue
#MaleFemale
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PreAsianFixation" & samplecsv$TaskCue == "PreMaleFemaleFixation")] <- "Pre-A-MF-Fixation"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PreCaucasionFixation" & samplecsv$TaskCue == "PreMaleFemaleFixation")] <- "Pre-C-MF-Fixation"
#InOut
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PreAsianFixation" & samplecsv$TaskCue == "PreInOutFixation")] <- "Pre-A-IO-Fixation"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PreCaucasionFixation" & samplecsv$TaskCue == "PreInOutFixation")] <- "Pre-C-IO-Fixation"
#LikeDislike
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PreAsianFixation" & samplecsv$TaskCue == "PreLikeDislikeFixation")] <- "Pre-A-LD-Fixation"
samplecsv$FaceTaskCue [which(samplecsv$FaceType == "PreCaucasionFixation" & samplecsv$TaskCue == "PreLikeDislikeFixation")] <- "Pre-C-LD-Fixation"

#Copy baseline fixations as well

samplecsv$FactorsAll [which(is.na(samplecsv$FactorsAll))] <- "BaselineFixation"
samplecsv$ValenceTaskCue [which(is.na(samplecsv$ValenceTaskCue))] <- "BaselineFixation"
samplecsv$ValenceFace [which(is.na(samplecsv$ValenceFace))] <- "BaselineFixation"
samplecsv$FaceTaskCue [which(is.na(samplecsv$FaceTaskCue))] <- "BaselineFixation"

#save csv
write.table(samplecsv,file="/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv",sep=",",row.names=F)

#Now clean rows with data data
library(plyr)
library(reshape)
##########
#count total number of rows, keep a tally so you can calculate the percentages of usable data
count(samplecsv$DataType) 
#count total during stimulus viewing
count(samplecsv$TaskCue)
count(samplecsv$ValenceType)
count(samplecsv$FaceType)
#delete rows with contain any data point that is outside of bounds
samplecsv <- samplecsv[!(samplecsv$X_Gaze>1024 | samplecsv$X_Gaze<0 | samplecsv$Y_Gaze>768 | samplecsv$Y_Gaze<0),]
#recount so you can calculate percentage of data used
count(samplecsv$DataType)
count(samplecsv$TaskCue )

#delete rows in .csv is value in column X_Gaze is sequentially repreated
samplecsvv2 <- samplecsv[which(!duplicated(samplecsv$X_Gaze)] <- 
  samplecsvv3 <- samplecsvv2[which(!duplicated(samplecsvv2$Y_Gaze)] <- 
  #double check
  which(duplicated(samplecsvv3$X_Gaze),)
#count one last time
count(samplecsvv3$DataType)
count(samplecsvv3$TaskCue )

write.table(samplecsvv3,file="/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv",sep=",",row.names=F)

#delete rows with NAs in TaskCue/ValenceType/FaceType
#These are inter block fixation where the subjects were instructed to rest. Some stretched, or left the chin rest in general
samplecsv <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv")

samplecsv <- samplecsv[complete.cases(samplecsv[,12:14]),]
count(samplecsv$DataType)
count(samplecsv$TaskCue)
#Make new columns based on combined dependent factors


#After creating columns distributing the fixations and the stimuli, it is helpful for visualization to inspect them as one variable. 

samplecsv <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv")

samplecsv$TaskCueNew <- NA
samplecsv$ValenceTypeNew <- NA
samplecsv$FaceTypeNew <- NA
samplecsv$ValenceFaceNew <- NA

samplecsv$TaskCueNew [which(samplecsv$TaskCue == "InOut" | samplecsv$TaskCue == "PostInOutFixation" | samplecsv$TaskCue == "PreInOutFixation")] <- 'InOut'

samplecsv$TaskCueNew [which(samplecsv$TaskCue == "LikeDislike" | samplecsv$TaskCue == "PostLikeDislikeFixation" | samplecsv$TaskCue == "PreLikeDislikeFixation")] <- 'LikeDislike'

samplecsv$TaskCueNew [which(samplecsv$TaskCue == "MaleFemale" | samplecsv$TaskCue == "PostMaleFemaleFixation" | samplecsv$TaskCue == "PreMaleFemaleFixation")] <- 'MaleFemale'


samplecsv$ValenceTypeNew [which(samplecsv$ValenceType == "Fear" | samplecsv$ValenceType == "PostFearFixation" | samplecsv$ValenceType == "PreFearFixation")] <- 'Fear'

samplecsv$ValenceTypeNew [which(samplecsv$ValenceType == "Neutral" | samplecsv$ValenceType == "PostNeutralFixation" | samplecsv$ValenceType == "PreNeutralFixation")] <- 'Neutral'


samplecsv$FaceTypeNew [which(samplecsv$FaceType == "Asian" | samplecsv$FaceType == "PostAsianFixation" | samplecsv$FaceType == "PreAsianFixation")] <- 'Asian'


samplecsv$FaceTypeNew [which(samplecsv$FaceType == "Caucasion" | samplecsv$FaceType == "PostCaucasionFixation" | samplecsv$FaceType == "PreCaucasionFixation")] <- 'Caucasion'


samplecsv$ValenceFaceNew [which(samplecsv$ValenceFace == "CaucasionFear" | samplecsv$ValenceFace == "Post-F-C-Fixation" | samplecsv$ValenceFace == "Pre-F-C-Fixation")] <- 'CaucasionFear'

samplecsv$ValenceFaceNew [which(samplecsv$ValenceFace == "CaucasionNeutral" | samplecsv$ValenceFace == "Post-N-C-Fixation" | samplecsv$ValenceFace == "Pre-N-C-Fixation")] <- 'CaucasionNeutral'

samplecsv$ValenceFaceNew [which(samplecsv$ValenceFace == "AsianFear" | samplecsv$ValenceFace == "Post-F-A-Fixation" | samplecsv$ValenceFace == "Pre-F-A-Fixation")] <- 'AsianFear'

samplecsv$ValenceFaceNew [which(samplecsv$ValenceFace == "AsianNeutral" | samplecsv$ValenceFace == "Post-N-A-Fixation" | samplecsv$ValenceFace == "Pre-N-A-Fixation")] <- 'AsianNeutral'



write.table(samplecsv,file="/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv",sep=",",row.names=F)

samplecsv <- read.csv("/Volumes/NO NAME/Csvgroup3-27/CSEyesamplecsv.csv")


#make a new column that defines subject
samplecsv$SubjectId <- "samplecsv"
samplecsv$Trial <- NA


samplecsv$Trial [which(samplecsv$MarkerNumber>=1 & samplecsv$MarkerNumber<2)] <- 1
samplecsv$Trial [which(samplecsv$MarkerNumber>=2 & samplecsv$MarkerNumber<3)] <- 2
samplecsv$Trial [which(samplecsv$MarkerNumber>=3 & samplecsv$MarkerNumber<4)] <- 3
samplecsv$Trial [which(samplecsv$MarkerNumber>=4 & samplecsv$MarkerNumber<5)] <- 4
samplecsv$Trial [which(samplecsv$MarkerNumber>=5 & samplecsv$MarkerNumber<6)] <- 5
samplecsv$Trial [which(samplecsv$MarkerNumber>=6 & samplecsv$MarkerNumber<7)] <- 6
samplecsv$Trial [which(samplecsv$MarkerNumber>=7 & samplecsv$MarkerNumber<8)] <- 7
samplecsv$Trial [which(samplecsv$MarkerNumber>=8 & samplecsv$MarkerNumber<9)] <- 8
samplecsv$Trial [which(samplecsv$MarkerNumber>=9 & samplecsv$MarkerNumber<10)] <- 9
samplecsv$Trial [which(samplecsv$MarkerNumber>=10 & samplecsv$MarkerNumber<11)] <- 10
samplecsv$Trial [which(samplecsv$MarkerNumber>=11 & samplecsv$MarkerNumber<12)] <- 11
samplecsv$Trial [which(samplecsv$MarkerNumber>=12 & samplecsv$MarkerNumber<13)] <- 12
samplecsv$Trial [which(samplecsv$MarkerNumber>=13 & samplecsv$MarkerNumber<14)] <- 13
samplecsv$Trial [which(samplecsv$MarkerNumber>=14 & samplecsv$MarkerNumber<15)] <- 14
samplecsv$Trial [which(samplecsv$MarkerNumber>=15 & samplecsv$MarkerNumber<16)] <- 15
samplecsv$Trial [which(samplecsv$MarkerNumber>=16 & samplecsv$MarkerNumber<17)] <- 16
samplecsv$Trial [which(samplecsv$MarkerNumber>=17 & samplecsv$MarkerNumber<18)] <- 17
samplecsv$Trial [which(samplecsv$MarkerNumber>=18 & samplecsv$MarkerNumber<19)] <- 18
samplecsv$Trial [which(samplecsv$MarkerNumber>=19 & samplecsv$MarkerNumber<20)] <- 19
samplecsv$Trial [which(samplecsv$MarkerNumber>=20 & samplecsv$MarkerNumber<21)] <- 20
samplecsv$Trial [which(samplecsv$MarkerNumber>=21 & samplecsv$MarkerNumber<22)] <- 21
samplecsv$Trial [which(samplecsv$MarkerNumber>=22 & samplecsv$MarkerNumber<23)] <- 22
samplecsv$Trial [which(samplecsv$MarkerNumber>=23 & samplecsv$MarkerNumber<24)] <- 23
samplecsv$Trial [which(samplecsv$MarkerNumber>=24 & samplecsv$MarkerNumber<25)] <- 24
samplecsv$Trial [which(samplecsv$MarkerNumber>=25 & samplecsv$MarkerNumber<26)] <- 25
samplecsv$Trial [which(samplecsv$MarkerNumber>=26 & samplecsv$MarkerNumber<27)] <- 26
samplecsv$Trial [which(samplecsv$MarkerNumber>=27 & samplecsv$MarkerNumber<28)] <- 27
samplecsv$Trial [which(samplecsv$MarkerNumber>=28 & samplecsv$MarkerNumber<29)] <- 28
samplecsv$Trial [which(samplecsv$MarkerNumber>=29 & samplecsv$MarkerNumber<30)] <- 29
samplecsv$Trial [which(samplecsv$MarkerNumber>=30 & samplecsv$MarkerNumber<31)] <- 30
samplecsv$Trial [which(samplecsv$MarkerNumber>=31 & samplecsv$MarkerNumber<32)] <- 31
samplecsv$Trial [which(samplecsv$MarkerNumber>=32 & samplecsv$MarkerNumber<33)] <- 32
samplecsv$Trial [which(samplecsv$MarkerNumber>=33 & samplecsv$MarkerNumber<34)] <- 33
samplecsv$Trial [which(samplecsv$MarkerNumber>=34 & samplecsv$MarkerNumber<35)] <- 34
samplecsv$Trial [which(samplecsv$MarkerNumber>=35 & samplecsv$MarkerNumber<36)] <- 35
samplecsv$Trial [which(samplecsv$MarkerNumber>=36 & samplecsv$MarkerNumber<37)] <- 36
samplecsv$Trial [which(samplecsv$MarkerNumber>=37 & samplecsv$MarkerNumber<38)] <- 37
samplecsv$Trial [which(samplecsv$MarkerNumber>=38 & samplecsv$MarkerNumber<39)] <- 38
samplecsv$Trial [which(samplecsv$MarkerNumber>=39 & samplecsv$MarkerNumber<40)] <- 39
samplecsv$Trial [which(samplecsv$MarkerNumber>=40 & samplecsv$MarkerNumber<41)] <- 40
samplecsv$Trial [which(samplecsv$MarkerNumber>=41 & samplecsv$MarkerNumber<42)] <- 41
samplecsv$Trial [which(samplecsv$MarkerNumber>=42 & samplecsv$MarkerNumber<43)] <- 42
samplecsv$Trial [which(samplecsv$MarkerNumber>=43 & samplecsv$MarkerNumber<44)] <- 43
samplecsv$Trial [which(samplecsv$MarkerNumber>=44 & samplecsv$MarkerNumber<45)] <- 44
samplecsv$Trial [which(samplecsv$MarkerNumber>=45 & samplecsv$MarkerNumber<46)] <- 45
samplecsv$Trial [which(samplecsv$MarkerNumber>=46 & samplecsv$MarkerNumber<47)] <- 46
samplecsv$Trial [which(samplecsv$MarkerNumber>=47 & samplecsv$MarkerNumber<48)] <- 47
samplecsv$Trial [which(samplecsv$MarkerNumber>=48 & samplecsv$MarkerNumber<49)] <- 48
samplecsv$Trial [which(samplecsv$MarkerNumber>=49 & samplecsv$MarkerNumber<50)] <- 49
samplecsv$Trial [which(samplecsv$MarkerNumber>=50 & samplecsv$MarkerNumber<51)] <- 50
samplecsv$Trial [which(samplecsv$MarkerNumber>=51 & samplecsv$MarkerNumber<52)] <- 51
samplecsv$Trial [which(samplecsv$MarkerNumber>=52 & samplecsv$MarkerNumber<53)] <- 52
samplecsv$Trial [which(samplecsv$MarkerNumber>=53 & samplecsv$MarkerNumber<54)] <- 53
samplecsv$Trial [which(samplecsv$MarkerNumber>=54 & samplecsv$MarkerNumber<55)] <- 54
samplecsv$Trial [which(samplecsv$MarkerNumber>=55 & samplecsv$MarkerNumber<56)] <- 55
samplecsv$Trial [which(samplecsv$MarkerNumber>=56 & samplecsv$MarkerNumber<57)] <- 56
samplecsv$Trial [which(samplecsv$MarkerNumber>=57 & samplecsv$MarkerNumber<58)] <- 57
samplecsv$Trial [which(samplecsv$MarkerNumber>=58 & samplecsv$MarkerNumber<59)] <- 58
samplecsv$Trial [which(samplecsv$MarkerNumber>=59 & samplecsv$MarkerNumber<60)] <- 59
samplecsv$Trial [which(samplecsv$MarkerNumber>=60 & samplecsv$MarkerNumber<61)] <- 60
samplecsv$Trial [which(samplecsv$MarkerNumber>=61 & samplecsv$MarkerNumber<62)] <- 61
samplecsv$Trial [which(samplecsv$MarkerNumber>=62 & samplecsv$MarkerNumber<63)] <- 62
samplecsv$Trial [which(samplecsv$MarkerNumber>=63 & samplecsv$MarkerNumber<64)] <- 63
samplecsv$Trial [which(samplecsv$MarkerNumber>=64 & samplecsv$MarkerNumber<65)] <- 64
samplecsv$Trial [which(samplecsv$MarkerNumber>=65 & samplecsv$MarkerNumber<66)] <- 65
samplecsv$Trial [which(samplecsv$MarkerNumber>=66 & samplecsv$MarkerNumber<67)] <- 66
samplecsv$Trial [which(samplecsv$MarkerNumber>=67 & samplecsv$MarkerNumber<68)] <- 67
samplecsv$Trial [which(samplecsv$MarkerNumber>=68 & samplecsv$MarkerNumber<69)] <- 68
samplecsv$Trial [which(samplecsv$MarkerNumber>=69 & samplecsv$MarkerNumber<70)] <- 69
samplecsv$Trial [which(samplecsv$MarkerNumber>=70 & samplecsv$MarkerNumber<71)] <- 70
samplecsv$Trial [which(samplecsv$MarkerNumber>=71 & samplecsv$MarkerNumber<72)] <- 71
samplecsv$Trial [which(samplecsv$MarkerNumber>=72 & samplecsv$MarkerNumber<73)] <- 72
samplecsv$Trial [which(samplecsv$MarkerNumber>=73 & samplecsv$MarkerNumber<74)] <- 73
samplecsv$Trial [which(samplecsv$MarkerNumber>=74 & samplecsv$MarkerNumber<75)] <- 74
samplecsv$Trial [which(samplecsv$MarkerNumber>=75 & samplecsv$MarkerNumber<76)] <- 75
samplecsv$Trial [which(samplecsv$MarkerNumber>=76 & samplecsv$MarkerNumber<77)] <- 76
samplecsv$Trial [which(samplecsv$MarkerNumber>=77 & samplecsv$MarkerNumber<78)] <- 77
samplecsv$Trial [which(samplecsv$MarkerNumber>=78 & samplecsv$MarkerNumber<79)] <- 78
samplecsv$Trial [which(samplecsv$MarkerNumber>=79 & samplecsv$MarkerNumber<80)] <- 79
samplecsv$Trial [which(samplecsv$MarkerNumber>=80 & samplecsv$MarkerNumber<81)] <- 80
samplecsv$Trial [which(samplecsv$MarkerNumber>=81 & samplecsv$MarkerNumber<82)] <- 81
samplecsv$Trial [which(samplecsv$MarkerNumber>=82 & samplecsv$MarkerNumber<83)] <- 82
samplecsv$Trial [which(samplecsv$MarkerNumber>=83 & samplecsv$MarkerNumber<84)] <- 83
samplecsv$Trial [which(samplecsv$MarkerNumber>=84 & samplecsv$MarkerNumber<85)] <- 84
samplecsv$Trial [which(samplecsv$MarkerNumber>=85 & samplecsv$MarkerNumber<86)] <- 85
samplecsv$Trial [which(samplecsv$MarkerNumber>=86 & samplecsv$MarkerNumber<87)] <- 86
samplecsv$Trial [which(samplecsv$MarkerNumber>=87 & samplecsv$MarkerNumber<88)] <- 87
samplecsv$Trial [which(samplecsv$MarkerNumber>=88 & samplecsv$MarkerNumber<89)] <- 88
samplecsv$Trial [which(samplecsv$MarkerNumber>=89 & samplecsv$MarkerNumber<90)] <- 89
samplecsv$Trial [which(samplecsv$MarkerNumber>=90 & samplecsv$MarkerNumber<91)] <- 90
samplecsv$Trial [which(samplecsv$MarkerNumber>=91 & samplecsv$MarkerNumber<92)] <- 91
samplecsv$Trial [which(samplecsv$MarkerNumber>=92 & samplecsv$MarkerNumber<93)] <- 92
samplecsv$Trial [which(samplecsv$MarkerNumber>=93 & samplecsv$MarkerNumber<94)] <- 93
samplecsv$Trial [which(samplecsv$MarkerNumber>=94 & samplecsv$MarkerNumber<95)] <- 94
samplecsv$Trial [which(samplecsv$MarkerNumber>=95 & samplecsv$MarkerNumber<96)] <- 95
samplecsv$Trial [which(samplecsv$MarkerNumber>=96 & samplecsv$MarkerNumber<97)] <- 96
samplecsv$Trial [which(samplecsv$MarkerNumber>=97 & samplecsv$MarkerNumber<98)] <- 97
samplecsv$Trial [which(samplecsv$MarkerNumber>=98 & samplecsv$MarkerNumber<99)] <- 98
samplecsv$Trial [which(samplecsv$MarkerNumber>=99 & samplecsv$MarkerNumber<100)] <- 99
samplecsv$Trial [which(samplecsv$MarkerNumber>=100 & samplecsv$MarkerNumber<101)] <- 100
samplecsv$Trial [which(samplecsv$MarkerNumber>=101 & samplecsv$MarkerNumber<102)] <- 101
samplecsv$Trial [which(samplecsv$MarkerNumber>=102 & samplecsv$MarkerNumber<103)] <- 102
samplecsv$Trial [which(samplecsv$MarkerNumber>=103 & samplecsv$MarkerNumber<104)] <- 103
samplecsv$Trial [which(samplecsv$MarkerNumber>=104 & samplecsv$MarkerNumber<105)] <- 104
samplecsv$Trial [which(samplecsv$MarkerNumber>=105 & samplecsv$MarkerNumber<106)] <- 105
samplecsv$Trial [which(samplecsv$MarkerNumber>=106 & samplecsv$MarkerNumber<107)] <- 106
samplecsv$Trial [which(samplecsv$MarkerNumber>=107 & samplecsv$MarkerNumber<108)] <- 107
samplecsv$Trial [which(samplecsv$MarkerNumber>=108 & samplecsv$MarkerNumber<109)] <- 108
samplecsv$Trial [which(samplecsv$MarkerNumber>=109 & samplecsv$MarkerNumber<110)] <- 109
samplecsv$Trial [which(samplecsv$MarkerNumber>=110 & samplecsv$MarkerNumber<111)] <- 110
samplecsv$Trial [which(samplecsv$MarkerNumber>=111 & samplecsv$MarkerNumber<112)] <- 111
samplecsv$Trial [which(samplecsv$MarkerNumber>=112 & samplecsv$MarkerNumber<113)] <- 112
samplecsv$Trial [which(samplecsv$MarkerNumber>=113 & samplecsv$MarkerNumber<114)] <- 113
samplecsv$Trial [which(samplecsv$MarkerNumber>=114 & samplecsv$MarkerNumber<115)] <- 114
samplecsv$Trial [which(samplecsv$MarkerNumber>=115 & samplecsv$MarkerNumber<116)] <- 115
samplecsv$Trial [which(samplecsv$MarkerNumber>=116 & samplecsv$MarkerNumber<117)] <- 116
samplecsv$Trial [which(samplecsv$MarkerNumber>=117 & samplecsv$MarkerNumber<118)] <- 117
samplecsv$Trial [which(samplecsv$MarkerNumber>=118 & samplecsv$MarkerNumber<119)] <- 118
samplecsv$Trial [which(samplecsv$MarkerNumber>=119 & samplecsv$MarkerNumber<120)] <- 119
samplecsv$Trial [which(samplecsv$MarkerNumber>=120 & samplecsv$MarkerNumber<121)] <- 120
samplecsv$Trial [which(samplecsv$MarkerNumber>=121 & samplecsv$MarkerNumber<122)] <- 121
samplecsv$Trial [which(samplecsv$MarkerNumber>=122 & samplecsv$MarkerNumber<123)] <- 122
samplecsv$Trial [which(samplecsv$MarkerNumber>=123 & samplecsv$MarkerNumber<124)] <- 123
samplecsv$Trial [which(samplecsv$MarkerNumber>=124 & samplecsv$MarkerNumber<125)] <- 124
samplecsv$Trial [which(samplecsv$MarkerNumber>=125 & samplecsv$MarkerNumber<126)] <- 125
samplecsv$Trial [which(samplecsv$MarkerNumber>=126 & samplecsv$MarkerNumber<127)] <- 126
samplecsv$Trial [which(samplecsv$MarkerNumber>=127 & samplecsv$MarkerNumber<128)] <- 127
samplecsv$Trial [which(samplecsv$MarkerNumber>=128 & samplecsv$MarkerNumber<129)] <- 128
samplecsv$Trial [which(samplecsv$MarkerNumber>=129 & samplecsv$MarkerNumber<130)] <- 129
samplecsv$Trial [which(samplecsv$MarkerNumber>=130 & samplecsv$MarkerNumber<131)] <- 130
samplecsv$Trial [which(samplecsv$MarkerNumber>=131 & samplecsv$MarkerNumber<132)] <- 131
samplecsv$Trial [which(samplecsv$MarkerNumber>=132 & samplecsv$MarkerNumber<133)] <- 132
samplecsv$Trial [which(samplecsv$MarkerNumber>=133 & samplecsv$MarkerNumber<134)] <- 133
samplecsv$Trial [which(samplecsv$MarkerNumber>=134 & samplecsv$MarkerNumber<135)] <- 134
samplecsv$Trial [which(samplecsv$MarkerNumber>=135 & samplecsv$MarkerNumber<136)] <- 135
samplecsv$Trial [which(samplecsv$MarkerNumber>=136 & samplecsv$MarkerNumber<137)] <- 136
samplecsv$Trial [which(samplecsv$MarkerNumber>=137 & samplecsv$MarkerNumber<138)] <- 137
samplecsv$Trial [which(samplecsv$MarkerNumber>=138 & samplecsv$MarkerNumber<139)] <- 138
samplecsv$Trial [which(samplecsv$MarkerNumber>=139 & samplecsv$MarkerNumber<140)] <- 139
samplecsv$Trial [which(samplecsv$MarkerNumber>=140 & samplecsv$MarkerNumber<141)] <- 140
samplecsv$Trial [which(samplecsv$MarkerNumber>=141 & samplecsv$MarkerNumber<142)] <- 141
samplecsv$Trial [which(samplecsv$MarkerNumber>=142 & samplecsv$MarkerNumber<143)] <- 142
samplecsv$Trial [which(samplecsv$MarkerNumber>=143 & samplecsv$MarkerNumber<144)] <- 143
samplecsv$Trial [which(samplecsv$MarkerNumber>=144 & samplecsv$MarkerNumber<145)] <- 144
samplecsv$Trial [which(samplecsv$MarkerNumber>=145 & samplecsv$MarkerNumber<146)] <- 145
samplecsv$Trial [which(samplecsv$MarkerNumber>=146 & samplecsv$MarkerNumber<147)] <- 146
samplecsv$Trial [which(samplecsv$MarkerNumber>=147 & samplecsv$MarkerNumber<148)] <- 147
samplecsv$Trial [which(samplecsv$MarkerNumber>=148 & samplecsv$MarkerNumber<149)] <- 148
samplecsv$Trial [which(samplecsv$MarkerNumber>=149 & samplecsv$MarkerNumber<150)] <- 149
samplecsv$Trial [which(samplecsv$MarkerNumber>=150 & samplecsv$MarkerNumber<151)] <- 150
samplecsv$Trial [which(samplecsv$MarkerNumber>=151 & samplecsv$MarkerNumber<152)] <- 151
samplecsv$Trial [which(samplecsv$MarkerNumber>=152 & samplecsv$MarkerNumber<153)] <- 152
samplecsv$Trial [which(samplecsv$MarkerNumber>=153 & samplecsv$MarkerNumber<154)] <- 153
samplecsv$Trial [which(samplecsv$MarkerNumber>=154 & samplecsv$MarkerNumber<155)] <- 154
samplecsv$Trial [which(samplecsv$MarkerNumber>=155 & samplecsv$MarkerNumber<156)] <- 155
samplecsv$Trial [which(samplecsv$MarkerNumber>=156 & samplecsv$MarkerNumber<157)] <- 156
samplecsv$Trial [which(samplecsv$MarkerNumber>=157 & samplecsv$MarkerNumber<158)] <- 157
samplecsv$Trial [which(samplecsv$MarkerNumber>=158 & samplecsv$MarkerNumber<159)] <- 158
samplecsv$Trial [which(samplecsv$MarkerNumber>=159 & samplecsv$MarkerNumber<160)] <- 159
samplecsv$Trial [which(samplecsv$MarkerNumber>=160 & samplecsv$MarkerNumber<161)] <- 160
samplecsv$Trial [which(samplecsv$MarkerNumber>=161 & samplecsv$MarkerNumber<162)] <- 161
samplecsv$Trial [which(samplecsv$MarkerNumber>=162 & samplecsv$MarkerNumber<163)] <- 162
samplecsv$Trial [which(samplecsv$MarkerNumber>=163 & samplecsv$MarkerNumber<164)] <- 163
samplecsv$Trial [which(samplecsv$MarkerNumber>=164 & samplecsv$MarkerNumber<165)] <- 164
samplecsv$Trial [which(samplecsv$MarkerNumber>=165 & samplecsv$MarkerNumber<166)] <- 165
samplecsv$Trial [which(samplecsv$MarkerNumber>=166 & samplecsv$MarkerNumber<167)] <- 166
samplecsv$Trial [which(samplecsv$MarkerNumber>=167 & samplecsv$MarkerNumber<168)] <- 167
samplecsv$Trial [which(samplecsv$MarkerNumber>=168 & samplecsv$MarkerNumber<169)] <- 168
samplecsv$Trial [which(samplecsv$MarkerNumber>=169 & samplecsv$MarkerNumber<170)] <- 169
samplecsv$Trial [which(samplecsv$MarkerNumber>=170 & samplecsv$MarkerNumber<171)] <- 170
samplecsv$Trial [which(samplecsv$MarkerNumber>=171 & samplecsv$MarkerNumber<172)] <- 171
samplecsv$Trial [which(samplecsv$MarkerNumber>=172 & samplecsv$MarkerNumber<173)] <- 172
samplecsv$Trial [which(samplecsv$MarkerNumber>=173 & samplecsv$MarkerNumber<174)] <- 173
samplecsv$Trial [which(samplecsv$MarkerNumber>=174 & samplecsv$MarkerNumber<175)] <- 174
samplecsv$Trial [which(samplecsv$MarkerNumber>=175 & samplecsv$MarkerNumber<176)] <- 175
samplecsv$Trial [which(samplecsv$MarkerNumber>=176 & samplecsv$MarkerNumber<177)] <- 176
samplecsv$Trial [which(samplecsv$MarkerNumber>=177 & samplecsv$MarkerNumber<178)] <- 177
samplecsv$Trial [which(samplecsv$MarkerNumber>=178 & samplecsv$MarkerNumber<179)] <- 178
samplecsv$Trial [which(samplecsv$MarkerNumber>=179 & samplecsv$MarkerNumber<180)] <- 179
samplecsv$Trial [which(samplecsv$MarkerNumber>=180 & samplecsv$MarkerNumber<181)] <- 180
samplecsv$Trial [which(samplecsv$MarkerNumber>=181 & samplecsv$MarkerNumber<182)] <- 181
samplecsv$Trial [which(samplecsv$MarkerNumber>=182 & samplecsv$MarkerNumber<183)] <- 182
samplecsv$Trial [which(samplecsv$MarkerNumber>=183 & samplecsv$MarkerNumber<184)] <- 183
samplecsv$Trial [which(samplecsv$MarkerNumber>=184 & samplecsv$MarkerNumber<185)] <- 184
samplecsv$Trial [which(samplecsv$MarkerNumber>=185 & samplecsv$MarkerNumber<186)] <- 185
samplecsv$Trial [which(samplecsv$MarkerNumber>=186 & samplecsv$MarkerNumber<187)] <- 186
samplecsv$Trial [which(samplecsv$MarkerNumber>=187 & samplecsv$MarkerNumber<188)] <- 187
samplecsv$Trial [which(samplecsv$MarkerNumber>=188 & samplecsv$MarkerNumber<189)] <- 188
samplecsv$Trial [which(samplecsv$MarkerNumber>=189 & samplecsv$MarkerNumber<190)] <- 189
samplecsv$Trial [which(samplecsv$MarkerNumber>=190 & samplecsv$MarkerNumber<191)] <- 190
samplecsv$Trial [which(samplecsv$MarkerNumber>=191 & samplecsv$MarkerNumber<192)] <- 191
samplecsv$Trial [which(samplecsv$MarkerNumber>=192 & samplecsv$MarkerNumber<193)] <- 192
samplecsv$Trial [which(samplecsv$MarkerNumber>=193 & samplecsv$MarkerNumber<194)] <- 193
samplecsv$Trial [which(samplecsv$MarkerNumber>=194 & samplecsv$MarkerNumber<195)] <- 194
samplecsv$Trial [which(samplecsv$MarkerNumber>=195 & samplecsv$MarkerNumber<196)] <- 195
samplecsv$Trial [which(samplecsv$MarkerNumber>=196 & samplecsv$MarkerNumber<197)] <- 196
samplecsv$Trial [which(samplecsv$MarkerNumber>=197 & samplecsv$MarkerNumber<198)] <- 197
samplecsv$Trial [which(samplecsv$MarkerNumber>=198 & samplecsv$MarkerNumber<199)] <- 198
samplecsv$Trial [which(samplecsv$MarkerNumber>=199 & samplecsv$MarkerNumber<200)] <- 199
samplecsv$Trial [which(samplecsv$MarkerNumber>=200 & samplecsv$MarkerNumber<200.999)] <- 200
samplecsv$Trial [which(samplecsv$MarkerNumber>=201 & samplecsv$MarkerNumber<201.999)] <- 201
write.table(samplecsv,file="/Volumes/NO NAME/Csvgroup3-28/CSEyesamplecsv.csv",sep=",",row.names=F)

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
