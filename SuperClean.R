##Heatmapping

```{r SEAT_Example, fig.width=16, fig.height=6}
library(ggplot2)
library(png)
library(gridExtra)


AF11o11 <- readPNG("Kfear11_out11.png")
CN1i1 <- readPNG("neu1_in1.png")


SEAT1 <- ggplot(data_all_nobase, aes(x = X_Gaze, y = Y_Gaze)) + annotation_raster(AF11o11, 265.8304, 758.1696, -123.1104, -644.88, interpolate = TRUE) + xlim(0, 1024) +ylim(768, 0) + theme(legend.position="none") + labs(title = "Asian Fear Outdoor") + xlab("") + ylab("") + theme(plot.background = element_rect(fill='black', colour='black')) + theme(panel.grid=element_line(colour="black"))

SEAT2 <- ggplot(data_all_nobase, aes(x = X_Gaze, y = Y_Gaze)) + annotation_raster(CN1i1, 265.8304, 758.1696, -123.1104, -644.88, interpolate = TRUE) + xlim(0, 1024) + ylim(768, 0) + theme(legend.position="none") + labs(title = "Caucasion Neutral Indoor" ) + xlab("") + ylab("") + theme(plot.background = element_rect(fill='black', colour='black')) + theme(panel.grid=element_line(colour="black"))



```
