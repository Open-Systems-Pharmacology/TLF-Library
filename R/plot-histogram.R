set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                  rating = c(rnorm(200),rnorm(200, mean=.8)))
# View first few rows
head(dat)

binWidth = 2;
bins = 20;


#the histogram plot is actially two plots.
#the first is the histrogram itself,
#the second is the set of vertical lines that are functions of the data represented in the histogram.
#so can use two data mappings.

histGrouping <- Groupings$new(fill = GroupMapping$new(group = "cond", label = "Condition") )
#verticalLineGrouping <- Groupings$new(fill = "cond")

hdm <- HistogramDataMapping$new( x = "rating" , groupings = histGrouping )

hpc <- HistogramPlotConfiguration$new(data = dat , dataMapping = hdm , bins = bins , binWidth = NULL , verticalLineFunctions = c(mean,median) , verticalLineFunctionNames = c("mean","median") )

plotObject <- ggplot2::ggplot()

plotObject <- hpc$addHistograms(plotObject, data = dat, metaData=NULL, dataMapping = hdm , bins = NULL, binWidth = NULL )

#plotObject <- hpc$addHistograms(plotObject, data = dat, metaData=NULL, dataMapping = hdm)

plotObject <- hpc$setPlotLabels(plotObject)

plotObject <- hpc$legend$setPlotLegend(plotObject)

#plotObject <- hpc$addVerticalLines(plotObject)

sr <- aggregate(hpc$mapData$x,list(hpc$mapData$Condition),function(x) mean(x))

colnames(sr)<-c("Grp","mean")

plotObject <- plotObject + geom_vline(data=sr, aes(xintercept=mean, colour=Grp))



#plotObject <- plotObject +  geom_vline( mapping =  aes(xintercept = c(0,2,3)) )

#plotObject <- plotObject +  geom_vline(#data = hpc$mapData,
#                                      mapping = aes(xintercept = c(-1,-2,-3))
#                                       )


# plotObject <- plotObject + ggplot2::geom_histogram(
#   data = dat,
#   mapping = aes( x = rating , fill = cond ),
#   show.legend = TRUE
# )

show(plotObject)

#p <- p + geom_vline(data=sr, aes(xintercept=mean, colour=Grp))



# plth <- plotHistogram(data  = data,
#                       dataMapping = hdm,
#                       plotConfiguration = hpc)


