#' @title setFontProperties
#' @param plotHandle ggplot object
#' @param TitleFont Font Class for title
#' @param SubtitleFont Font Class for subtitle
#' @param XAxisFont Font Class for xaxis and ticks
#' @param YAxisFont Font Class for yaxis and ticks
#' @param LegendFont Font Class for legend
#' @return plotHandle ggplot object with updated fonts
#' @description 
#' setFontProperties set Font Properties on a ggplot object
#' @export
#' @examples
#' 
#' p <- ggplot()+labs(title='Title', )
#' NewFont <- FontClass$new(color="blue", size=20)
#' p <- setFontProperties(plotHandle=p, TitleFont=NewFont)
setFontProperties <- function(plotHandle, 
                              TitleFont=NULL, 
                              SubtitleFont=NULL, 
                              XAxisFont=NULL, 
                              YAxisFont=NULL, 
                              LegendFont=NULL){

      if(!is.null(TitleFont)) { plotHandle <- plotHandle + theme(plot.title = TitleFont$setFont())} 
      if(!is.null(SubtitleFont)) { plotHandle <- plotHandle + theme(plot.subtitle = SubtitleFont$setFont())}
      if(!is.null(XAxisFont)){ plotHandle <- plotHandle+ theme(axis.title.x = XAxisFont$setFont(), axis.text.x=XAxisFont$setFont())} 
      if(!is.null(YAxisFont)){ plotHandle <- plotHandle + theme(axis.title.y = YAxisFont$setFont(), axis.text.y=YAxisFont$setFont())}
      if(!is.null(LegendFont)){plotHandle <- plotHandle + theme(legend.text = LegendFont$setFont())} 
  
  return(plotHandle)  
    
}
