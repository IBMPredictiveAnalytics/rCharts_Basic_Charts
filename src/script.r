# Install packages   -Need to get rCharts from GitHub - no binary for 3.1
list.of.packages <- c( "devtools", 'RCurl', 'base64enc', 'rjson')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(devtools)
if (! ("rCharts" %in% rownames(installed.packages()))) {install_github("ramnathv/rCharts@dev") }
library("rCharts")
options(
  rcharts.mode = 'iframesrc', 
  rcharts.cdn = %%cdnflag%%,
  RCHART_WIDTH =  %%width%%,
  RCHART_HEIGHT = %%height%%
)
use_custom_directory <- %%use_custom_directory%%
  
  if(use_custom_directory) {
    filepath <- "%%custom_directory%%"
    if(nchar(filepath)==0) {
      stop("Please provide a valid file path to save the output")
    }
  } else {
     #filepath <- paste( tempfile(), ".htm", sep='')

makeName <- function(n=2, len=10){
  empty.str <- c(1:n)                  
  for (i in 1:n)
  {
    empty.str[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    len, replace=TRUE),
                             collapse="")
  }
  tmp <- Sys.getenv('TEMP')
  dir.create(paste0(tmp,"\\",empty.str[1]))
  full.path <- paste0(tmp,"\\",empty.str[1],"\\",empty.str[2],".htm")
  return(full.path)
}

filepath <- makeName()
  }


#setting the working dir to the same path as where the htm map will be saved
#allows the legend to be displayed correctly
working_dir <- dirname(filepath)
setwd(working_dir)
setwd("~/")
#print(names(modelerData))
df <-data.frame(modelerData)
xx<-df['%%Xaxis%%']
yy<-df['%%Yaxis%%']



#Encoding(xx)
#xxx<-iconv(xx, "latin1", "UTF-8")

%%Xaxis%%<-xx
%%Yaxis%%<-yy

chart.type <- "%%chartoption%%"

if('%%category_field%%' != ''){      
  cat.fld  <-df['%%category_field%%']
  %%category_field%% <- cat.fld
  df2 <- data.frame(%%Xaxis%%, %%Yaxis%%,%%category_field%%)
  
  if (chart.type == 'bar'){
    d1 <- dPlot(x ='%%Xaxis%%', y = '%%Yaxis%%',groups='%%category_field%%',data = df2, type = chart.type)
    
  }
    if(chart.type == 'point')  {
      d1 <- rPlot(%%Yaxis%%  ~%%Xaxis%%,
                  color ='%%category_field%%',
                  data = df2, 
                  type =chart.type)
      d1$chart(color = %%Xaxis%%)
    }
        if(chart.type == 'line')  {
      d1 <- rPlot('%%Xaxis%%','%%Yaxis%%',
                  color ='%%category_field%%',
                  data = df2, 
                  type =chart.type)
      d1$chart(color = %%Xaxis%%)
    }
    }else{
      df2 <- data.frame(%%Xaxis%%, %%Yaxis%%)
      if(chart.type != 'point'){
           d1 <- dPlot(x ='%%Xaxis%%', y = '%%Yaxis%%',data = df2, type = chart.type)
       }  else{
            d1 <- dPlot( %%Yaxis%% ~ %%Xaxis%%,
            data = df2,
            type = "bubble"
            )
}
    }

 d1$guides (
  x = list(
    min = pretty(df2$%%Xaxis%%)[1],
    max = tail(pretty(df2$%%Xaxis%%),1))
,  
  y = list(
  min = pretty(df2$%%Yaxis%%)[1],
  max = tail(pretty(df2$%%Yaxis%%),1))
)
    d1$legend(
      x = 60,
      y = 10,
      width = 700,
      height = 20,
      horizontalAlign = "left"
    )

d1$save(filepath, cdn = %%cdnflag%%)
print("saved!")  
browseURL(filepath)
    




