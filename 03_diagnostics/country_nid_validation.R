library(tidyverse)
rm(list = ls())
mydat3 <- read.csv('J:/WORK/11_geospatial/10_mbg/input_data/w_piped.csv',
                   stringsAsFactors = F)
pdf('C:/Users/adesh/Desktop/country_nid_plots.pdf')
for (j in unique(mydat3$country)) {
  message(j)
  test <- filter(mydat3, country == j)

  indices <- seq(1, length(unique(test$nid)), by = 5)
  nids <- unique(test$nid)

  for (i in 0:round(((length(nids))/5))) {
    plotdat <- filter(test, nid %in% nids[(5*(i)):min((5*(i)+4),length(nids))])
    plotdat$nid <- as.character(plotdat$nid)
    print(
          ggplot(plotdat) +
            geom_point(aes(x = N, y = prop, col = nid)) + 
            ylim(0,1) + xlim(0,NA) +
            ggtitle(paste0(j,'_',i))
    
    )
  }
}
dev.off()

