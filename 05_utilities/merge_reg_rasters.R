rm(list = ls())

library(raster)

setwd('/share/geospatial/mbg/wash/w_piped/output/2017_09_18_00_49_14')
piped <- lapply(list.files()[grep('.gri', list.files())], brick)
piped <- do.call(raster::merge, piped)

pdf('/home/adesh/Desktop/piped_2017.09.18.pdf')
print(
	print(spplot(piped[[c(1,6,11,16)]]))
	)
dev.off()


setwd('/share/geospatial/mbg/wash/w_imp/output/2017_09_18_01_00_23/')
imp <- lapply(list.files()[grep('.gri', list.files())], brick)
imp <- do.call(raster::merge, imp)

setwd('/share/geospatial/mbg/wash/w_unimp/output/2017_09_18_01_02_59')
unimp <- lapply(list.files()[grep('.gri', list.files())], brick)
unimp <- do.call(raster::merge, unimp)

setwd('/share/geospatial/mbg/wash/w_surface/output/2017_09_18_01_03_42')
surface <- lapply(list.files()[grep('.gri', list.files())], brick)
surface <- do.call(raster::merge, surface)