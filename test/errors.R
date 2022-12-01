# errors.R


library(shapefiles)

shapefiles::read.shapefile("./clown_plot_outline_polygon/k1_crown_plot_outlinepolygon2")

# > shapefiles::read.shapefile("./clown_plot_outline_polygon/k1_crown_plot_outlinepolygon2")
# Error in make.names(onames, unique = TRUE) :
#   invalid multibyte string at '<8e><f7><8e><ed>'
