current_version <- "1.0.0" # Simpler navigation
wd = getwd()

#Set our working directory.
setwd(paste0(wd,"/docs"))

#render site.
rmarkdown::render_site(encoding = 'utf-8')

setwd(wd)
rm(current_version)

