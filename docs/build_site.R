current_version <- "1.1.0" # Simpler navigation
wd = getwd()

#Set our working directory.
setwd(paste0(wd,"/docs"))

#render site.
rmarkdown::render_site(encoding = 'utf-8')

setwd(wd)
rm(current_version)

# News
# 1.0.1 # fix map link
# 1.00 Google form announcement up
# # 0.3 # Emily's Edits
# 0.2.1 - Little Cleaning, index has subpage liks in text flow
# 0.2.0.9002 - Clean up and add sub-page links
# 0.2.0.9001 - Added some navigation links to index page
# 0.2.0.9000 - Changing focus to a Primarty goal of developing annotation workflow
# 0.1.0 First Rough Draft
# 0.0.0 Framework

