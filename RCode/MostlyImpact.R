######################################################################
######################### IMPACT & LOSS DATA #########################
######################################################################

# Extract historical hazard events:
dfGDACS<-FilterGDACS(iso3c = ISO)

# historical storm surge hazard sizes:
# article:   https://www.nature.com/articles/s41597-021-00906-x#Sec8
# data can be downloaded here :    http://gssr.info/

# Predicted multi-hazard economic impact on road & railway transport infrastructure:
# https://www.nature.com/articles/s41467-019-10442-3