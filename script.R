# Any package that is required by the script below is given here:
# Check to see if packages are installed, if not install.
inst_pkgs = load_pkgs =  c("ff","ffbase","biglm")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

# Set Working Directory to where big data is
setwd("C:/Rprojects/Rain")

# Check temporary directory ff will write to (avoid placing on a drive with SSD)
getOption("fftempdir")

# Set new temporary directory
options(fftempdir = "C:/Rprojects/Rain/temp")

# Load in the big data
ffx = read.table.ffdf(file="train/train.csv", # File Name
                      sep=",",         # Tab separator is used
                      header=TRUE,     # No variable names are included in the file
                      fill = TRUE)    # Missing values are represented by NA

fftest = read.table.ffdf(file="test/test.csv", # File Name
                      sep=",",         # Tab separator is used
                      header=TRUE,     # No variable names are included in the file
                      fill = TRUE)    # Missing values are represented by NA



# Export created R Object by saving files 
ffsave(ffx, # ffdf object
       file="C:/Rprojects/Rain/ffdata/ffdata.ff", # Permanent Storage location
       # Last name in the path is the name for the file you want 
       # e.g. ffdac.Rdata and ffdac.ff etc.
       rootpath="C:/Rprojects/Rain/temp")     # Temporary write directory
# where data was initially loaded via the options(fftempdir) statement

# Export created R Object by saving files 
ffsave(fftest, # ffdf object
       file="C:/Rprojects/Rain/ffdata/fftest.ff", # Permanent Storage location
       # Last name in the path is the name for the file you want 
       # e.g. ffdac.Rdata and ffdac.ff etc.
       rootpath="C:/Rprojects/Rain/temp")     # Temporary write directory
# where data was initially loaded via the options(fftempdir) statement



# Load Data R object on subsequent runs (saves ~ 20 mins)
ffload(file="C:/Rprojects/Rain/ffdata/ffdata.ff", # Load data from archive
       overwrite = TRUE) # Overwrite any existing files with new data

ffload(file="C:/Rprojects/Rain/ffdata/fftest.ff", # Load data from archive
       overwrite = TRUE) # Overwrite any existing files with new data

##Normal operations
#str(ffx[,])

##removing NAs per column
load("medianColumn.R")
medianColumn(ffx$Ref)
medianColumn(ffx$Ref_5x5_10th)
medianColumn(ffx$Ref_5x5_50th)
medianColumn(ffx$Ref_5x5_90th)
medianColumn(ffx$RefComposite)
medianColumn(ffx$RefComposite_5x5_10th)
medianColumn(ffx$RefComposite_5x5_50th)
medianColumn(ffx$RefComposite_5x5_90th)
medianColumn(ffx$RhoHV)
medianColumn(ffx$RhoHV_5x5_10th)
medianColumn(ffx$RhoHV_5x5_50th)
medianColumn(ffx$RhoHV_5x5_90th)
medianColumn(ffx$Zdr)
medianColumn(ffx$Zdr_5x5_10th)
medianColumn(ffx$Zdr_5x5_50th)
medianColumn(ffx$Zdr_5x5_90th)
medianColumn(ffx$Kdp)
medianColumn(ffx$Kdp_5x5_10th)
medianColumn(ffx$Kdp_5x5_50th)
medianColumn(ffx$Kdp_5x5_90th)


##Expected
idx<-ffx$Expected >10
idx <- ffwhich(idx, idx == TRUE)
ffx$Expected <- ffindexset(x=ffx$Expected, index=idx, value=ff(0, length=length(idx), vmode = "double"))  
ffdata<-ffx[,3:ncol(ffx)]


medianColumn(fftest$Ref)
medianColumn(fftest$Ref_5x5_10th)
medianColumn(fftest$Ref_5x5_50th)
medianColumn(fftest$Ref_5x5_90th)
medianColumn(fftest$RefComposite)
medianColumn(fftest$RefComposite_5x5_10th)
medianColumn(fftest$RefComposite_5x5_50th)
medianColumn(fftest$RefComposite_5x5_90th)
medianColumn(fftest$RhoHV)
medianColumn(fftest$RhoHV_5x5_10th)
medianColumn(fftest$RhoHV_5x5_50th)
medianColumn(fftest$RhoHV_5x5_90th)
medianColumn(fftest$Zdr)
medianColumn(fftest$Zdr_5x5_10th)
medianColumn(fftest$Zdr_5x5_50th)
medianColumn(fftest$Zdr_5x5_90th)
medianColumn(fftest$Kdp)
medianColumn(fftest$Kdp_5x5_10th)
medianColumn(fftest$Kdp_5x5_50th)
medianColumn(fftest$Kdp_5x5_90th)

fftest<-fftest[,1:ncol(fftest)]
fftest$Expected<-0



# Model
# Get predictor variable names, removing Expected column 
data_variables = colnames(ffdata)[c(-22)]

# Create model formula statement
model_formula = as.formula(paste0("Expected ~", paste0(data_variables, collapse="+")))

## YOU MUST CLEAN THE DATA BEFORE RUNNING THE REGRESSION! RUNNING THE REGRESSION WITH MISSING VALUES WILL YIELD AN ETA ERROR!

# Use a modified version of bigglm so that bigglm will not try to convert to a regular data.frame
model_out = bigglm.ffdf(model_formula, family=gaussian(), data=ffdata,chunksize=100, na.action=na.exclude)
summary(model_out)$rsq


p<-predict(model_out, newdata=fftest, type="response")



