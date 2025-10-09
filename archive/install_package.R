# Install dataRetrieval package from local tar.gz file
install.packages("D:/R projects/week 8/dataRetrieval_2.7.21.tar.gz", 
                 repos = NULL, 
                 type = "source")

# Load the package to verify installation
library(dataRetrieval)

# Check version
packageVersion("dataRetrieval")

# Check for new functions
cat("Installation complete!\n")