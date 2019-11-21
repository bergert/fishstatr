library(faoebx5)
library(fishstatr)
library(dplyr)
library(data.table)

# required:
# load fishstat metadata from EBX5
metadata <- ReadMetadata()

# read dataset dimensions
dimensions <- GetDimensions(metadata,1)

# weenumerate all dimensions
#for (conceptDim in dimensions$ConceptID) {
#  GetDimensionGroups(metadata,conceptDim)
#}

# as a test, only read country
for (aDimension in GetDimensionGroups(metadata,1)) {
  print(aDimension)
}


X1=rep((100:102),4)
X2=(1:12)
df<-data.frame(parent=X1,child=X2)
df1 <- df %>% group_by(new=parent)




convert2List = function(parent) {
   return(tibble::tibble(children = list(parent)))
}

df2 <- df1 %>% group_map(~convert2List(.x$child))
df2 <- data.frame(df2)


