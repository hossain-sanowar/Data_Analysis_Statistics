#----------------------------------------------------------
# Task 5
#----------------------------------------------------------
# When reading data from text files, it is the responsibility of the user 
# to know and to specify the conventions used to create that file, e.g. 
# the comment character, whether a header line is present, the value separator, 
# the representation for missing values (and so on) described in Export to text files. 
# A markup language which can be used to describe not only content but also 
# the structure of the content can make a file self-describing, so that one 
# need not provide these details to the software reading the data.

# The eXtensible Markup Language - more commonly known simply as XML - can be 
# used to provide such structure, not only for standard datasets but also more 
# complex data structures. XML is becoming extremely popular and is emerging 
# as a standard for general data markup and exchange. It is being used by different 
# communities to describe geographical data such as maps, graphical displays, 
# mathematics and so on.

# XML provides a way to specify the file's encoding, e.g.<?xml version="1.0" encoding="UTF-8"?>
# although it does not require it.
# The XML package provides general facilities for reading and writing XML documents within R. 
# Package StatDataML on CRAN is one example building on XML.
# NB: XML is available as a binary package for Windows, normally from the 'CRAN extras' 
# repository (which is selected by default on Windows).
# (R Data Import/Export, CRAN)

# load the XML packages
#install.packages("XML", dependencies = TRUE)
#install.packages("plyr", dependencies = TRUE)

require("XML")
require("plyr")

# Next we need to parse the XML file, so we're sure that R can access the data within the file. 
# This is basically reading the file into R. Then, just to confirm that R knows our file is in XML, 
# we check the class. 
#xmlfile <- xmlParse("C:\\Neuer Ordner\\stat\\Master\\Data\\UNdata_Export_20160505_221159797\\UNdata_Export_20160505_221159797withoutFootnotes.xml")
#xmlfileComplete <- xmlParse("C:\\Users\\RD\\sciebo\\stat\\Master\\Data\\UNdata_Export_20160505_221159797\\UNdata_Export_20160505_221159797.xml")
xmlfileComplete <- xmlParse(file.choose())

#class(xmlfile) #"XMLInternalDocument" "XMLAbstractDocument"
class(xmlfileComplete) #"XMLInternalDocument" "XMLAbstractDocument"

# Now we can begin to explore our XML. We start by looking at the contents of the first node or root, 
# "ROOT". We can check how many child nodes that root node has and check upon their names. 
# This process corresponds to checking how many entries are in the XML file. 
#xmltop = xmlRoot(xmlfile) #gives content of root
#class(xmltop)         # ==>"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
#xmlName(xmltop)       # name of the root node ==> ROOT
#xmlSize(xmltop)       # how many children does the root node have ==> 1
#xmlName(xmltop[[1]])  # names of root's children ==> data

xmltopComplete = xmlRoot(xmlfileComplete) #gives content of root
class(xmltopComplete)         # ==>"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
xmlName(xmltopComplete)       # name of the root node ==> ROOT
xmlSize(xmltopComplete)       # how many children does the root node have ==> 2
xmlName(xmltopComplete[[1]])  # names of root first child ==> data
xmlName(xmltopComplete[[2]])  # names of root first child ==> footnotes
# ... You can continue inspecting the file' content this way

xmlSize(xmltopComplete[[1]])              #number of nodes in data: 100000
xmlSize(xmltopComplete[[2]])              #number of nodes in footnotes: 26

# Our exploration continues by looking at subnodes of the data and footnotes 
# As with the root node, we can list the name and size of the subnodes as well as their attributes. 
# In this case, the subnodes are MedlineCitation and PubmedData.
#Root Node's children
unique(xmlSApply(xmltopComplete[[1]], xmlName))   # all childs of data are called record
unique(xmlSApply(xmltopComplete[[1]], xmlSize))   # number of subnodes in record elements: 10
xmlSApply(xmltopComplete[[1]][[1]], xmlName)      # name(s) of subnodes in record elements: field...
unique(xmlSApply(xmltopComplete[[1]][[1]], xmlSize))   # number of subnodes in field elements: 1
xmlSApply(xmltopComplete[[1]][[1]], xmlAttrs)     # attribute(s) of field elements: field.name
unique(xmlSApply(xmltopComplete[[1]][[1]][[1]], xmlSize))   # number of subnodes in field elements: 0
# ==> field elements contain the content

unique(xmlSApply(xmltopComplete[[2]], xmlName))   # all childs of data are called footnote
unique(xmlSApply(xmltopComplete[[2]], xmlSize))   # number of subnodes in footnote elements: 1
unique(xmlSApply(xmltopComplete[[2]][[2]], xmlSize))  # number of subnodes in childs of footnote: 0
# ==> footnote hold the content
xmlSApply(xmltopComplete[[2]], xmlAttrs)          # attribute(s) of footnote elements: id
xmlSize(xmltopComplete[[2]][[1]])                 #number of nodes in each footnote: 1

# We can also separate each of the entries by these subnodes. 
# The separation of entries is just indexing into the tree structure of the XML. 
# We can continue to do this until we exhaust a path-or, in XML terminology, reach 
# the end of the branch. We can do this via the numbers of the child nodes:
xmltopComplete[[1]][[1]][[1]]       # first complete field element of the first record element: name of the country of the first entry
xmltopComplete[[1]][[1]][[1]][[1]]  # content of the first field of the first record
xmltopComplete[[1]][[1]][[2]]       # second field of the first record: year of the of the first entry

xmltopComplete[[2]][[1]]            # first footnote element with id = 1
xmltopComplete[[2]][[1]][[1]]       # content of the first footnote 

# Further inspection of the file has shown that the data we want to work with are inside the data element 
# of the XML file. So we are going to work with the data node as our root node
xmltop <- xmltopComplete[[1]]
xmlName(xmltop)       # name of the root node ==> data
xmlSize(xmltop)       # how many children does the root node have ==> ...
xmlName(xmltop[[1]])  # names of root's children ==> record --> one entry or observation

# In order to see the first two entries, we can do the following.
xmltop[[1]]
xmltop[[2]]

# Finally, we can transform the XML into a more familiar structure-a dataframe. 
XML2DF <- xmlToDataFrame(xmltop, stringsAsFactors = FALSE )
#                         colClasses = list("character", 
#                                          "integer",
#                                         "character",
#                                        "character",
#                                       "character",
#                                      "character",
#                                     "character",
#                                    "integer",
#                                   "numeric") )
# Provide column names based on the names of the fields that were holding the content 
names(XML2DF) <- xmlSApply(xmltop[[1]], xmlAttrs)
str(XML2DF)
# All content has been imported as text ==> Convert content types into more suitable ones
# Some content is not usefull and should be removed
XML2DF$Year <- as.integer(XML2DF$Year)
XML2DF$`Record Type`<- NULL
XML2DF$Reliability <- NULL
XML2DF$Value <- as.numeric(XML2DF$Value)
XML2DF$`Source Year` <- as.integer(XML2DF$`Source Year`)
XML2DF$`Value Footnotes` <-NULL
head(XML2DF)

# Get the subsets of observations that we need to fullfill the task
# 1 - Select the records for Armenia and Austria  
XML2DF <- XML2DF[ which(XML2DF$'Country or Area' == "Armenia" | XML2DF$'Country or Area' == "Austria"), ]
str(XML2DF)      

# 2 - The data frame contains records for different ages and age groups --> get rid of those that are useless
#     ==> create an index vector for age column
XML2DF$Age # this shows the current content of the age column
charVec <- as.character(0:84)  # there are only values ages up to 84
(charVec <- c(charVec,"85 +"))  # 85+ is only available as age group  
indexVec <- is.element(XML2DF$Age, charVec )
indexVec
# 3 - Use the index vector in order to remove unwanted records
XML2DF <- XML2DF[indexVec,]
str(XML2DF)

# Which data is available with respect to year the records refer to?
# XML2DF[ XML2DF$'Country or Area' == "Armenia", ]["Year"]
# XML2DF[ XML2DF$'Country or Area' == "Armenia", ]["Year"]
unique(XML2DF[ XML2DF$'Country or Area' == "Armenia", ][,2])  # ==> most current data: 2011
unique(XML2DF[ XML2DF$'Country or Area' == "Austria", ][,2])  # ==> most current data: 2014
# ==> we are going to work with records from 2011

# get the records for 2011 for the males for the total area of Armenia 
XML2DF_Armenia_Male_2011 <- XML2DF[ which(XML2DF$'Country or Area' ==  "Armenia" & 
                                            XML2DF$Year ==  2011      &
                                            XML2DF$Area == "Total"    &
                                            XML2DF$Age !=  "Total"   &
                                            XML2DF$Sex ==  "Male" ) , ]
str(XML2DF_Armenia_Male_2011) 
XML2DF_Armenia_Male_2011

# get the records for 2011 for the males for the total area of Austria
XML2DF_Austria_Male_2011 <- XML2DF[ which(XML2DF$'Country or Area' ==  "Austria" & 
                                            XML2DF$Year ==  2011      &
                                            XML2DF$Area == "Total"    &
                                            XML2DF$Age !=  "Total"   &
                                            XML2DF$Sex ==  "Male" ) , ]
str(XML2DF_Austria_Male_2011) 
XML2DF_Austria_Male_2011

# get the records for 2011 for the females for the total area of Armenia
XML2DF_Armenia_Female_2011 <- XML2DF[ which(XML2DF$'Country or Area' ==  "Armenia" & 
                                              XML2DF$Year ==  2011      &
                                              XML2DF$Area == "Total"    &
                                              XML2DF$Age !=  "Total"   &
                                              XML2DF$Sex ==  "Female" ) , ]
str(XML2DF_Armenia_Female_2011) 

# get the records for 2011 for the females for the total area of Austria
XML2DF_Austria_Female_2011 <- XML2DF[ which(XML2DF$'Country or Area' ==  "Austria" & 
                                              XML2DF$Year ==  2011      &
                                              XML2DF$Area == "Total"    &
                                              XML2DF$Age !=  "Total"   &
                                              XML2DF$Sex ==  "Female" ) , ]
str(XML2DF_Austria_Female_2011) 
XML2DF_Austria_Female_2011


# create the barplots
# get the current graphics settings first, inorder to be able to set the configuration back to this
default_par <- par()
# devide the graphic window into two rows and 2 columns
par(mfrow = c(2, 2), cex=1/2)

vec1 <- as.vector(XML2DF_Armenia_Male_2011$Value)
str(vec1)
barplot(height = vec1, names.arg = XML2DF_Armenia_Male_2011$Age,main = "Armenia, males",
        las = 1, col = 'gray90' )


vec11 <- as.vector(XML2DF_Austria_Male_2011$Value)
str(vec11)
barplot(height = vec11, names.arg = XML2DF_Austria_Male_2011$Age,main = "Austria, males",
        las = 1, col = 'gray90' )

vec2 <- as.vector(XML2DF_Armenia_Female_2011$Value)
str(vec2)
barplot(height = vec2, names.arg = XML2DF_Armenia_Female_2011$Age,main = "Armenia, females",
        las = 1, col = 'gray90' )

vec22 <- as.vector(XML2DF_Austria_Female_2011$Value)
str(vec22)
barplot(height = vec22, names.arg = XML2DF_Austria_Female_2011$Age,main = "Austria, females",
        las = 1, col = 'gray90' )

# setting the graphics settings back
par(default_par)
#title(main = "Population Size vs. Age")

# Pose questions related to your observations und speculate about possible explanations.
# ==> Why is there a dip in both male and female populations at the ages of 25-40 in
#     Armenia compared to Austria?
# ==> Why is there a dip in all graphs at the ages of 66-67, why is it more pronounced in those representing Armenia?
# ==> Why are there more older females than older males in Austria?
# ==> Why is there a big jump in the age group from age 84 to age 85+ in Armenia, but not in Austria?

