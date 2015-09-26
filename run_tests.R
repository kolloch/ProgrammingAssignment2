# If RUnit is missing, install with: 
# > install.packages("RUnit")
library('RUnit')

source('cachematrix.R')

test.suite <- defineTestSuite("cachematrix",
                              dirs = file.path("tests"),
                              testFileRegexp = '^test\\..*\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)