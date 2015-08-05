# run tests
library('RUnit')

source('ar_hmm.R')

test.suite <- defineTestSuite("test",
                              dirs = file.path("tests"))

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)