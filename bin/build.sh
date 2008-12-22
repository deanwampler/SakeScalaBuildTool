#!/bin/bash

# Bootstrap build. If you run the specs target in the same process as the compilation, 
# you won't see the latest changes, because the previous build was already loaded
# from the classpath!

bin/sake clean compile || exit $?
bin/sake -cp build spec jar
