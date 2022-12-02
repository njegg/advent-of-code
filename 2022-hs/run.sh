#!/bin/bash

# compile and run most recently
ls -t *.hs | head -1 | xargs ghc -dynamic -o out && ./out

# delete ghc generated files 
ls -1 | grep -Ev "(.hs$)|(.txt$)$" | xargs trash 