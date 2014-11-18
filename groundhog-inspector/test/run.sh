#!/bin/sh

runhaskell createMappings.hs $1 && runhaskell testMappings.hs $1
