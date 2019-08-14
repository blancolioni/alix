#!/bin/sh
rm -rf bin
rm -rf obj
rm -rf dependencies
mkdir bin
mkdir obj
gprbuild setup.gpr
./bin/setup_driver posix
