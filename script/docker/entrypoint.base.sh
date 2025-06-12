#!/bin/sh
set -e

export CL_SOURCE_REGISTRY=/reddit/
export PATH=$HOME/.roswell/bin:$PATH

ros install qlot
