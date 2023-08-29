#!/bin/sh
set -e

export CL_SOURCE_REGISTRY=/reddit/

ros run -e "(ql:quickload :reddit)" -e "(ql:quickload :reddit-db)" -e "(ql:quickload :reddit-test)" -e "(asdf:test-system :reddit-test)"
