#!/bin/sh
set -e

ros run -e "(ql:quickload :swank)" -e "(setf swank::*loopback-interface* \"0.0.0.0\")" -e "(swank:create-server :dont-close t :style :spawn)"
