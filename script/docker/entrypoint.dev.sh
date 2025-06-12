#!/bin/sh
set -e

qlot exec ros run -e "(ql:quickload :swank)" -e "(setf swank::*loopback-interface* \"0.0.0.0\")" -e "(swank:create-server :dont-close t :style :spawn)"
