#!/bin/sh
sbcl --non-interactive --load stazy.asd --eval '(ql:quickload :stazy)' --eval '(asdf:make :stazy)' --eval '(quit)'
