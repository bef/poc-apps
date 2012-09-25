#!/usr/bin/env tclsh8.5
#
# Yate Script: script timeout example
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2012-09-21
#
# Copyright (c) 2012, Ben Fuhrmannek
# All rights reserved.
# 

## YGI setup
set auto_path [linsert $auto_path 0 [file dirname $::argv0]]
package require ygi

## do something just before the end.
proc onexit {} {
	## cleanup. close database. log the end. send emails. call someone. beep. ...
	::ygi::log "THE END. (too bad)"
}
set ::ygi::onexit onexit

## kill script after 20 seconds
::ygi::script_timeout 20

## kill script after 10 seconds of inactivity. check for timeout every 2 seconds
::ygi::idle_timeout 10 2

## start message handler 
::ygi::start_ivr


## do something important here....
## !?


## event loop
::ygi::loop_forever

