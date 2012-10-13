#!/usr/bin/env tclsh8.5
#
# Yate Script: login example
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2012-09-21
#
# Copyright (c) 2012, Ben Fuhrmannek
# All rights reserved.
# 
#
# example regexroute.conf entry:
# ^84$=external/nodata/conference.tcl;room=conf/42;lonely=true;maxusers=23;password=0001
#

## YGI setup
set auto_path [linsert $auto_path 0 [file dirname $::argv0]]
package require ygi

## start message handler 
::ygi::start_ivr

## debugging
#::ygi::print_env
#set ::ygi::debug true

## beep or wait
#::ygi::play_wait "beep"
::ygi::sleep 500

## password? (exit if pw set but entered incorrectly)
if {[info exists ::ygi::env(password)]} {
	::ygi::ask_password password $::ygi::env(password) exit_on_failure true
}

## initiate conference call
set confparams {existing smart echo voice counted billing utility player maxusers lonely record notify recordwarn rate room}
set params {}
foreach p $confparams {
	if {[info exists ::ygi::env($p)]} {
		lappend params $p $::ygi::env($p)
	}
}
#::ygi::log $params
set success [::ygi::msg call.conference {*}$params]

#::ygi::log $::ygi::lastresult(kv)
#::ygi::log $success

if {!$success} {
	::ygi::play_wait conference
	::ygi::play_wait is-curntly-unavail
} else {
	::ygi::play_wait yintro
}
::ygi::sleep 1000
exit 0

