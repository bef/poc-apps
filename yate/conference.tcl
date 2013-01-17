#!/usr/bin/env tclsh8.5
#
# Yate Script: conference with optional password protection
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
::ygi::play_wait "yintro"
#::ygi::sleep 500

## password? (exit if pw set but entered incorrectly)
if {[info exists ::ygi::env(password)]} {
	::ygi::ask_password password $::ygi::env(password) exit_on_failure true
}

::ygi::play_wait "yintro"

## initiate conference call
set confparams {existing smart echo voice counted billing utility player maxusers lonely record notify recordwarn rate room}
set params [::ygi::filter_env $confparams]
set success [::ygi::msg chan.masquerade id $::ygi::env(id) message call.execute callto $::ygi::env(room) {*}params]

## INFO: script should be terminated here automatically on success.

if {!$success} {
	::ygi::play_wait im-sorry
	::ygi::play_wait conference-unavailable
	::ygi::play_wait please-try-again-later
}

::ygi::sleep 500
exit 0

