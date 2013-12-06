#!/usr/bin/env tclsh8.5
#
# Yate Script: say current time
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2012-09-21
#
# Copyright (c) 2012, Ben Fuhrmannek
# All rights reserved.
# 
# - call from dialplan -
# example regexroute.conf entry:
# ^80$=external/nodata/time.tcl
#


## YGI setup
set auto_path [linsert $auto_path 0 [file dirname $::argv0]]
package require ygi

## start message handler 
::ygi::start_ivr

## play intro sound and wait for the audio to come through properly
::ygi::play_wait "yintro"
::ygi::sleep 500

## say current time a few times, then hangup
for {set i 0} {$i < 3} {incr i} {

	set time [clock format [clock seconds] -format "%k:%M"]
	foreach {h m} [split $time ":"] {}
	set h [string trim $h]
	set m [string trimleft $m "0"]

	set out {time silence/200u}

	if {$h <= 23} {
		lappend out "digits/$h"
	} else {
		lappend out "digits/20" "digits/[expr {$h - 20}]"
	}
	lappend out silence/150u

	if {$m == 0} {
		lappend out "digits/oclock"
	} elseif {$m < 10} {
		lappend out "letters/o" "digits/$m"
	} elseif {$m <= 20} {
		lappend out "digits/$m"
	} else {
		lappend out "digits/[expr {$m/10*10}]"
		if {[expr {$m % 10}]} { lappend out "digits/[expr {$m % 10}]" }
	}

	foreach f $out {
		::ygi::play_wait $f
	}

	::ygi::sleep 1500
}

## end.

