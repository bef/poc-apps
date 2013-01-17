#!/usr/bin/env tclsh8.5
#
# Yate Script: access multiple conferences based on access code
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2013-01-16
#
# Copyright (c) 2012, Ben Fuhrmannek
# All rights reserved.
# 
#
# example regexroute.conf entry:
# ^85$=external/nodata/multiconference.tcl;configfile=/usr/local/share/yate/scripts/multiconference.conf;lonely=true
#

## YGI setup
set auto_path [linsert $auto_path 0 [file dirname $::argv0]]
package require ygi

## start message handler 
::ygi::start_ivr

## parse config file
package require inifile
if {![info exists ::ygi::env(configfile)]} { ::ygi::_exit 1 "ERROR: no configfile=... specified." }
if {![file readable $::ygi::env(configfile)]} { ::ygi::_exit  1 "ERROR: config file does not exist or is not readable" }

set ini [::ini::open $::ygi::env(configfile) r]
set defaultconfig {}
if {[::ini::exists $ini default]} {
	set defaultconfig [::ini::get $ini default]
}
foreach section [::ini::sections $ini] {
	if {$section eq "default"} { continue }
	set conferences($section) [dict merge $defaultconfig [::ini::get $ini $section]]
}
::ini::close $ini

## find password -> conference mapping for all conferences
array set passwords {}
foreach {section params} [array get conferences] {
	if {[dict exists $params password]} {
		set passwords([dict get $params password]) $section
		continue
	}
	if {[dict exists $params passwords]} {
		foreach pw [dict get $params passwords] {
			set passwords($pw) $section
		}
	}
}

if {[array size passwords] == 0} {
	::ygi::_exit 1 "ERROR: no password set in config file"
}

## beep or wait
::ygi::play_wait "yintro"
#::ygi::sleep 500

## ask for access code / exit unless password correct
set pwinput [::ygi::ask_password passwords [array names passwords] exit_on_failure true enter_pw_sound "please-enter-your-access-code"]

## SUCCESS
set section $passwords($pwinput)
::ygi::play_wait "yintro"

## assemble conference parameters (configfile defaults -> call variables -> configfile entry)
set confparams {existing smart echo voice counted billing utility player maxusers lonely record notify recordwarn rate room}
set all_params [dict merge [array get ::ygi::env] $conferences($section)]
set params {}
foreach p $confparams {
	if {[dict exists $all_params $p]} {
		lappend params $p [dict get $all_params $p]
	}
}
if {![dict exists $params room]} {
	lappend params room "conf/$section"
} elseif {[string first "conf/" [dict get $params room]] != 0} {
	dict set params room "conf/[dict get $params room]"
}

# initiate conference call
set success [::ygi::msg chan.masquerade id $::ygi::env(id) message call.execute callto [dict get $params room] {*}$params]

## INFO: script should be terminated here automatically on success.

if {!$success} {
	::ygi::play_wait im-sorry
	::ygi::play_wait conference-unavailable
	::ygi::play_wait please-try-again-later
}

::ygi::sleep 500
exit 0

