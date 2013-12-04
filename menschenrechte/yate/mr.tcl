#!/usr/bin/env tclsh8.6
#
# Yate Script: Play Menschenrechte
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2013-12-04
#
# Copyright (c) 2013, Ben Fuhrmannek
# All rights reserved.
# 

set auto_path [linsert $auto_path 0 [file dirname $::argv0]]
package require ygi

::ygi::script_timeout [expr {10*60}]
::ygi::start_ivr

## parameters -> variables
array set cfg {sound_prefix "mr/" mr_start 1 mr_end 30}
array set cfg [::ygi::filter_env [array names cfg]]

##

::ygi::play_wait "yintro"
::ygi::set_dtmf_notify

## intro
::ygi::play_wait "$cfg(sound_prefix)mr"

## play
while true {
	set nr [::ygi::getdigits digittimeout 3000 silence true maxdigits 2]
	set nr [string trimleft $nr 0]

	if {$nr eq "" || ![string is integer $nr]} {
		set nr [expr {int(rand() * ($cfg(mr_end) - $cfg(mr_start) + 1) + $cfg(mr_start))}]
	} elseif {$nr < $cfg(mr_start) || $nr > $cfg(mr_end)} {
		::ygi::play ybeeperr
		continue
	}
	
	::ygi::play_wait "$cfg(sound_prefix)art[format "%02d" $nr]"
}

