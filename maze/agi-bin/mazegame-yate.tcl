#!/usr/bin/env tclsh8.6
#
# Interactive Maze Game for Yate
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2013-12-06
#
# Copyright (c) 2013, Ben Fuhrmannek
# All rights reserved.
# 

set auto_path [linsert $auto_path 0 [file join [file dirname [file normalize [info script]]] .. .. yate]]
set auto_path [linsert $auto_path 0 [file join [file dirname [info script]] ..]]
set auto_path [linsert $auto_path 0 [file join [file dirname [info script]]]]
package require ygi

source [file join [file dirname $::argv0] mazeconfig.tcl]
source [file join [file dirname $::argv0] mazegen.tcl]
source [file join [file dirname $::argv0] mazecode.tcl]
if {$use_highscore} {
	source [file join [file dirname $::argv0] mazesql.tcl]
}

::ygi::idle_timeout 180 20
::ygi::script_timeout 3600
::ygi::start_ivr
::ygi::set_dtmf_notify
#set ::ygi::debug true

###############################################################################
## init vars
set language [::ygi::getenv language de]
set verbose [::ygi::getenv verbose 0]

# caller id
set cid [::ygi::getenv caller 0]

set running 1
set input ""
set outfiles {}
set level 0
array set pos {x 0 y 0 z 0 dir EAST}
set DIRS {NORTH EAST SOUTH WEST}
set starttime 0
set endtime 0
# 
# ## no cid error
# if {$cid == ""} {
# 	agi::verbose "no cid" 4
# 	agi::hangup
# }

###############################################################################
source [file join [file dirname $::argv0] mazesound.tcl]

###############################################################################
## functions ##

## stream soundfile(s) until * is pressed
proc stream {soundfile_list} {
	::ygi::play_getdigit filelist $soundfile_list stopdigits {*}
}

## stream soundfile(s) and wait for user input
proc get_data {soundfile_list {wait 10000} {maxdigits 1}} {
	set digits [::ygi::play_getdigit filelist $soundfile_list]
	if {$digits ne ""} {incr maxdigits -1}
	
	if {$maxdigits} {
		append digits [::ygi::getdigits maxdigits $maxdigits digittimeout $wait]
	}
	
	return $digits
}

## return a random element from the list l
proc random_choice {l} {
	return [lindex $l [expr {int(rand()*[llength $l])}]]
}

proc log {msg} {
	if {!$::verbose} {return}
	::ygi::log $msg
}
###############################################################################

## intro
stream [random_choice $sounds(intro)]

## game loop
while { 1 } {
	## generate maze
	gen_maze $level $cid
	
	## make rand() unpredictable
	expr {srand([clock clicks])}

	## print maze on console (parameter 0 means z-level 0. no 3D logfile yet.)
	::ygi::log "CID $cid, LEVEL $level"
	foreach line [show_level 0] { log "$cid $line" }
	
	stream [random_choice $sounds(level_intro)]
	::ygi::say_number $level $language
	
	set running 1
	set pos(x) 0
	set pos(y) [regsub {0,(\d+),0} $::sz(start) {\1}]
	set pos(z) 0
	set pos(dir) EAST
	set starttime [clock seconds]
	set endtime $starttime

	while {$running} {
		set m $::maze($pos(x),$pos(y),$pos(z))
		set dir_left [lindex $DIRS [expr {([lsearch $DIRS $pos(dir)] - 1 )%4}]]
		set dir_right [lindex $DIRS [expr {([lsearch $DIRS $pos(dir)] + 1 )%4}]]
		set dir_behind [lindex $DIRS [expr {([lsearch $DIRS $pos(dir)] + 2 )%4}]]

		set input ""
		if {$outfiles != {}} {
			set input [get_data $outfiles]
		}
		set outfiles {}
		log "CID $cid, LEVEL $level POS $pos(x)x$pos(y)x$pos(z)"
		switch $input {
			4 {
				## turn left
				set pos(dir) $dir_left
			}
			6 {
				## turn right
				set pos(dir) $dir_right
			}
			2 {
				## go upstairs
				if {$m & $::DOOR(UP)} {
					set newpos [MOVETO $pos(x) $pos(y) $pos(z) $DIR(UP)]
					set pos(x) [lindex $newpos 0]
					set pos(y) [lindex $newpos 1]
					set pos(z) [lindex $newpos 2]
				} else {
					set outfiles [random_choice $sounds(stair_error_UP)]
				}
			}
			8 {
				## go downstairs
				if {$m & $::DOOR(DOWN)} {
					set newpos [MOVETO $pos(x) $pos(y) $pos(z) $DIR(DOWN)]
					set pos(x) [lindex $newpos 0]
					set pos(y) [lindex $newpos 1]
					set pos(z) [lindex $newpos 2]
				} else {
					set outfiles [random_choice $sounds(stair_error_DOWN)]
				}
			}
			5 {
				## go ahead
				if {$m & $::WALL($pos(dir))} {
					set outfiles [random_choice $sounds(wall_error)]
				} else {
					set newpos [MOVETO $pos(x) $pos(y) $pos(z) $DIR($pos(dir))]
					set pos(x) [lindex $newpos 0]
					set pos(y) [lindex $newpos 1]
					set pos(z) [lindex $newpos 2]
				}
			}
			0 {
				## enter level code / change level
				set inlevel [get_data [random_choice $sounds(enter_level_nr)] 10000 2]
				set incode [get_data [random_choice $sounds(enter_level_code)] 10000 4]
				regsub {^0} $inlevel {} inlevel
				regsub -all {[^\d]} $inlevel {} inlevel
				if {![string is integer "$inlevel"]} { set inlevel 0 }
				if {$inlevel == ""} { set inlevel 0 }
				if {$incode != [code $inlevel $cid]} {
					stream [random_choice $sounds(level_code_incorrect)]
				} else {
					set level $inlevel
					break
				}
			}
			9 {
				## announce level code
				set snd [random_choice $sounds(level_code)]
				stream [lindex $snd 0]
				::ygi::say_number $level $language
				stream [lindex $snd 1]
				::ygi::say_digits [code $level $cid] $language
			}
			1 {
				## help
				stream [random_choice $sounds(help)]
			}
			#3 {
				## bible
			#	agi::streamfile {/home/bef/sounds/Genesis_01}
			#}
			{*} {
				## compass
				set outfiles [random_choice $sounds(compass_$pos(dir))]
			}
			default {
				## impression

				set corridors 0
				foreach side [list $::WALL($dir_left) $::WALL($dir_right) $::WALL($pos(dir)) $::WALL($dir_behind) $::WALL(UP) $::WALL(DOWN)] {
					if {[expr {!($m & $side)}]} { incr corridors }
				}
				if {$corridors > 2} {
					set outfiles [random_choice $sounds(imp_intersect)]
				} else {
					if {$m & $::WALL($pos(dir))} {
						set outfiles [random_choice $sounds(imp_wall)]
					} else {
						set outfiles [random_choice $sounds(imp_free)]
					}
				}
			}
		}

		if {$pos(x) == -1} {
			## exit through entrance
			set running 0
			stream [random_choice $sounds(exit_entrance)]
		}
		if {$pos(x) == $::sz(x)} {
			## exit through exit
			set running 0
			stream [random_choice $sounds(level_succeeded)]

			set endtime [clock seconds]
			## log to highscore
			if {$use_highscore} {
				if {[catch {
					::mazedb::connect_db
					::mazedb::add_to_highscore $cid $level $starttime $endtime
					::mazedb::disconnect_db
				} eid]} {
					::ygi::log "db error: $eid"
				}
			}

			## switch to next level
			incr level

			## announce level code
			set snd [random_choice $sounds(level_code)]
			stream [lindex $snd 0]
			::ygi::say_number $level $language
			stream [lindex $snd 1]
			::ygi::say_digits [code $level $cid] $language

		}
	}

	if {$level > $::maxlevel} {
		## winner
		set level $::maxlevel
		stream [random_choice $sounds(game_over)]
		::ygi::log "$cid wins at level $level."
		break
	}
	
}
