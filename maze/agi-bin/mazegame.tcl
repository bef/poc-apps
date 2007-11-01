#!/usr/bin/tclsh
##
## maze game
##	(c) BeF <bef@erlangen.ccc.de>
##

###############################################################################



package require festtcl
package require agi

agi::init
agi::answer


##
if {[catch {
##

###############################################################################

source [file join [file dirname $::argv0] mazeconfig.tcl]
source [file join [file dirname $::argv0] mazegen.tcl]
source [file join [file dirname $::argv0] mazecode.tcl]
if {$use_highscore} {
	source [file join [file dirname $::argv0] mazesql.tcl]
}

## accept language code as first parameter: de/en
if {[llength $::argv] > 0} {
	set language [lindex $::argv 0]
}

## asterisk < 1.2
#agi::exec SetLanguage $language
## asterisk >= 1.2
agi::exec Set "LANGUAGE()=$language"

###############################################################################
array set sounds {}
switch $language {
	en {
		set sounds(intro) {
			{beep maze/en/init/welcome maze/en/init/begin maze/en/init/skip_help maze/en/init/help maze/en/init/fnord}
			}
		set sounds(help) {
			{maze/en/init/help}
			}
		set sounds(level_intro) {
			{maze/en/init/commence_level}
			}
		set sounds(stair_error_UP) {
			{maze/en/stairs/go_up}
			{maze/en/stairs/try_sth_else}
			}
		set sounds(stair_error_DOWN) {
			{maze/en/stairs/go_down}
			{maze/en/stairs/try_sth_else}
			}
		set sounds(wall_error) {
			{maze/en/wall/ran_into}
			{maze/en/wall/ouch}
			{maze/en/wall/any_further}
			{maze/en/wall/change_dir}
			{maze/en/wall/no_way_ahead}
			{maze/en/wall/solid_matter}
			{maze/en/wall/suicide}
			{maze/en/wall/not_here}
			}
		set sounds(enter_level_nr) {
			{maze/en/level/enter_level}
			}
		set sounds(enter_level_code) {
			{maze/en/level/enter_code}
			}
		set sounds(level_code_incorrect) {
			{maze/en/level/incorrect_code}
			{maze/en/level/guess_incorrect}
			{maze/en/level/not_meant_for_you}
			{maze/en/level/wrong_code}
			}
		set sounds(level_code) {
			{{maze/en/level/level_code} {maze/en/level/is}}
			}
		set sounds(exit_entrance) {
			{maze/en/exit/found_entrance}
			{maze/en/exit/entrance}
			}
		set sounds(compass_NORTH) {
			{maze/en/compass/headed_north}
			{maze/en/compass/compass_north}
			{maze/en/compass/north}
			}
		set sounds(compass_SOUTH) {
			{maze/en/compass/headed_south}
			{maze/en/compass/compass_south}
			{maze/en/compass/south}
			}
		set sounds(compass_EAST) {
			{maze/en/compass/headed_east}
			{maze/en/compass/compass_east}
			{maze/en/compass/east}
			}
		set sounds(compass_WEST) {
			{maze/en/compass/headed_west}
			{maze/en/compass/compass_west}
			{maze/en/compass/west}
			}
		set sounds(level_succeeded) {
			{maze/en/exit/well_done}
			{maze/en/exit/congratulations}
			}
		set sounds(imp_wall) {
			{maze/en/impression/facing_wall}
			}
		set sounds(imp_free) {
			{maze/en/impression/move_on}
			}
		set sounds(imp_intersect) {
			{maze/en/impression/move_or_turn}
			}
		set sounds(game_over) {
			{/home/bef/sounds/blaue_berge}
			{{/home/bef/sounds/Zimmer Frei Theme_telefon}}
			}

	}
	de -
	default {
		set sounds(intro) {
			{beep maze/de/init/willkommen maze/de/init/skip_intro maze/de/init/help}
			{beep maze/de/init/hallo_erstmal maze/de/init/herumlaber maze/de/init/help2 maze/de/init/viel_spass}
			}
		set sounds(help) {
			{maze/de/init/help}
			{maze/de/init/help2}
			}
		set sounds(level_intro) {
			{maze/de/init/level}
			{maze/de/init/beginnt_level}
			}
		set sounds(stair_error_UP) {
			{maze/de/stairs/keine_treppe_nach_oben}
			{maze/de/stairs/nicht_hoch}
			}
		set sounds(stair_error_DOWN) {
			{maze/de/stairs/keine_treppe_nach_unten}
			{maze/de/stairs/nicht_runter}
			}
		set sounds(wall_error) {
			{maze/de/wall/hier_gehts_nicht_weiter}
			{maze/de/wall/au}
			{maze/de/wall/richtungsaenderung}
			{maze/de/wall/andere_tasten}
			{maze/de/wall/umdreh}
			{maze/de/wall/nicht_weiter}
			{maze/de/wall/durch_waende}
			{maze/de/wall/mauer}
			{maze/de/wall/versuch_kein_durchgang}
			{maze/de/wall/kein_durchgang}
			{maze/de/wall/nicht_durch_wand}
			{maze/de/wall/solide_mauer}
			}
		set sounds(enter_level_nr) {
			{maze/de/level/eingabe_level}
			{maze/de/level/eingabe_level2}
			}
		set sounds(enter_level_code) {
			{maze/de/level/eingabe_code}
			{maze/de/level/eingabe_code2}
		}
		set sounds(level_code_incorrect) {
			{maze/de/level/code_falsch}
			{maze/de/level/code_nicht_korrekt}
			{maze/de/level/verwaehlt}
			}
		set sounds(level_code) {
			{{maze/de/level/der_levelcode} {maze/de/level/ist}}
			{{maze/de/level/level} {maze/de/level/hat_den_code}}
			{{maze/de/level/in_level} {maze/de/level/code_lautet}}
			}
		set sounds(exit_entrance) {
			{maze/de/exit/eingang_gefunden}
			{maze/de/exit/eingang_geflohen}
			{maze/de/exit/war_eingang}
			}
		set sounds(compass_NORTH) {
			{maze/de/compass/blick_norden}
			{maze/de/compass/kompass_norden}
			{maze/de/compass/norden}
			{maze/de/compass/richtung_norwegen}
			}
		set sounds(compass_SOUTH) {
			{maze/de/compass/blick_sueden}
			{maze/de/compass/kompass_sueden}
			{maze/de/compass/sueden}
			{maze/de/compass/richtung_italien}
			}
		set sounds(compass_EAST) {
			{maze/de/compass/blick_osten}
			{maze/de/compass/kompass_osten}
			{maze/de/compass/osten}
			{maze/de/compass/richtung_polen}
			}
		set sounds(compass_WEST) {
			{maze/de/compass/blick_westen}
			{maze/de/compass/kompass_westen}
			{maze/de/compass/westen}
			{maze/de/compass/richtung_frankreich}
			}
		set sounds(level_succeeded) {
			{maze/de/exit/geschafft}
			{maze/de/exit/jaa}
			{maze/de/exit/na_endlich}
			{maze/de/exit/kinderspiel}
			{maze/de/exit/kein_aufwand}
			{maze/de/exit/toe_toe}
			}
		set sounds(imp_wall) {
			{maze/de/impression/wand}
			{maze/de/impression/wand2}
			}
		set sounds(imp_free) {
			{maze/de/impression/geradeaus}
			{maze/de/impression/geh_weiter}
			{maze/de/impression/frei}
			}
		set sounds(imp_intersect) {
			{maze/de/impression/abzweigung}
			{maze/de/impression/koenntest_abbiegen}
			}
		set sounds(game_over) {
			{/home/bef/sounds/blaue_berge}
			{{/home/bef/sounds/Zimmer Frei Theme_telefon}}
			}
	}
}

###############################################################################
## init vars
set cid $agi::env(agi_callerid)
set running 1
set input ""
set outfiles {}
set level 0
array set pos {x 0 y 0 z 0 dir EAST}
set DIRS {NORTH EAST SOUTH WEST}
set starttime 0
set endtime 0

## no cid error
if {$cid == ""} {
	agi::verbose "no cid" 4
	agi::hangup
}

###############################################################################
## functions ##

## stream soundfile(s) until * is pressed
proc stream {soundfile_list} {
	foreach f $soundfile_list {
		set digit [agi::streamfile $f "*"]
		if {$digit == "*"} { break }
	}
}

## stream soundfile(s) until $digits digits are pressed
proc get_data {soundfile_list wait digits} {
	set first [lrange $soundfile_list 0 [expr {[llength $soundfile_list]-2}]]
	set last [lindex $soundfile_list end]
	set input ""
	set firstwait $wait
	if {$digits == 1} { set firstwait 1000 }
	foreach f $first {
		set input [agi::getdata $f $firstwait $digits]
		if {$input != ""} { break }
	}
	if {$input == ""} {
		set input [agi::getdata $last $wait $digits]
	}
	return $input
}

## return a random element from the list l
proc random_choice {l} {
	expr {srand([clock clicks])}
	return [lindex $l [expr {int(rand()*[llength $l])}]]
}


###############################################################################

## intro
stream [random_choice $sounds(intro)]

## game loop
while { 1 } {
	gen_maze $level $cid
	foreach line [show_level 0] { agi::verbose "$cid $line" 3 }
	
	stream [random_choice $sounds(level_intro)]
	agi::saynumber $level
	
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
			set input [get_data $outfiles 10000 1]
		}
		set outfiles {}
		agi::verbose "CID $cid, LEVEL $level POS $pos(x)x$pos(y)x$pos(z)" 4
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
				set inlevel [agi::getdata [random_choice $sounds(enter_level_nr)] 10000 2]
				set incode [agi::getdata [random_choice $sounds(enter_level_code)] 10000 4]
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
				agi::saynumber $level
				stream [lindex $snd 1]
				agi::saydigits [code $level $cid]
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
					agi::verbose "db error: $eid" 3
				}
			}

			## switch to next level
			incr level

			## announce level code
			set snd [random_choice $sounds(level_code)]
			stream [lindex $snd 0]
			agi::saynumber $level
			stream [lindex $snd 1]
			agi::saydigits [code $level $cid]

		}
	}

	if {$level > $::maxlevel} {
		## winner
		set level $::maxlevel
		stream [random_choice $sounds(game_over)]
		agi::exec echo ""
		break
	}
	
}

##
} fid]} {
	agi::verbose "error $fid" 4
}
##
