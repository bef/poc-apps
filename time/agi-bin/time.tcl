#!/usr/bin/env tclsh
##
##	time announcement
##		BeF <bef@eventphone.de>
##

package require agi

#################################################
## init
agi::init
agi::answer

if {[catch {

	for {set i 0} {$i < 3} {incr i} {
		agi::exec SetLanguage en
		agi::streamfile the_current_time_is
		agi::saytime [clock seconds]
	}

} fid]} {
	agi::verbose "error $fid" 4
}
