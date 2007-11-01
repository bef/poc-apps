#!/usr/bin/tclsh
##
##	AGI random menschenrechte
##		BeF <bef@erlangen.ccc.de>
##
##	exten => 8008,1,agi(mr.tcl,mr/mr,mr/art,1,30)
##
package require agi
package require festtcl

#################################################
## init
agi::init
agi::answer

##
if {[catch {
##

## var init
set initsound [lindex $::argv 0]
set soundprefix [lindex $::argv 1]
set start [lindex $::argv 2]
set end [lindex $::argv 3]

## rand init
expr {srand([clock clicks])}

## init sound
set nr [agi::getdata $initsound 3000 2]

## play
for {set round 0} {$round < [expr {2*($end - $start + 1)}]} {incr round} {
	if {[string length $nr] != 2} {
		set rnd [expr {int(rand() * ($end - $start + 1) + $start)}]
		set nr [format "%02d" $rnd]
	}

	set num_nr [string trimleft $nr 0]
	if {[expr {![string is integer $num_nr] || $num_nr < $start || $num_nr > $end}]} {
		agi::verbose "no such input: '$nr'"
		agi::streamfile beep
		set nr [agi::getdata $initsound 3000 2]
	} else {
		set nr [agi::getdata $soundprefix$nr 3000 2]
	}
}


##
} fid]} {
	agi::verbose "error $fid" 4
}
##
