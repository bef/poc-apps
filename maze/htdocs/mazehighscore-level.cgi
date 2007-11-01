#!/usr/bin/env tclsh
##
## highscore for the maze game
##

package require ncgi

ncgi::parse
source /var/lib/asterisk/agi-bin/mazeconfig.tcl
source /var/lib/asterisk/agi-bin/mazesql.tcl

ncgi::header


puts "<html><head><title>maze game</title></head> <body>"


::mazedb::connect_db
puts "<h2> Level based Highscore </h2>"
puts "<table><tr><th>rank</th><th>level rank</th><th>CID</th><th>name</th><th>level</th><th>time</th></tr>"
set cnt 1
for {set l 50} {$l >= 0} {incr l -1} {
	set hs_list [::mazedb::get_level_highscore $l 10]
	if {$hs_list != {}} {
		set i 1
		puts "<tr colspan=3><th>level $l</th></tr>"
		foreach hs_entry $hs_list {
			puts "<tr><th>$cnt</th><th>$i</th>"
			foreach cell $hs_entry {
				puts "<td>$cell</td>"
			}
			puts "</tr>"
			incr i
			incr cnt
		}
		puts "</tr>"
	}
}

puts "</table>"

puts "</form></body> </html>"
"
