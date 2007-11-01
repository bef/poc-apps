#!/usr/bin/env tclsh

package require ncgi

ncgi::parse
source /var/lib/asterisk/agi-bin/mazeconfig.tcl
source /var/lib/asterisk/agi-bin/mazesql.tcl

ncgi::header


puts "<html><head><title>maze game</title></head> <body>"


::mazedb::connect_db

set hs_list [::mazedb::get_highscore 20]
puts "<h2>Maze Highscore</h2>"
puts "<table><tr><th>rank</th><th>CID</th><th>name</th><th>highest level</th><th>time</th></tr>"
set i 1
foreach hs_entry $hs_list {
	puts "<tr><td>$i</td>"
	foreach hs_item $hs_entry {
		puts "<td>$hs_item</td>"
	}
	puts "</tr>"
	incr i
}
puts "</table>"

puts "</body> </html>"

