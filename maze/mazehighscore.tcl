#!/usr/bin/env tclsh
source agi-bin/mazeconfig.tcl
source agi-bin/mazesql.tcl

::mazedb::connect_db

puts "--- CID based Highscore ---"
set hs_list [::mazedb::get_highscore]
set i 1
foreach hs_entry $hs_list {
	puts "$i $hs_entry"
	incr i
}

puts "--- Level based Highscore ---"
for {set l 50} {$l >= 0} {incr l -1} {
	set hs_list [::mazedb::get_level_highscore $l 10]
	if {$hs_list != {}} {
		set i 1
		puts "level $l"
		foreach hs_entry $hs_list {
			puts "$i $hs_entry"
		}
	}

}

::mazedb::disconnect_db
