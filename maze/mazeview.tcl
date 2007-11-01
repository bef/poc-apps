#!/usr/bin/env tclsh
## usage: ./mazeview.tcl level cid
source agi-bin/mazeconfig.tcl
source agi-bin/mazegen.tcl
source agi-bin/mazecode.tcl


set level [lindex $::argv 0]
set cid [lindex $::argv 1]
gen_maze $level $cid
for {set z 0} {$z < $::sz(z)} {incr z} {
	puts "\nlayer: $z"
	puts [join [show_level $z] "\n"]
}
puts "code: [code $level $cid]"

