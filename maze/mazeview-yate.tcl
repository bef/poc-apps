#!/usr/bin/env tclsh
## usage: ./mazeview.tcl level cid

set auto_path [linsert $auto_path 0 [file join [file dirname [file normalize [info script]]] .. .. yate]]
set auto_path [linsert $auto_path 0 [file join [file dirname [info script]] ..]]
set auto_path [linsert $auto_path 0 [file join [file dirname [info script]]]]

source [file join [file dirname $::argv0] mazeconfig.tcl]
source [file join [file dirname $::argv0] mazegen.tcl]
source [file join [file dirname $::argv0] mazecode.tcl]



set level [lindex $::argv 0]
set cid [lindex $::argv 1]
gen_maze $level $cid
for {set z 0} {$z < $::sz(z)} {incr z} {
	puts "\nlayer: $z"
	puts [join [show_level $z] "\n"]
}
puts "code: [code $level $cid]"

