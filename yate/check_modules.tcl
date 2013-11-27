#!/usr/bin/env tclsh8.6
##
## this script checks if all modules and only existing modules are listed in yate.conf
##

package require fileutil
package require cmdline

set options {
	{lib.arg		"/usr/local/lib/yate"	"where all the yate modules are"}
	{yate.conf.arg	"/usr/local/etc/yate/yate.conf"				"yate.conf"}
}

set usage "$::argv0 <options>..."
if {[catch {
	array set cfg [::cmdline::getoptions argv $options $usage]
} e]} {
	puts stderr $e
	exit 1
}

set all_modules [lmap entry [glob -directory $cfg(lib) -- *.yate */*.yate] {
	lindex [file split $entry] end
}]

set yateconf [::fileutil::cat -- $cfg(yate.conf)]
regexp -lineanchor -- {^\[modules\].*?\n\[} $yateconf all
set yateconf_modules {}
foreach {_ entry} [regexp -all -inline -line -- {^;?([a-zA-Z].*\.yate)=(?:true|false)} $all] {
	lappend yateconf_modules $entry
}

puts "* all modules:\n  $all_modules"
puts "* yateconf modules:\n  $yateconf_modules"
puts "-------------"
puts "* should be added to $cfg(yate.conf):"
foreach entry $all_modules {
	if {[lsearch -exact $yateconf_modules $entry] >= 0} {continue}
	puts ";${entry}=true"
}
puts "-------------"
puts "* can be removed:"
foreach entry $yateconf_modules {
	if {[lsearch -exact $all_modules $entry] >= 0} {continue}
	puts $entry
}
