#!/usr/bin/env tclsh

package require Tcl 8.5
package require tcltest

eval ::tcltest::configure $argv

## YGI setup
set auto_path [linsert $auto_path 0 [file dirname $::argv0]]
package require ygi

namespace eval ::ygi::test {
	namespace import ::tcltest::*
	
	# proc ::ygi::_split_kv 
	test _splitkv {} -body {
		::ygi::_split_kv {a=b c=d e=f}
	} -result {a b c d e f}
	
	# proc ::ygi::_join_kv 
	test _joinkv {} -body {
		::ygi::_join_kv {a b c d e f}
	} -result {a=b c=d e=f}
	
	
	# proc ::ygi::yencode 
	set lambda {::ygi::yencode [format "%c" $i]}
	for {set i 0} {$i < 32} {incr i} {
		test yencode-$i {} -body "apply {i {$lambda}} $i" -result "%[format "%c" [expr {$i + 64}]]"
	}
	for {set i 32} {$i < 256} {incr i} {
		switch $i {
			37 {set result "%%"}
			58 {set result "%z"}
			default {set result [format "%c" $i]}
		}
		test yencode-$i {} -body "apply {i {$lambda}} $i" -result $result
	}



	# proc ::ygi::ydecode
	set out ""
	for {set i 0} {$i < 32} {incr i} {
		append out [format "%c" $i]
	}
	test ydecode-0-31 {} -body {
		set in ""
		for {set i 0} {$i < 32} {incr i} {
			append in "%" [format "%c" [expr {$i + 64}]]
		}
		::ygi::ydecode $in
	} -result $out

	set out ""
	for {set i 32} {$i < 256} {incr i} {
		append out [format "%c" $i]
	}
	test ydecode-32-255 {} -body {
		set in ""
		for {set i 32} {$i < 256} {incr i} {
			switch $i {
				37 {append in "%%"}
				58 {append in "%z"}
				default {append in [format "%c" $i]}
			}
		}
		::ygi::ydecode $in
	} -result $out

	test ydecode-invalid-0-63 {} -body {
		set in ""
		for {set i 0} {$i < 64} {incr i} {
			if {$i == 37} {continue}
			append in "%" [format "%c" $i]
		}
		::ygi::ydecode $in
	} -result ""

	set out ""
	for {set i 32} {$i < 192} {incr i} {
		append out [format "%c" $i]
	}
	test ydecode-encoded-32-191 {} -body {
		set in ""
		for {set i 32} {$i < 192} {incr i} {
			append in "%" [format "%c" [expr {$i + 64}]]
		}
		::ygi::ydecode $in
	} -result $out


	# proc ::ygi::yencodelist
	test yencodelist {} -body {
		::ygi::yencodelist {"\x05:" "\xff " "abc"}
	} -result [list [::ygi::yencode "\x05:"] [::ygi::yencode "\xff "] [::ygi::yencode "abc"]]
	
	# proc ::ygi::ydecodelist 
	test ydecodelist {} -body {
		::ygi::ydecodelist [list [format "%%%c%%z" {69}] "\xff " "abc"]
	} -result [list "\x05:" "\xff " "abc"]
	
	# proc ::ygi::uniqueid 
	test uniqueid {} -body {
		set uid1 [::ygi::uniqueid]
		set uid2 [::ygi::uniqueid]
		expr {$uid1 ne $uid2}
	} -result 1
	
	# lmap - own implementation
	if {$::tcl_version eq "8.5"} {
		test lmap-one-var {} -body {
			lmap i {1 2 3 4} {expr {$i * 2}}
		} -result {2 4 6 8}
		test lmap-two-vars {} -body {
			lmap {k v} {a 1 b 2} {set _ "$k=$v"}
		} -result {a=1 b=2}
	}

	# proc ::ygi::filter_env 
	test filter_env {} -body {
		array set ::ygi::env {a b c d e f}
		::ygi::filter_env a e
	} -result {a b e f}

	# proc ::ygi::dict_args 
	test dict_args {} -body {
		set y 9
		set args {a 0 e 1 y 0}
		::ygi::dict_args {a b c d e f}
		set _ "$a$c$e$y"
	} -result "0d19"


	## functions not currently tested with this testsuite:
	# proc ::ygi::_input 
	# proc ::ygi::_exit 
	# proc ::ygi::_ivr_call_execute_handler 
	# proc ::ygi::_notify_handler 
	# proc ::ygi::_dtmf_handler 
	# proc ::ygi::_raw_write 
	# proc ::ygi::_write 
	# proc ::ygi::_find_soundfile 
	# proc ::ygi::log 
	# proc ::ygi::forcelog 
	# proc ::ygi::start 
	# proc ::ygi::start_tcp 
	# proc ::ygi::start_ivr 
	# proc ::ygi::loop_forever 
	# proc ::ygi::connect 
	# proc ::ygi::message 
	# proc ::ygi::msg 
	# proc ::ygi::install 
	# proc ::ygi::uninstall 
	# proc ::ygi::watch 
	# proc ::ygi::unwatch 
	# proc ::ygi::setlocal 
	# proc ::ygi::sleep 
	# proc ::ygi::print_env 
	# proc ::ygi::waitfornotify 
	# proc ::ygi::play 
	# proc ::ygi::play_wait 
	# proc ::ygi::silence 
	# proc ::ygi::clear_dtmfbuffer 
	# proc ::ygi::getdigit 
	# proc ::ygi::getdigits 
	# proc ::ygi::script_timeout 
	# proc ::ygi::idle_timeout 
	# proc ::ygi::heartbeat 
	# proc ::ygi::quit 
	# proc ::ygi::ask_password 
	
	cleanupTests
}
namespace delete ::ygi::test