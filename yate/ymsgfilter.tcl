#!/usr/bin/env tclsh8.6
#
# Yate Message Filter/Dumper
# - dump all messages meeting certain criteria -
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2013-11-30
#
# Copyright (c) 2013, Ben Fuhrmannek
# All rights reserved.

## cmd opts
package require cmdline
set options {
	{H.arg		"localhost"	"connect to host"}
	{P.arg		"5039"		"connect to port"}
	{m.arg		""		"message filter, e.g. user.register"}
	{kv.arg		""		"key/value match filter, e.g. username=1234"}
}

set usage "$::argv0 <options>..."
if {[catch {
	array set cfg [::cmdline::getoptions argv $options $usage]
} e]} {
	puts stderr $e
	exit 1
}


## YGI setup
set auto_path [linsert $auto_path 0 [file dirname [info script]]]
package require ygi

proc dump_msg {_id processed name retvalue kv} {
	puts "---------> new message: $name | $processed | $retvalue"
	foreach {k v} $kv {
		puts [format "| %18s %s" $k $v]
	}
}
## watch handler function
proc watch_handler {args} {
	if {$::cfg(kv) eq ""} {
		dump_msg {*}$args
		return
	}
	
	lassign $args _id processed name retvalue kv
	lassign [split $::cfg(kv) "="] key value
	if {![dict exists $kv $key]} {return}
	if {[dict get $kv $key] ne $value} {return}
	dump_msg {*}$args
}




## start.
::ygi::start_tcp $cfg(H) $cfg(P)
# set ::ygi::debug true


## first command to socket must be connect
::ygi::connect global

## log or console message
::ygi::log "start"

## install watch handler(s)
::ygi::watch $cfg(m) watch_handler

## never stop
::ygi::loop_forever

