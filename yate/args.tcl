#!/usr/bin/env tclsh8.5
#
# Yate Script: example: pass arguments from dialplan to script
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2012-09-21
#
# Copyright (c) 2012, Ben Fuhrmannek
# All rights reserved.
# 
#
# example regexroute.conf entry:
# ^83$=external/nodata/args.tcl arg1 arg2 "arg 3";ext_foo=bar
#

## ygi init
set auto_path [linsert $auto_path 0 [file dirname $::argv0]]
package require ygi

::ygi::start_ivr

## debugging
::ygi::print_env
#set ::ygi::debug true

## check for command line arguments
if {$::argc > 0} {
	## extmodule can only pass one parameter (or none), which means
	## all parameters are contained in the first element of $::argv
	## (also: assume argv element 0 to be a Tcl list)
	set ::argv [lindex $::argv 0]
	set ::argc [llength $::argv]
	::ygi::log "ARGV: [join $::argv ", "]"
}

## variables passed with call.execute can be found in ::ygi::env
## NOTE: When passing channel variables it's best to prefix them (e.g. ext_...)
##       in order to avoid collisions!
if {[info exists ::ygi::env(ext_foo)]} {
	::ygi::log "env(ext_foo): $::ygi::env(ext_foo)"
}

## Also: Don't forget to reload the dialplan (e.g. Ctrl-\ in rmanager) if needed.

