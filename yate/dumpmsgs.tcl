#!/usr/bin/env tclsh8.5
#
# Yate Script: TCP connection example
# - dump all messages except engine.timer -
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2012-09-24
#
# Copyright (c) 2012, Ben Fuhrmannek
# All rights reserved.
#
# - call from command line -
# extmodule.conf listener example:
# [listener tcp5039]
# type=tcp
# addr=127.0.0.1
# port=5039
# 

## YGI setup
set auto_path [linsert $auto_path 0 [file dirname [info script]]]
package require ygi

## watch handler function
proc watch_handler {id processed name retvalue kv} {
	if {$name eq "engine.timer"} { return }
	puts "---------> new message: $name $id $processed $retvalue"
	foreach {k v} $kv {
		puts [format "| %18s %s" $k $v]
	}
}

## connect to socket
set fd [socket 127.0.0.1 5039]
set ::ygi::onexit {catch {close $::fd}}

## start message handler 
::ygi::start $fd $fd
#set ::ygi::debug true


## first command to socket must be connect
::ygi::connect global

## log or console message
::ygi::log "TCP connection example"

## install watch handler for some/all messages
#::ygi::watch "call.execute" watch_handler
::ygi::watch "" watch_handler

## never stop
::ygi::loop_forever

