#!/usr/bin/env tclsh8.5
#
# Yate Script: unix domain socket connection example (kind of)
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2012-09-25
#
# Copyright (c) 2012, Ben Fuhrmannek
# All rights reserved.
#
# - call from command line -
# extmodule.conf listener example:
# [listener unix]
# type=unix
# path=/tmp/yate.sock
# 

## YGI setup
set auto_path [linsert $auto_path 0 [file dirname $::argv0]]
package require ygi


## Tcl cannot easily connect to unix domain sockets out of the box (as far as I know)
## evil hack: pipe netcat
set fd [open "|nc -U /tmp/yate.sock" r+]
set ::ygi::onexit {catch {close $::fd}}

## start message handler 
::ygi::start $fd $fd

## debugging
set ::ygi::debug true

## first command to socket must be connect
::ygi::connect global


## some business logic goes here.
::ygi::log "UNIX domain socket connection example"

## the end.
::ygi::quit
::ygi::loop_forever

