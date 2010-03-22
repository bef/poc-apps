#!/usr/bin/tclsh8.5
source fastagi.tcl


## init logging
package require log
namespace import ::log::log
log::lvSuppressLE info 0

log info "start."

## fastagi callbacks
proc new_player {chan} {
	puts "===> new player $chan"
	# puts [::fastagi::result [::fastagi::send_cmd $chan {channel status}] result]
	puts [::fastagi::result_cmd $chan {channel status}]
	::fastagi::send_cmd $chan answer
	puts [::fastagi::result_cmd $chan {channel status}]
	::fastagi::send_cmd $chan hangup
}

proc chan_closed {chan} {
	puts "===> chan closed $chan"
}
set ::fastagi::callback(connected) new_player
set ::fastagi::callback(closed) chan_closed

## start server
::fastagi::start_server -myaddr 127.0.0.1 1324


vwait forever
## close?
log info "end."
