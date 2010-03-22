##
## Tcl FastAGI server
## 2010-03-15 - BeF <bef@eventphone.de>
##

package require log

## start namespace
namespace eval ::fastagi {
##

namespace import ::log::log

## agi environment variables - agienv(channel,key) = value
variable agienv

## callbacks: connected, closed
variable callback

## monitoring interval
variable monitor_timeout
set monitor_timeout 4000


#### internal variables ####

## async channel mutex equiv.
variable block

## open channels
variable channel


proc new_connection {chan addr port} {
	log info "new connection $chan from $addr:$port"
	fconfigure $chan -buffering line -translation lf
	fileevent $chan readable [list ::fastagi::new_header_line $chan]
	
	variable channel
	set channel($chan) [clock seconds]
}

proc close_chan {chan} {
	log debug "$chan: close."
	
	## remove agienv entries
	variable agienv
	foreach key [array names agienv -glob "$chan,*"] {
		unset agienv($key)
	}
	
	## cleanup block
	variable block
	if {[array get block $chan] != ""} { unset block($chan) }
	
	## close channel
	close $chan

	## remove channel from open channel list
	variable channel
	if {[array get channel $chan] != ""} { unset channel($chan) }
	
	## callback
	variable callback
	if {[array get callback closed] != ""} { $callback(closed) $chan }
}

proc new_header_line {chan} {
	variable agienv
	
	if {[eof $chan]} {
		close_chan $chan
		return
	}
	
	set headerline [gets $chan]
	
	if {[regexp {^(agi_\w+):\s+(.*)} $headerline all key val]} {
		## valid header line -> store in agienv
		log debug "$chan: ENV $key: $val"
		set agienv($chan,$key) $val
		
	} elseif {$headerline == "" } {
		## empty line -> end of header
		log debug "$chan: end of headers"
		fileevent $chan readable [list ::fastagi::new_line $chan]
		
		## callback
		variable callback
		if {[array get callback connected] != ""} { $callback(connected) $chan }
		
		## channel monitor
		start_channel_monitor $chan
		
	} else {
		## invalid header line  -> close connection
		log error "$chan: invalid header line in $chan: $headerline"
		close_chan $chan
	}
}

proc new_line {chan} {
	variable block
	set block($chan) [gets $chan]
	log debug "$chan -> $block($chan)"
	
	## update timestamp
	variable channel
	set channel($chan) [clock seconds]
}


proc send_cmd {chan cmd} {
	if {[eof $chan]} {
		close_chan $chan
		return
	}
	
	variable block
	log debug "$chan <- $cmd"

	puts $chan $cmd
	vwait ::fastagi::block($chan)

	if {[array get block $chan] == ""} { return }

	if {[regexp {^200\s+(.*)} $block($chan) all kv]} {
		return $kv
	} elseif {[regexp {^511\s} $block($chan)]} {
		## 511 Command Not Permitted on a dead channel
		close_chan $chan
		return error
	}
	
	## error state
	log error "return code != 200"
	return error
	# error "received '$block($chan)' from channel $chan"
}

proc result {result name} {
	foreach kvpair [split $result " "] {
		set kv [split $kvpair "="]
		if {[llength $kv] < 2} { return }
		if {[lindex $kv 0] == $name} { return [lindex $kv 1] }
	}
	return
}

proc result_cmd {chan cmd} {
	return [result [send_cmd $chan $cmd] result]
}

proc start_channel_monitor {chan} {
	variable monitor_timeout
	after $monitor_timeout [list ::fastagi::channel_monitor $chan]
}

proc channel_monitor {chan} {
	variable channel
	## invalid channel?
	if {[array get channel $chan] == ""} { return }

	## active channel?
	if {[expr "$channel($chan) + 10"] > [clock seconds]} { return }

	## channel status (auto-hangup for dead channels
	set status [send_cmd $chan {channel status}]
	if {$status == "error"} {
	# 	close_chan $chan
		return
	}
	
	## restart timer
	start_channel_monitor $chan
}

proc start_server {args} {
	log info "starting FastAGI server with args: $args"
	set srv [socket -server ::fastagi::new_connection {*}$args]
}


## end of namespace
}
##

