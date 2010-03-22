##
## blinkenlights extended blinken isdn protocol server library
## 2010-03-15 - BeF <bef@eventphone.de>
##

package require log
package require udp

## start namespace
namespace eval ::blisdn {
##

namespace import ::log::log

## check for missing heartbeats (ms)
variable monitor_interval
set monitor_interval 5000

## remove peers not seen for some time (s)
variable monitor_timeout
set monitor_timeout 60

## periodically send heartbeat to all peers (ms)
variable heartbeat_interval
set heartbeat_interval 20000

variable callback
variable registry



proc send_cmd {host port data} {
	set s [udp_open]
	udp_conf $s $host $port
	puts -nonewline $s [join $data ":"]
	close $s
}

proc broadcast_cmd {data} {
	variable registry
	foreach peerkey [array names registry -glob "*,ip"] {
		set peer $registry($peerkey)
		send_cmd $peer $registry($peer,port) $data
	}
}

proc send_peer_cmd {peer data} {
	log debug "send_peer_cmd $peer $data"
	variable registry
	if {[array get registry "$peer,port"] == ""} {
		log debug "oeh."
		return
	}
	send_cmd $peer $registry($peer,port) $data
}

proc event_handler {s} {
	set pkt [read $s]
	set peer [lindex [fconfigure $s -peer] 0]
	log debug "$peer -> $pkt"

	if {[regexp {^(\d+):(.*)} $pkt all ichan cmd]} {
		## valid command -> callback
		variable callback
		set argv [split $cmd ":"]
		set argv0 [lrange $argv 0 0]
		set argvn [lrange $argv 1 end]
		if {[array get callback $argv0] != ""} {
			if {[llength [info args $callback($argv0)]] != [expr [llength $argvn] + 2]} { return }
			$callback($argv0) $peer $ichan {*}$argvn
		}
	}
}

proc start_server {port} {
	set srv [udp_open $port]
	fconfigure $srv -buffering none -translation binary
	fileevent $srv readable [list ::blisdn::event_handler $srv]
	log info "blisdn listening on udp port: [fconfigure $srv -myport]"

	## default callbacks
	variable callback
	set callback(heartbeat) ::blisdn::handle_heartbeat
	set callback(register) ::blisdn::handle_register
}

proc remove_peer {peer} {
	variable registry
	foreach key [array names registry -glob "$peer,*"] {
		unset registry($key)
	}
}


proc start_monitor {peer} {
	variable monitor_interval
	after $monitor_interval [list ::blisdn::monitor $peer]
}

proc monitor {peer} {
	variable registry
	variable monitor_timeout
	if {[array get registry "$peer,port"] == ""} { return }
	if {[expr "[clock seconds] - $registry($peer,last_received_heartbeat)"] > $monitor_timeout} {
		log info "lost contact to peer $peer for at least $monitor_timeout seconds --> delete."
		remove_peer $peer
		return
	}
	start_monitor $peer
}

proc start_heartbeat {peer} {
	variable heartbeat_interval
	after $heartbeat_interval [list ::blisdn::heartbeat $peer]
}

proc heartbeat {peer} {
	variable registry
	if {[array get registry "$peer,port"] == ""} { return }
	log debug "sendig heartbeat to $peer:$registry($peer,port)"
	send_cmd $peer $registry($peer,port) {0 heartbeat}
	start_heartbeat $peer
}

## default handlers

proc handle_heartbeat {peer ichan} {
	log debug "heartbeat from $peer $ichan"
	variable registry
	if {[array get registry "$peer,port"] == ""} {
		log warning "unknown peer $peer. go away."
		return
	}
	set registry($peer,last_received_heartbeat) [clock seconds]
}

proc handle_register {peer ichan port} {
	if {![regexp {^\d+$} $port]} { return }
	log debug "register from $peer:$port"
	variable registry
	if {[array get registry "$peer,port"] != ""} {
		## already registered
		return
	}
	
	set registry($peer,port) $port
	set registry($peer,ip) $peer
	set registry($peer,last_received_heartbeat) [clock seconds]
	start_monitor $peer
	heartbeat $peer
}

## end namespace
}
##