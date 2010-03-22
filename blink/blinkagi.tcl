#!/usr/bin/tclsh8.5
##
## Blinkenlights AGI interface
## 2010-03-15 -- BeF <bef@eventphone.de>
##


###############################################################################
## optparse
package require cmdline
proc parseopts {} {
	global argv0
	global argv
	global params
	set options {
		{aa.arg "127.0.0.1" "FastAGI listen address"}
		{ap.arg 1324 "FastAGI listen port (TCP)"}
		{bp.arg 1234 "BlinkenISDN listen port (UDP)"}
		{i.arg 2 "support num. ISDN channels"}
		{sp.arg "" "sound prefix"}
	}
	set usage ": $argv0 \[options]\noptions:"
	array set params [::cmdline::getoptions argv $options $usage]
}

if {[catch parseopts err]} {
	puts $err
	exit
}

###############################################################################
## init logging
package require log
namespace import ::log::log
log::lvSuppressLE critical 0
# log::lvSuppressLE debug 1

log info "init."

###############################################################################
## import libraries
source [file join [file dirname $::argv0] blisdn.tcl]
source [file join [file dirname $::argv0] fastagi.tcl]


###############################################################################
## ichannel handling (fake isdn lines)

## ichannel(x) = "free" | <agi socket>
## ichannel(x,loopaction) = "wait" | "play" | "end"
## ichannel(x,blisdnpeer) = <ip>
set maxichannels $params(i)
log debug "setting up $maxichannels isdn channels"
for {set x 1} {$x <= $maxichannels} {incr x} {
	set ichannel($x) "free"
	set ichannel($x,loopaction) "wait"
}
proc next_free_ichannel {} {
	global ichannel
	global maxichannels
	for {set x 1} {$x <= $maxichannels} {incr x} {
		if {$ichannel($x) == "free"} { return $x }
	}
	return
}
proc get_ichannel {agichan} {
	global ichannel
	foreach {k v} [array get ichannel] {
		if {$v == $agichan} { return $k }
	}
}

###############################################################################
## init fastagi
proc new_player {chan} {
	global ichannel
	log info "new player on $chan"
	set ichan [next_free_ichannel]
	if {$ichan == ""} {
		log debug "no free ichannel -> hangup"
		::fastagi::send_cmd $chan hangup
	} else {
		set ichannel($ichan) $chan
		set cid $::fastagi::agienv($chan,agi_callerid)
		set exten $::fastagi::agienv($chan,agi_extension)
		log debug "setup line $chan cid $cid exten $exten -> $chan"
		::blisdn::broadcast_cmd [list $ichan setup "$cid" "$exten"]
	}
}

proc chan_closed {chan} {
	global ichannel
	log debug "chan $chan closed."
	set ichan [get_ichannel $chan]
	
	if {$ichan == ""} {
		log debug "hmmm."
		return
	}
	
	::blisdn::broadcast_cmd [list $ichan onhook]
	set ichannel($ichan) "free"
	set ichannel($ichan,loopaction) "end"
}

set ::fastagi::callback(connected) new_player
set ::fastagi::callback(closed) chan_closed

::fastagi::start_server -myaddr $params(aa) $params(ap)

###############################################################################
## init blisdn server

proc handle_accept {peer ichan} {
	global ichannel
	log debug "handle_accept $peer $ichan $ichannel($ichan)"
	if {$ichannel($ichan) == "free"} {return}
	
	set ichannel($ichan,blisdnpeer) $peer
	set ichannel($ichan,loopaction) "wait"
	
	::fastagi::send_cmd $ichannel($ichan) "answer"
	::blisdn::send_peer_cmd $peer [list $ichan connected]
	
	digitloop $ichan
}

proc handle_hangup {peer ichan cause} {
	global ichannel
	log debug "got hangup from $peer line $ichan cause $cause"
	if {$ichannel($ichan) == "free"} {return}
	## graceful hangup, but undefined agi disconnect
	# ::fastagi::send_cmd $ichannel($ichan) "hangup"
	## dirty hangup, but fast and functional
	::fastagi::close_chan $ichannel($ichan)
}

proc handle_play {peer ichan filename} {
	global ichannel
	global params
	if {[regexp {^/} $filename] || [regexp {(\.\./|\s|")} $filename]} {
		set filename beep
	} elseif {[regexp {\.(alaw|wav|gsm|ulaw)$} $filename]} {
		set filename [file rootname $filename]
		set filename [file join $params(sp) $filename]
	}
	set ichannel($ichan,fn) $filename
	set ichannel($ichan,loopaction) play
}

proc handle_playbackground {peer ichan filename} {
	handle_play $peer $ichan $filename
}

set ::blisdn::callback(accept) handle_accept
set ::blisdn::callback(hangup) handle_hangup
set ::blisdn::callback(play) handle_play
set ::blisdn::callback(playbackground) handle_playbackground
::blisdn::start_server $params(bp)


###############################################################################
## event loop
proc fwd_dtmf {asciivalue ichan} {
	global ichannel
	if {$asciivalue != "" && $asciivalue >= 32} {
		set pressed_key [format "%c" $asciivalue]
		log debug "sending dtmf $pressed_key to $ichannel($ichan,blisdnpeer)"
		::blisdn::send_peer_cmd $ichannel($ichan,blisdnpeer) [list $ichan "dtmf" $pressed_key]
	}
}
proc digitloop {ichan} {
	global ichannel
	while true {
		if {$ichannel($ichan) == "free"} { return }
		
		switch $ichannel($ichan,loopaction) {
			wait {
				set result [::fastagi::result_cmd $ichannel($ichan) "wait for digit 2000"]
				log debug "got $result from line $ichan"
				fwd_dtmf $result $ichan
			}
			play {
				set ichannel($ichan,loopaction) wait
				set result [::fastagi::result_cmd $ichannel($ichan) "stream file \"$ichannel($ichan,fn)\" \"0123456789*#\""]
				fwd_dtmf $result $ichan
			}
			default {
				log debug "end digitloop for line $ichan"
				return
			}
		}
	}
}



log info "ready."
vwait forever
