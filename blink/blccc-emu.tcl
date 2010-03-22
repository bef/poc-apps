#!/usr/bin/tclsh8.5
##
## blccc emulator / isdn handling
## 2010-03-15 -- BeF <bef@eventphone.de>
##

set serverhost 127.0.0.1
set serverport 1234
set localport 12345

set last_received_hb 0

source blisdn.tcl

## start client
set srv [udp_open $localport]
fconfigure $srv -buffering none -translation binary
fileevent $srv readable [list ::blisdn::event_handler $srv]

## init heartbeat
proc start_hb {} {
	after 5000 ::hb
}

proc hb {} {
	global last_received_hb
	global serverhost
	global serverport
	global localport
	if {[expr "[clock seconds] - $last_received_hb"] > 30} {
		## no heartbeat from server -> send register
		puts "sending register"
		::blisdn::send_cmd $serverhost $serverport [list 0 register $localport]
	} else {
		## normal heartbeat
		puts "sending heartbeat to $serverhost:$serverport"
		::blisdn::send_cmd $serverhost $serverport [list 0 heartbeat]
	}
	start_hb
}

proc handle_hb {peer ichan} {
	global last_received_hb
	puts "getting heartbeat from $peer"
	set last_received_hb [clock seconds]
}
set ::blisdn::callback(heartbeat) ::handle_hb

## other callbacks
proc handle_setup {peer ichan cid exten} {
	puts "setup $peer $ichan $cid $exten"
}
proc handle_dtmf {peer ichan digit} {
	puts "dtmf $peer $ichan $digit"
}
proc handle_connected {peer ichan} {
	puts "connected $peer $ichan"
}
proc handle_onhook {peer ichan} {
	puts "onhook $peer $ichan"
}
set ::blisdn::callback(setup) ::handle_setup
set ::blisdn::callback(dtmf) ::handle_dtmf
set ::blisdn::callback(connected) ::handle_connected
set ::blisdn::callback(onhook) ::handle_onhook


## cli
proc handle_stdin {} {
	global serverhost
	global serverport
	if {[eof stdin]} {
		fileevent stdin readable {}
		return
	}
	set line [gets stdin]
	switch $line {
		a1 { ::blisdn::send_cmd $serverhost $serverport {1 accept}}
		a2 { ::blisdn::send_cmd $serverhost $serverport {2 accept}}
		h1 { ::blisdn::send_cmd $serverhost $serverport {1 hangup 16}}
		h2 { ::blisdn::send_cmd $serverhost $serverport {2 hangup 16}}
		default {
			if {[regexp {^p(\d)(.*)$} $line all i fn]} {
				::blisdn::send_cmd $serverhost $serverport [list $i play $fn]
			} elseif {[regexp {^b(\d)(.*)$} $line all i fn]} {
				::blisdn::send_cmd $serverhost $serverport [list $i playbackground $fn]
			} else { puts "try one of: a1 a2 h1 h2 p1file p2file b1file b2file" }
		}
	}
}
fconfigure stdin -buffering line -translation lf
fileevent stdin readable handle_stdin

## start
puts "ready."
hb

vwait forever
