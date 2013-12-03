#!/usr/bin/env tclsh8.6
#
# Yate Status Monitor
# - display active channels and important messages in real-time
#   with a curses-like interface -
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2013-12-03
#
# Copyright (c) 2013, Ben Fuhrmannek
# All rights reserved.
#

set ysm_version "0.1a1"

set auto_path [linsert $auto_path 0 [file dirname [info script]]]
package require ygi
package require cmdline
package require term::ansi::code
package require term::ansi::code::ctrl


##############################################################################
## cmd opts
set options {
	{H.arg		"localhost"	"connect to host"}
	{P.arg		"5039"		"connect to port"}
	{cd.arg		"6000"		"channel remove delay"}
	{fd.arg		"8000"		"flashmessage delay"}
	{yd						"show debug msgs in yate"}
}

set usage "$::argv0 <options>..."
if {[catch {
	array set cfg [::cmdline::getoptions argv $options $usage]
} e]} {
	puts stderr $e
	exit 1
}

proc dg {k {default "?"}} {
	## special dict get for $kv with default value
	upvar 1 kv d
	if {[dict exists $d $k]} {
		return [dict get $d $k]
	}
	return $default
}

##############################################################################
## UI
namespace eval ::ystatusui {
	::term::ansi::code::ctrl::import [namespace current]::a *
	
	variable chandata {}
	variable flashmsgs {}
	variable ui_last_updated 0
	
	proc init {} {
		puts -nonewline "[a::init][a::title {Yate Status Monitor}]"
		flush stdout
		update_display
	}

	proc cleanup {} {puts "[a::clear][a::rd]bye."}

	proc update_display {} {
		variable ui_last_updated
		variable chandata
		variable flashmsgs

		## big todo: only update, if data changed
		set now [clock microseconds]
		if {$now  < $ui_last_updated + 500} {return}
		set ui_last_updated $now

		puts -nonewline "[a::sda_bgblack][a::clear]"
		puts -nonewline "[a::sda_fgyellow][a::sda_bgblue][a::sda_bold]"
		puts "Yate Status Monitor v$::ysm_version - (c) 2013, BeF <bef@eventphone.de>[a::eeol]"
		puts -nonewline "[a::sda_nobold][a::sda_bgblack][a::sda_fgwhite]"
		
		if {$chandata eq ""} {
			puts "[a::sda_fgcyan]no channel active[a::sda_fgwhite]"
		}
		dict for {id kv} $chandata {
			## -[in]-> sip/37 id sip/37 module sip status incoming address 172.16.229.1:62258 billid 1385551575-19 answered false direction incoming callid sip/1331328408@devvm/176417463/ caller 3333 called 3333
			switch [dg direction] {
				incoming {set inout "-\[in\]->"}
				outgoing {set inout "<-\[out\]"}
				default {set inout "<-\[?\]->"}
			}

			set ystatus [dg ysm_status]
			if {$ystatus eq "hungup" && [dg status] ne "answered"} {set ystatus [dg status]}
			switch $ystatus {
				rejected {puts -nonewline "[a::sda_fgyellow][a::sda_bgred]"}
				ringing {puts -nonewline "[a::sda_fgyellow][a::sda_bgblack]"}
				answered {puts -nonewline "[a::sda_fggreen][a::sda_bgblack]"}
				hungup {puts -nonewline "[a::sda_fgwhite][a::sda_bgblue]"}
				default {puts -nonewline "[a::sda_fgwhite][a::sda_bgblack]"}
			}
			
			set status $ystatus
			if {[dg reason ""] ne ""} {append status ": [dg reason]"}
			if {[dg cause_sip ""] ne ""} {append status " SIP [dg cause_sip]/[dg reason_sip]"}
			append status [a::eeol]
			puts [format "%-10s %s %10s -> %-10s | %s %s" $id $inout [dg caller] [dg called] [dg address] $status]
			# puts $kv
		}
		
		## show flash messages
		dict for {id data} $flashmsgs {
			lassign $data level msg
			switch $level {
				info {puts -nonewline "[a::sda_fgyellow][a::sda_bgblack]"}
				notice {puts -nonewline "[a::sda_fgblack][a::sda_bgcyan]"}
				warning {puts -nonewline "[a::sda_fgblack][a::sda_bgred]"}
				default {puts -nonewline "[a::sda_fgcyan][a::sda_bgblack]"}
			}
			puts "\[$level\] $msg"
		}
		
		flush stdout
	}
	
	proc update_chan {id kv} {
		variable chandata
		if {![dict exists $chandata $id]} {
			dict set chandata $id $kv
		} else {
			dict update chandata $id v {
				set v [dict merge $v $kv]
			}
		}
		update_display
	}
	
	proc remove_chan {id} {
		after $::cfg(cd) "[namespace current]::_remove_chan $id"
	}
	proc _remove_chan {id} {
		variable chandata
		dict unset chandata $id
		update_display
	}
	
	proc flashmessage {level message} {
		variable flashmsgs
		set id [clock microseconds]
		dict set flashmsgs $id [list $level $message]
		after $::cfg(fd) "[namespace current]::_remove_flashmsg $id"
		update_display
	}
	
	proc _remove_flashmsg {id} {
		variable flashmsgs
		dict unset flashmsgs $id
		update_display
	}
}


##############################################################################
## watch handlers

proc chan_update_handler {_id processed name retvalue kv} {
	dict set kv ysm_status [dg status ""]
	::ystatusui::update_chan [dict get $kv id] $kv
}

proc chan_hangup_handler {_id processed name retvalue kv} {
	dict set kv ysm_status hungup
	::ystatusui::update_chan [dict get $kv id] $kv
	::ystatusui::remove_chan [dict get $kv id]
}

proc chan_disconnected_handler {_id processed name retvalue kv} {
	dict set kv ysm_status disconnected
	::ystatusui::update_chan [dict get $kv id] $kv
	::ystatusui::remove_chan [dict get $kv id]
}
proc engine_timer_handler {_id processed name retvalue kv} {
	::ystatusui::update_display
	# ::ystatusui::flashmessage info "tick [dg nodename] [dg time]"
}

proc user_auth_handler {_id processed name retvalue kv} {
	# [warning] false protocol sip username 200 realm devvm nonce 1ec87b5b3adc9e645f194b785ca18353.1385828277 response fd559c7c7a642a6ccf5c7557c2cf697e method REGISTER uri sip:devvm ip_host 172.16.229.1 ip_port 5060 ip_transport UDP address 172.16.229.1:5060 newcall false domain devvm device YATE/5.0.0 number 200 expires 600 handlers monitoring:1,regexroute:40,register:50,regfile:100
	if {$processed eq "false" && [dg response ""] ne ""} {
		::ystatusui::flashmessage warning "auth failed: [dg username]@[dg realm] / [dg address] / [dg device]"
	}
}

proc user_register_handler {_id processed name retvalue kv} {
	# [info] true number 3333 sip_uri sip:devvm sip_callid 187312661@devvm expires 600 username 3333 realm devvm ip_transport TLS newcall false domain devvm device YATE/5.0.0 driver sip data sip/sip:3333@172.16.229.1:63934 ip_host 172.16.229.1 ip_port 63934 sip_to sip:3333@172.16.229.1:63934 connection_id tls:172.16.229.129:5061-172.16.229.1:63934 connection_reliable true route_params oconnection_id oconnection_id tls:172.16.229.129:5061-172.16.229.1:63934 sip_contact <sip:3333@172.16.229.1:63934> sip_expires 600 sip_user-agent YATE/5.0.0 sip_allow {ACK, INVITE, BYE, CANCEL, OPTIONS, INFO} handlers monitoring:1,register:50
	::ystatusui::flashmessage info "registered user [dg username] [dg data] / [dg device]"
}

proc user_unregister_handler {_id processed name retvalue kv} {
	# [info] false number 3333 sip_uri sip:devvm sip_callid 187312661@devvm expires 0 username 3333 realm devvm ip_transport TLS newcall false domain devvm device YATE/5.0.0 driver sip data sip/sip:3333@172.16.229.1:63934 ip_host 172.16.229.1 ip_port 63934 sip_contact <sip:3333@172.16.229.1:63934> sip_expires 0 sip_user-agent YATE/5.0.0 sip_allow {ACK, INVITE, BYE, CANCEL, OPTIONS, INFO} handlers register:50,regfile:100
	if {[dg username ""] ne ""} {
		::ystatusui::flashmessage info "unregistered user [dg username] [dg data] / [dg device]"
	}
}



##############################################################################
## start.
set ::ygi::onexit {
	## cleanup
	::ystatusui::cleanup
}
::ystatusui::init

::ygi::start_tcp $cfg(H) $cfg(P)
set ::ygi::debug $cfg(yd)

::ygi::connect global
::ygi::log "start for $::env(USER)"

::ygi::watch chan.startup chan_update_handler
::ygi::watch chan.hangup chan_hangup_handler
::ygi::watch call.ringing chan_update_handler
::ygi::watch call.answered chan_update_handler
::ygi::watch chan.disconnected chan_disconnected_handler
::ygi::watch call.update chan_update_handler
::ygi::watch user.auth user_auth_handler
::ygi::watch user.register user_register_handler
::ygi::watch user.unregister user_unregister_handler
::ygi::watch engine.timer engine_timer_handler

::ygi::loop_forever

