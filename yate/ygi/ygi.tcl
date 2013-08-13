#
# Tcl message handler library for Yate extmodule scripting
#
# Author: Ben Fuhrmannek <bef@eventphone.de>
# Date: 2012-09-21
# 
# Copyright (c) 2012, Ben Fuhrmannek
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the author nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package require Tcl 8.5
package provide ygi 0.1


##
## ::ygi NAMESPACE
##

namespace eval ::ygi {
	## PUBLIC VARIABLES

	## execute script or function just before program terminates
	##   note: ::ygi::log may not be available at this point. use ::ygi::forcelog instead
	variable onexit {}

	## string to prepend to log messages
	variable logprefix "?: "
	if {[info exists ::argv0]} { set logprefix "\[[file tail $::argv0]\]: " }

	## file descriptors from/to Yate
	##   note: These should be set for TCP connections accordingly, e.g. ::ygi::start $in $out
	variable fdin stdin
	variable fdout stdout

	## contains last ACK message
	##   note: This variable will change with every received message.
	##         If needed, copy contents for safekeeping directly after message call to engine.
	variable lastresult
	array set lastresult {}

	## debug output
	variable debug false

	## IVR environment - key/value pairs of call.execute message
	variable env
	array set env {}

	## last received DTMF digit (or letter)
	variable lastdigit ""

	## PRIVATE VARIABLES - best not to disturb them
	variable uniqueid_counter 0
	variable messagehandler
	array set messagehandler {}
	variable watchhandler
	array set watchhandler {}
	variable dtmfbuffer {}
	variable config
	array set config {sndpath {} sndformat {}}
}

##
## PRIVATE FUNCTIONS
##

proc ::ygi::_input {} {
	variable onexit
	if {[gets $::ygi::fdin line] < 0} {
		## end of input
		if {$::ygi::debug} { forcelog "EXIT" }
		fileevent $::ygi::fdin readable ""
		_exit
	}

	## process $line
	if {$::ygi::debug} {
		log "DEBUG: $line"
	}

	if {[regexp -- {^%%>message:(.*?):(.*?):(.*?):(.*?)(?::(.*?))?$} $line all id time name retvalue kv]} {
		## message process request
		set d_name [ydecode ${name}]
		if {[info exists ::ygi::messagehandler($name)]} {
			set d_id [ydecode $id]
			set d_time [ydecode $time]
			set d_retvalue [ydecode $retvalue]
			set d_kv [_split_kv [ydecodelist [split $kv ":"]]]
			set result [$::ygi::messagehandler($name) {*}[list $d_id $d_time $d_name $d_retvalue $d_kv]]
			if {$result eq "" || $result eq "false"} {
				_raw_write "%%<message:$id:false:$name:$retvalue:$kv"
			} elseif {$result eq "true"} {
				_raw_write "%%<message:$id:true:$name:$retvalue:$kv"
			} else {
				lassign $result processed name retvalue kv
				if {$processed == 1 || $processed eq "true"} {
					set processed "true"
				} else {
					set processed "false"
				}
				_write "%%<message" $d_id $processed $name $retvalue {*}[_join_kv $kv]
			}
		} else {
			_raw_write "%%<message:$id:false:${_name}:$retvalue:$kv"
		}
	} elseif {[regexp -- {^%%<message:(.*?):(.*?):(.*?):(.*?)(?::(.*?))?$} $line all id processed name retvalue kv]} {
		## message ACK from engine
		set id [ydecode $id]
		set processed [ydecode $processed]
		set name [ydecode $name]
		set retvalue [ydecode $retvalue]
		set kv [_split_kv [ydecodelist [split $kv ":"]]]

		if {[regexp -- "^ygi\\.[pid]\\.\\d+\$" $id]} {
			## correct ygi message id
			array unset ::ygi::lastresult
			array set ::ygi::lastresult [list id $id processed $processed name $name retvalue $retvalue kv $kv]
			set "::ygi::$id" 1
		} elseif {[info exists ::ygi::watchhandler($name)]} {
			## dispatch message to watchhandler (unless previously matched as own ACK message)
			$::ygi::watchhandler($name) $id $processed $name $retvalue $kv
		} elseif {[info exists ::ygi::watchhandler()]} {
			## wildcard message handler?
			$::ygi::watchhandler() $id $processed $name $retvalue $kv
		} else {
			## unknown message id
			log "WARNING: got ACK message with unknown id: $line"
		}
	} elseif {[regexp -- {^%%<((?:un)?install):(.*?):(.*?):(.*?)$} $line all type priority name success]} {
		## (un)install ACK from engine
		set name [ydecode $name]
		set "::ygi::ygi.$type.$name" [expr {$success eq "true"}]
	} elseif {[regexp -- {^%%<((?:un)?watch):(.*?):(.*?)$} $line all type name success]} {
		## (un)watch ACK from engine
		set name [ydecode $name]
		set "::ygi::ygi.$type.$name" [expr {$success eq "true"}]
	} elseif {[regexp -- {^%%<setlocal:(.*?):(.*?):(.*?)$} $line all name value success]} {
		## setlocal ACK from engine
		set name [ydecode $name]
		set "::ygi::ygi.setlocal.$name" [ydecode $value]
	} elseif {[regexp -- {^Error in:(.*)$} $line all msg]} {
		## syntax error
		forcelog "SYNTAX ERROR: $msg"
	} elseif {[regexp -- {^%%<quit$} $line]} {
		## quit ACK. do nothing.
	} else {
		log "unknown msg: $line"
	}
}

proc ::ygi::_exit {{code 0} {message ""} {logfn log}} {
	if {[catch {eval $::ygi::onexit} err]} {
		$logfn "ERROR in onexit script: $err"
	}
	if {$message ne ""} {
		$logfn $message
	}
	exit $code
}

## handle first call.execute message to script
##   and unregister handler
proc ::ygi::_ivr_call_execute_handler {id time name retvalue kv} {
	unset ::ygi::messagehandler(call.execute)
	array unset ::ygi::env
	array set ::ygi::env $kv
	return true
}

## default notify handler
proc ::ygi::_notify_handler {id time name retvalue kv} {
	set ::ygi::_notify 1
	return true
}

## default dtmf handler
proc ::ygi::_dtmf_handler {id time name retvalue kv} {
	variable lastdigit
	variable dtmfbuffer
	set digit [dict get $kv text]
	if {$::ygi::debug} {::ygi::log "DTMF $digit"}
	lappend dtmfbuffer $digit
	set lastdigit $digit
	heartbeat
	return true
}

## convert {a=b c=d ...} --> {a b c d ...}
proc ::ygi::_split_kv {kv} {
	set out {}
	foreach el $kv {
		if {[regexp -- {^(.*?)(?:=(.*))?$} $el all k v]} {
			lappend out $k $v
		}
	}
	return $out
}

## convert {a b c d ...} --> {a=b c=d ...}
proc ::ygi::_join_kv {kv} {
	set out {}
	foreach {k v} $kv {
		lappend out "$k=$v"
	}
	return $out
}

proc ::ygi::_raw_write {s} {
	puts $::ygi::fdout $s
}

proc ::ygi::_write {keyword args} {
	if {[string range $keyword 0 1] ne "%%"} {
		set keyword "%%>$keyword"
	}
	set out [list $keyword {*}[yencodelist $args]]
	_raw_write [join $out ":"]
}

proc ::ygi::yencode {s} {
	set ret {}
	foreach c [split $s {}] {
		set ord [scan "$c" "%c"]
		if {$ord < 32 || $c eq ":"} {
			lappend ret [format "%%%c" [expr {$ord + 64}]]
		} elseif {$c eq "%"} {
			lappend ret "%%"
		} else {
			lappend ret $c
		}
	}
	return [join $ret ""]
}

proc ::ygi::ydecode {s} {
	set ret {}
	set len [string length $s]
	for {set i 0} {$i < $len} {incr i} {
		set c [string index $s $i]
		if {$c ne "%"} {
			lappend ret $c
			continue
		}
		incr i
		if {$i == $len} {
			## last character is "%"? -> ignore
			continue
		}
		set c [string index $s $i]
		set ord [scan "$c" "%c"]
		if {$ord < 64} {
			## invalid character -> ignore
			continue
		}
		lappend ret [format "%c" [expr {$ord - 64}]]


	}
	return [join $ret ""]
}

proc ::ygi::yencodelist {l} {
	set out {}
	foreach element $l {
		lappend out [yencode $element]
	}
	return $out
}
proc ::ygi::ydecodelist {l} {
	set out {}
	foreach element $l {
		lappend out [ydecode $element]
	}
	return $out
}

proc ::ygi::uniqueid {} {
	variable uniqueid_counter
	return "ygi.[pid].[incr uniqueid_counter]"
}

proc ::ygi::_find_soundfile {fn} {
	variable config
	if {[string index $fn 0] eq "/" && [file exists $fn]} { return $fn }
	if {[llength $config(sndpath)] == 0} {
		set config(sndpath) [split [setlocal config.ygi.sndpath] ","]
		set config(sndformats) [split [setlocal config.ygi.sndformats] ","]
	}
	set sndpath $config(sndpath)
	if {[string index $fn 0] eq "/"} {
		set sndpath [linsert $sndpath 0 "/"]
	}
	foreach fmt [list "" {*}$config(sndformats)] {
		if {$fmt ne "" && [string index $fmt 0] ne "."} {
			set fmt ".$fmt"
		}
		foreach dir $sndpath {
			set ret [file join $dir "$fn$fmt"]
			if {[file exists $ret]} {
				return $ret
			}
		}
	}
	## not found. return $fn anyway and let Yate deal with it.
	return $fn
}

## helper function to create variables from 'args' with predefined default values
proc dict_args {default_args} {
	upvar 1 args args
	set args [dict merge $default_args $args]
	foreach k [dict keys $default_args] {
		uplevel 1 [list set $k [dict get $args $k]]
	}
}

##
## PUBLIC API
##

## log <msg> to yate console
proc ::ygi::log {msg} {
	variable logprefix
	if {[catch {
		_raw_write "%%>output:$logprefix$msg"
	}]} {
		forcelog $msg
	}
}

## log <msg> to stderr
proc ::ygi::forcelog {msg} {
	puts stderr "${::ygi::logprefix}$msg"
}


## start processing messages
## this should be the first function to call in every global script
proc ::ygi::start {{fdin stdin} {fdout stdout}} {
	set ::ygi::fdin $fdin
	set ::ygi::fdout $fdout
	fconfigure $fdin -buffering line
	fconfigure $fdout -buffering line
	fileevent $fdin readable ::ygi::_input
}

## shortcut for TCP start
proc ::ygi::start_tcp {ip port} {
	set fd [socket 127.0.0.1 5039]
	start $fd $fd
	return $fd
}

## start processing messages
##   and install call.execute handler for first call to the script
##   and install chan.notify and chan.dtmf default handlers
## this should be the first function to call call in every called script
proc ::ygi::start_ivr {} {
	set ::ygi::messagehandler(call.execute) ::ygi::_ivr_call_execute_handler
	start
	vwait ::ygi::env
	install "chan.notify" ::ygi::_notify_handler 100 targetid $::ygi::env(id)
	install "chan.dtmf" ::ygi::_dtmf_handler 100 id $::ygi::env(id)
}

## enter event loop
proc ::ygi::loop_forever {} {
	vwait ::forever
}

## set role - for TCP connections only
## this should be called after ::ygi::start for TCP based scripts
## <role> - role of this connection: global, channel, play, record, playrec
## <id> - channel id to connect this socket to
## <type> - type of data channel, assuming audio if missing
proc ::ygi::connect {role {id ""} {type ""}} {
	set out [list connect $role]
	if {$id ne ""} {
		lappend out $id
		if {$type ne ""} {
			lappend out $type
		}
	}
	_write {*}$out
}

## send message and wait for result
proc ::ygi::message {name {retvalue ""} {kv {}}} {
	set id [uniqueid]
	set time [clock seconds]
	_write message $id $time $name $retvalue {*}[_join_kv $kv]
	vwait "::ygi::$id"
	unset "::ygi::$id"
	return $::ygi::lastresult(processed)
}

## shortcut for ::ygi::message
## e.g. 'message foo.test "" {a b c d}' --> 'msg foo.test a b c d'
proc ::ygi::msg {name args} {
	return [message $name "" $args]
}

## install message handler
proc ::ygi::install {name handler {priority ""} {filtername ""} {filtervalue ""}} {
	set out [list install $priority $name]
	if {$filtername ne ""} {
		lappend out $filtername
		if {$filtervalue ne ""} {
			lappend out $filtervalue
		}
	}
	_write {*}$out
	vwait "::ygi::ygi.install.$name"
	set success [set "::ygi::ygi.install.$name"]
	unset "::ygi::ygi.install.$name"
	if {$success} {
		set ::ygi::messagehandler($name) $handler
	}
	return $success
}

## uninstall message handler
proc ::ygi::uninstall {name} {
	_write uninstall $name
	vwait "::ygi::ygi.uninstall.$name"
	set success [set "::ygi::ygi.uninstall.$name"]
	unset "::ygi::ygi.uninstall.$name"
	if {[info exists ::ygi::messagehandler($name)]} {
		unset ::ygi::messagehandler($name)
	}
	return $success
}

## install post-dispatching watcher
proc ::ygi::watch {name handler} {
	_write watch $name
	vwait "::ygi::ygi.watch.$name"
	set success [set "::ygi::ygi.watch.$name"]
	unset "::ygi::ygi.watch.$name"
	if {$success} {
		set ::ygi::watchhandler($name) $handler
	}
	return $success
}

## uninstall post-dispatching watcher
proc ::ygi::unwatch {name} {
	_write unwatch $name
	vwait "::ygi::ygi.watch.$name"
	set success [set "::ygi::ygi.watch.$name"]
	unset "::ygi::ygi.watch.$name"
	if {[info exists ::ygi::watchhandler($name)]} {
		unset ::ygi::watchhandler($name)
	}
	return $success
}

## change/query local parameter
## Currently supported parameters (as of yate 4.2.0):
## id (string) - Identifier of the associated channel, if any
## disconnected (bool) - Enable or disable sending "chan.disconnected" messages
## trackparam (string) - Set the message handler tracking name, cannot be made empty
## reason (string) - Set the disconnect reason that gets received by the peer channel
## timeout (int) - Timeout in milliseconds for answering to messages
## timebomb (bool) - Terminate this module instance if a timeout occured
## setdata (bool) - Attach channel pointer as user data to generated messages
## reenter (bool) - If this module is allowed to handle messages generated by itself
## selfwatch (bool) - If this module is allowed to watch messages generated by itself
## restart (bool) - Restart this global module if it terminates unexpectedly. Must be turned off to allow normal termination
## Engine read-only run parameters:
## engine.version (string,readonly) - Version of the engine, like "2.0.1"
## engine.release (string,readonly) - Release type and number, like "beta2"
## engine.nodename (string,readonly) - Server's node name as known by the engine
## engine.runid (int,readonly) - Engine's run identifier
## engine.configname (string,readonly) - Name of the master configuration
## engine.sharedpath (string,readonly) - Path to the shared directory
## engine.configpath (string,readonly) - Path to the program config files directory
## engine.cfgsuffix (string,readonly) - Suffix of the config files names, normally ".conf"
## engine.modulepath (string,readonly) - Path to the main modules directory
## engine.modsuffix (string,readonly) - Suffix of the loadable modules, normally ".yate"
## engine.logfile (string,readonly) - Name of the log file if in use, empty if not logging
## engine.clientmode (bool,readonly) - Check if running as a client
## engine.supervised (bool,readonly) - Check if running under supervisor 
## engine.maxworkers (int,readonly) - Maximum number of message worker threads
## Engine configuration file parameters:
## config.<section>.<key> (readonly) - Content of key= in [section] of main config file (yate.conf, yate-qt4.conf)
proc ::ygi::setlocal {name {value ""}} {
	_write setlocal $name $value
	vwait "::ygi::ygi.setlocal.$name"
	set retval [set "::ygi::ygi.setlocal.$name"]
	unset "::ygi::ygi.setlocal.$name"
	return $retval
}

## event-safe sleep function
## as suggested by http://wiki.tcl.tk/933
proc ::ygi::sleep {ms} {
	set varname "::__sleep_tmp_[clock clicks]"
	after $ms set $varname 1
	vwait $varname
	unset $varname
}

## print IVR environment to Yate console for debugging
proc ::ygi::print_env {} {
	log "--------------->"
	foreach {k v} [array get ::ygi::env] {
		log [format "| %18s %s" $k $v]
	}
}

## wait for chan.notify, e.g. for EOF on wave/play
proc ::ygi::waitfornotify {} {
	vwait ::ygi::_notify
}

## play <fn>
## <args> are parameters to chan.attach, e.g. autorepeat true
## <fn> filename. either absolute, e.g. /sounds/beep.slin
##   or without extension, e.g. /sounds/beep
##   or relative to search path, e.g. beep
##   or relative, but with extension, e.g. beep.slin
##
## example yate.conf section:
## [ygi]
## sndpath=/usr/local/share/yate/sounds,/usr/local/share/yate/sounds/ast,/var/lib/asterisk/sounds/en
## sndformats=slin,gsm
##
proc ::ygi::play {fn args} {
	msg chan.attach source "wave/play/[_find_soundfile $fn]" {*}$args
}

## play <fn> and wait
## <args> are parameters to chan.attach, e.g. autorepeat true
## note: requires default chan.notify handler
proc ::ygi::play_wait {fn args} {
	play $fn notify $::ygi::env(id) {*}$args
	waitfornotify
}

## play silence
proc ::ygi::silence {} {
	msg chan.attach source tone/silence
}

## discard buffered dtmf digits
proc ::ygi::clear_dtmfbuffer {} {
	set ::ygi::dtmfbuffer {}
}

## get dtmf digit
## <usebuffer> - use buffered digits if available
## <timeout> - return "" after timeout in ms - 0 for no timeout
proc ::ygi::getdigit {{usebuffer true} {timeout 0}} {
	variable dtmfbuffer
	if {$usebuffer} {
		if {[llength $dtmfbuffer] > 0} {
			set digit [lindex $dtmfbuffer 0]
			set dtmfbuffer [lreplace $dtmfbuffer 0 0]
			return $digit
		}
	} else {
	}
	if {$timeout > 0} {
		set afterid [after $timeout {set ::ygi::lastdigit ""}]
	}
	vwait ::ygi::lastdigit
	set dtmfbuffer {}
	if {$timeout > 0} {
		after cancel $afterid
	}
	return $::ygi::lastdigit
}

## get one or more digits
## <maxdigits> - stop collecting digits after so many digits
## <digittimeout> - stop collecting digits after inactivity in ms - 0 to disable
## <silence> - play silence after first entered digit
## <enddigit> - stop after getting this digit
proc ::ygi::getdigits {args} {
	dict_args {	maxdigits 10
			digittimeout 6000
			silence true
			enddigit "#"}

	set digits {}
	for {set i 0} {$i < $maxdigits} {incr i} {
		set digit [getdigit true $digittimeout]
		if {$digit eq ""} {
			return $digits
		}
		if {$i == 0 && $silence} {
			silence
		}
		if {$digit eq $enddigit} {
			return $digits
		}
		lappend digits $digit
	}
	return $digits
}

## terminate script after <timeout> s
proc ::ygi::script_timeout {{timeout 1200}} {
	after [expr {$timeout * 1000}] {
		::ygi::_exit 1 "NOTICE: script timeout."
	}
}

## terminate script after <timeout> s of inactivity
## check every <interval> s
proc ::ygi::idle_timeout {{timeout 600} {interval 60}} {
	if {![info exists ::ygi::idlesince]} {
		heartbeat
	} else {
		if {$::ygi::idlesince + $timeout < [clock seconds]} {
			::ygi::_exit 1 "NOTICE: idle timeout after $timeout seconds."
		}
	}
	after [expr {$interval * 1000}] ::ygi::idle_timeout $timeout $interval
}

## heartbeat for the idle timeout
## this function should be called from the DTMF handler, or any other user interaction event handler
proc ::ygi::heartbeat {} {
	set ::ygi::idlesince [clock seconds]
}

proc ::ygi::quit {} {
	_raw_write "%%>quit"
}

## ask for password
## return <input> or false
## example: ask_password password 1234 exit_on_failure true
proc ::ygi::ask_password {args} {
	dict_args {	password 0000
			passwords {}
			retries 3
			enter_pw_sound "enter-password"
			invalid_sound "ybeeperr"
			getdigits_args {}
			exit_on_failure false
			exit_sound "goodbye"}

	set loggedin false
	for {set i 0} {$i < $retries} {incr i} {
		play $enter_pw_sound
		set input [getdigits {*}$getdigits_args]
		set input [join $input ""]

		## check password
		if {[llength $passwords] > 0} {
			## allow multiple $passwords
			if {[lsearch -exact $passwords $input] != -1} {
				set loggedin true
				break
			}
		} elseif {$input eq $password} {
			set loggedin true
			break
		}

		if {[info exists ::ygi::env(caller)]} {
			log "${::ygi::env(called)}: INVALID PASSWORD FROM CALLER ${::ygi::env(caller)}"
		}
		play_wait $invalid_sound
	}
	if {!$loggedin && $exit_on_failure} {
		play_wait $exit_sound
		_exit 1
	}
	if {$loggedin} {
		return $input
	}
	return $loggedin
}

## return ::ygi::env dict with given keys only
proc ::ygi::filter_env {args} {
	set params {}
	foreach p $args {
		if {[info exists ::ygi::env($p)]} {
			lappend params $p $::ygi::env($p)
		}
	}
	return $params
}

