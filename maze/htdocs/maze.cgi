#!/usr/bin/env tclsh

package require ncgi

ncgi::parse
source /var/lib/asterisk/agi-bin/mazeconfig.tcl
source /var/lib/asterisk/agi-bin/mazecode.tcl
source /var/lib/asterisk/agi-bin/mazegen.tcl

## PARAMS
## cid
set cid [ncgi::value cid 0]
set cid [normalize_cid $cid]
#regsub -all {[^\d]} $cid {} cid
#if {[string length $cid] > 8} { set cid [string range $cid 0 7] }
## level
set level [ncgi::value level 0]
regsub -all {[^\d]} $level {} level
if {[expr {![string is integer $level] || $level < 0 || $level > $::maxlevel}]} { set level 0 }
## png?
set png [ncgi::value png 0]

## gen maze
if {[expr {$level != "" && $cid != ""}]} {
	gen_maze $level $cid
	set alt [join [show_level 0] "\n"]
}


if {$png == "1"} {
	if {[catch {
		package require Gdtclft
		#set sc [expr {40 - int(($level+1)/4)}]
		set sc 33
		set linewidth 3
		set gd [gd create [expr {$sc * $::sz(x) + $linewidth+1}] [expr {$sc * $::sz(y) + $linewidth+1}]]
		set white [gd color new $gd 255 255 255]
		set black [gd color new $gd 0 0 0]
		set red [gd color new $gd 255 0 0]

		## paint arrows
		foreach {cellx celly} [list 0 [regsub {0,(\d+),0} $::sz(start) {\1}] [expr {$::sz(x)-1}] [expr {[regsub "[expr {$::sz(x)-1}],(\\d+),0" $::sz(end) {\1}]}]] {
			## coords
			set x1 6; set y1 8; set x2 16; set y2 12; set x3 22; set y3 14; set y4 17; set y5 20
			## scale
			foreach var {x1 x2 x3 y1 y2 y3 y4 y5} { set $var [expr {int([set $var] * ($sc-$linewidth)/29)}] }
			## offset
			foreach var {x1 x2 x3} { set $var [expr {[set $var] + $sc * $cellx + $linewidth}] }
			foreach var {y1 y2 y3 y4 y5} { set $var [expr {[set $var] + $sc * $celly + $linewidth}] }
			## paint
			gd fillpolygon $gd $red $x1 $y2 $x2 $y2 $x2 $y1 $x3 $y3 $x2 $y5 $x2 $y4 $x1 $y4
			
		}
		
		## paint walls
		for {set y 0} {$y < $::sz(y)} {incr y} {
			for {set x 0} {$x < $::sz(x)} {incr x} {
				set m $::maze($x,$y,0)
				if {$m & $::WALL(NORTH)} {
					set x1 [expr {$x * $sc}]
					set y1 [expr {$y * $sc}]
					set x2 [expr {$x1 + $sc + $linewidth}]
					set y2 [expr {$y1 + $linewidth}]
					gd fillrectangle $gd $black $x1 $y1 $x2 $y2
				}
				if {$m & $::WALL(WEST)} {
					set x1 [expr {$x * $sc}]
					set y1 [expr {$y * $sc}]
					set x2 [expr {$x1 + $linewidth}]
					set y2 [expr {$y1 + $sc}]
					gd fillrectangle $gd $black $x1 $y1 $x2 $y2
				}
				if {$m & $::WALL(SOUTH)} {
					set x1 [expr {$x * $sc}]
					set y1 [expr {($y+1) * $sc}]
					set x2 [expr {$x1 + $sc + $linewidth}]
					set y2 [expr {$y1 + $linewidth}]
					gd fillrectangle $gd $black $x1 $y1 $x2 $y2
				}
				if {$m & $::WALL(EAST)} {
					set x1 [expr {($x+1) * $sc}]
					set y1 [expr {$y * $sc}]
					set x2 [expr {$x1 + $linewidth}]
					set y2 [expr {$y1 + $sc}]
					gd fillrectangle $gd $black $x1 $y1 $x2 $y2
				}
			}
		}
		gd writePNGvar $gd img
		puts "Content-type: image/png\n"
		puts $img
	} eid]} {
		ncgi::header
		puts "error: $eid"
	}
} else {
	ncgi::header
	puts "<html><head><title>maze game</title></head>
	<body>
	<form method=get action=[file tail $::argv0]>
	caller id: <input name=cid maxlength=8 value=$cid>
	level: <select name=level onchange=\"this.form.submit();\">
	"
	for {set i 0} {$i <= $::maxlevel} {incr i} {
		set x ""
		if {$level == $i} {set x " selected"}
		puts "<option value=$i$x>$i"
	}
	puts "
	</select>
	<input type=submit>
	"
	puts "<br>"
	if {[expr {$level != "" && $cid != ""}]} {
		puts "<img src=\"[file tail $::argv0]?cid=$cid&level=$level&png=1\" alt=\"$alt\">"
	}

	puts "</form></body>
	</html>
	"
}
