##
## maze generator for the maze game
##
## written by Keith Vetter
## original location: http://wiki.tcl.tk/4159
## highly modified by BeF <bef@erlangen.ccc.de>
##


set sz(x) 15  ;# Maze width
set sz(y) 15  ;# Maze height
set sz(z)  1  ;# Maze levels -- you can have 3-d mazes

##+##########################################################################
#
# Init
#
# Sets up some global variables.
#
proc Init {} {
    global sz DIR WALL DOOR MOTION

    set sz(w) 550                               ;# Canvas width
    set sz(h) 550                               ;# Canvas height

    # These directions also act as bit shift amounts
    array set DIR {NORTH 0 EAST 1 UP 2 SOUTH 3 WEST 4 DOWN 5 }
    array set WALL {
        NORTH 0x01 EAST 0x02 UP 0x04 SOUTH 0x08 WEST 0x10 DOWN 0x20 ANY 0x3F
    }
    array set DOOR {
        NORTH 0x0100 EAST 0x0200 UP 0x0400 SOUTH 0x0800 WEST 0x1000 DOWN 0x2000
        ANY 0x3F00
    }
    array set MOTION {
        0,x 0 0,y -1 0,z 0    1,x  1 1,y 0 1,z 0   2,x 0 2,y 0 2,z -1
        3,x 0 3,y  1 3,z 0    4,x -1 4,y 0 4,z 0   5,x 0 5,y 0 5,z  1
    }
}

proc WALLDIR {dir}     {return [expr {$::WALL(NORTH) << $dir}] }
proc DOORDIR {dir}     {return [expr {$::DOOR(NORTH) << $dir}] }
proc WALLDOORDIR {dir} {return [expr {($::WALL(NORTH) | $::DOOR(NORTH))<<$dir}]}
proc OPPOSITE {dir}    {return [expr {($dir + 3) % 6}] }
proc BACKINFO {dir}    {return [expr {($dir + 1) << 16}]}
proc BACKUNINFO {val}  {return [expr {($val >> 16) - 1}]}
proc MOVETO {x y z dir} { list [incr x $::MOTION($dir,x)] \
                              [incr y $::MOTION($dir,y)] \
                              [incr z $::MOTION($dir,z)]
}
##+##########################################################################
#
# NewMaze
#
# Creates a new maze of a given size.
#
proc NewMaze {{x -1} {y -1} {z 1}} {
    if {$x != -1} { set ::sz(x) $x ; set ::sz(y) $y ; set ::sz(z) $z }

    FillMaze
}
##+##########################################################################
#
# InitMaze
#
# Set up matrix and pick start and ending points
#
proc InitMaze {} {
    global maze sz
    catch {unset maze}

    for {set x 0} {$x < $sz(x)} {incr x} {      ;# Set all cells to 0
        for {set y 0} {$y < $sz(y)} {incr y} {
            for {set z 0} {$z < $sz(z)} {incr z} {
                set maze($x,$y,$z) 0
            }
        }
    }
    for {set z 0} {$z < $sz(z)} {incr z} {      ;# North, south walls
        for {set x 0} {$x < $sz(x)} {incr x} {
            OrMaze $x 0 $z   $::WALL(NORTH)
            OrMaze $x [expr {$sz(y) - 1}] $z $::WALL(SOUTH)
        }
    }
    for {set z 0} {$z < $sz(z)} {incr z} {      ;# East, west walls
        for {set y 0} {$y < $sz(y)} {incr y} {
            OrMaze 0 $y $z   $::WALL(WEST)
            OrMaze [expr {$sz(x) - 1}] $y $z $::WALL(EAST)
        }
    }
    for {set x 0} {$x < $sz(x)} {incr x} {      ;# Up, down walls
        for {set y 0} {$y < $sz(y)} {incr y} {
            OrMaze $x $y 0   $::WALL(UP)
            OrMaze $x $y [expr {$sz(z) - 1}] $::WALL(DOWN)
        }
    }
}
##+##########################################################################
#
# FillMaze
#
# Does the actual maze creation by randomly walking around the maze.
#
proc FillMaze {} {
    global sz maze

    InitMaze
    set ::mstack {}
    eval PushPos [PickEntrance]
    set cnt [expr {$sz(x) * $sz(y) * $sz(z)}]

    while {1} {
        foreach {px py pz} [PopPos] break
        if {$px == -1} break                    ;# We're done
        set newDir [PickDir $px $py $pz]        ;# Get a new direction
        if {$newDir == -1} continue             ;# Can't move, try new position
        set whence [OPPOSITE $newDir]

        PushPos $px $py $pz
        OrMaze $px $py $pz [DOORDIR $newDir]    ;# Add door in the new direction

        # Cell we move into
        foreach {px py pz} [MOVETO $px $py $pz $newDir] break

        # It too has a door
        PushPos $px $py $pz
        OrMaze $px $py $pz [DOORDIR $whence]

        # Stuff solution info into high bits
        OrMaze $px $py $pz [BACKINFO $whence]
        #if {([incr cnt -1] % 100) == 0} { INFO "Thinking $cnt" }
    }

    # Now open the outer wall up for our entrance and exit
    set maze($sz(start)) [expr {$maze($sz(start)) & ~$::WALL(WEST)}]
    set maze($sz(end))   [expr {$maze($sz(end))   & ~$::WALL(EAST)}]
}
##+##########################################################################
#
# PickEntrance
#
# Pick where the entrance and exit should be.
#
proc PickEntrance {} {
    set y1 [expr {int(rand() * $::sz(y))}]
    set y2 [expr {int(rand() * $::sz(y))}]

    set ::sz(start) "0,$y1,0"
    set ::sz(end)   "[expr {$::sz(x) - 1}],$y2,[expr {$::sz(z) - 1}]"
    return [list 0 $y1 0]
}
##+##########################################################################
#
# PickDir
#
# Picks a random legal direction to move from (px,py,pz), -1 if no move.
#
proc PickDir {px py pz} {
    set dirs {}
    foreach dir {0 1 2 3 4 5} {
        eval lappend dirs [OKDir? $px $py $pz $dir]
    }

    set len [llength $dirs]
    if {$len == 0} {return -1}
    return [lindex $dirs [expr {int(rand() * $len)}]]
}
##+##########################################################################
#
# OKDir?
#
# Sees if it's legal to move in direction dir. If that cell is
# already visited then we put up a wall.
#
proc OKDir? {px py pz dir} {
    if {$::maze($px,$py,$pz) & [WALLDOORDIR $dir]} {return ""}
    foreach {px2 py2 pz2} [MOVETO $px $py $pz $dir] break
    if {$::maze($px2,$py2,$pz2) & $::DOOR(ANY)} { ;# Destination already done?
        OrMaze $px $py $pz [WALLDIR $dir]
        OrMaze $px2 $py2 $pz2 [WALLDIR [OPPOSITE $dir]]
        return ""
    }
    return $dir
}
##+##########################################################################
#
# OrMaze
#
# Helper function to logically OR value to maze(x,y,z)
#
proc OrMaze {x y z value} {
    set ::maze($x,$y,$z) [expr {$::maze($x,$y,$z) | $value}]
}
##+##########################################################################
#
# PushPos
#
# Pushes a position onto stack stack
#
proc PushPos {x y z} {
    lappend ::mstack [list $x $y $z]
    return ""
}
##+##########################################################################
#
# PopPos
#
# Pops top position off the stack. If we always take the top, then the
# maze will have one main corridor from the initial random walk. So we
# occassionally pick a position at random.
#
proc PopPos {} {
    set len [llength $::mstack]
    if {$len == 0} { return [list -1 -1 -1]}

    set where end
    if {rand() > .8} { set where [expr {int(rand() * $len)}] }
    set pos [lindex $::mstack end]
    set ::mstack [lrange $::mstack 0 end-1]
    return $pos
}

##+##########################################################################
## textmode output
##+##########################################################################
proc show_level {z} {
	set row1 ""
	set row2 ""
	set row3 ""
	set ret {}
	for {set y 0} {$y < $::sz(y)} {incr y} {
		set row1 ""
		set row2 ""
		set row3 ""
		for {set x 0} {$x < $::sz(x)} {incr x} {
			set m $::maze($x,$y,$z)
			#puts "${x}x${y}: [expr {$m & $::WALL(NORTH)}]"
			if {$m & $::WALL(NORTH)} { set row1 "${row1}+--+" } else { set row1 "${row1}+  +" }
			if {$m & $::WALL(SOUTH)} { set row3 "${row3}+--+" } else { set row3 "${row3}+  +" }
			if {$m & $::WALL(WEST)} { set row2 "${row2}|" } else { set row2 "${row2} " }
			if {$m & $::DOOR(UP)} { set row2 "${row2}u" } else { set row2 "${row2} " }
			if {$m & $::DOOR(DOWN)} { set row2 "${row2}d" } else { set row2 "${row2} " }
			if {$m & $::WALL(EAST)} { set row2 "${row2}|" } else { set row2 "${row2} " }
		}
		lappend ret $row1 $row2 $row3
	}
	return $ret
}

##+##########################################################################

proc show_maze {} {
	set ret {}
	for {set z 0} {$z < $::sz(z)} {incr z} {
		lappend ret "level $z" [show_level $z]
	}
}

##+##########################################################################

Init
