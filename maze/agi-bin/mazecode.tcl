## generate things out of cid & level
proc normalize_cid {cid} {
	regsub -all {[^\d]} $cid {} cid
	regsub {^0+} $cid {} cid
	if {[string length $cid] > 8} { set cid [string range $cid 0 3] }
	if {$cid == ""} { set cid 0 }
	return $cid
}
proc gen_maze {level cid} {
	set cid [normalize_cid $cid]
	expr $::gen_maze_srand
	NewMaze [expr {2+int(($level+1)/5)}] [expr {2+int($level/5)}] [expr {1 + $level % 5}]
}

proc code {level cid} {
	set cid [normalize_cid $cid]
	return [format "%04d" [expr $::code_algo]]
}


