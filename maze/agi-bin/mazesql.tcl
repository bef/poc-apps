##
## highscore support for the maze game
##

##
namespace eval ::mazedb {
##

package require mysqltcl

variable handle

## connect to mysql db
proc connect_db {} {
	variable handle
	array set db $::mysql_db
	set handle [mysqlconnect -host $db(host) -port $db(port) -user $db(user) -password $db(password) -db $db(db)]
}

## disconnect from mysql db
proc disconnect_db {} {
	variable handle
	mysqlclose $handle
}

## get name from eventphone phonebook
## mysql 5
proc getname_eventphone {cid} {
	variable handle
	set event_sql "SELECT event_name FROM
		(
			SELECT event_name, event_start-UNIX_TIMESTAMP(NOW())+(3600*24*7) AS x
			FROM guru.events
			HAVING x > 0
			ORDER BY x
			LIMIT 1
		) as sq1"
	set sql "SELECT ext_name
		FROM guru.phonebook
		WHERE extension = '[mysqlescape $cid]'
		AND event_name = ($event_sql)"

	return [mysqlsel $handle $sql -list]
}
## mysql < 5
proc getname_3 {cid} {
	variable handle
	set sql "SELECT event_name, event_start-UNIX_TIMESTAMP(NOW())+(3600*24*7) AS x
		FROM guru.events
		HAVING x > 0
		ORDER BY x
		LIMIT 1
		"
	set ret [mysqlsel $handle $sql -list]
	if {$ret == {}} { return "" }
	set eventname [lindex [lindex $ret 0] 0]

	set sql "SELECT ext_name
		FROM guru.phonebook
		WHERE extension = '[mysqlescape $cid]'
		AND event_name = '$eventname'"
	#puts $sql ;# REMOVE THIS!!
	set ret [mysqlsel $handle $sql -list]
	if {$ret == {}} { return "" }
	return [lindex [lindex $ret 0] 0]
}

## rename this to getname for use outside eventphone
proc getname {cid} {
	return "unknown"
}

## add time to highscore
proc add_to_highscore {cid level starttime endtime} {
	variable handle
	set sql "INSERT INTO highscore (cid, level, starttime, endtime, name)
		VALUES ('[mysqlescape $cid]', [mysqlescape $level], FROM_UNIXTIME($starttime), FROM_UNIXTIME($endtime), '[getname $cid]')"
	#puts $sql ;# REMOVE THIS!!
	mysqlexec $handle $sql
}

## TIMEDIFF
proc timediff {time1 time2} {
	## mysql >= 4.1.11
	return "TIMEDIFF($time1, $time2)"
	## mysql < 4.1.11
	#return "SEC_TO_TIME( (TO_DAYS($time1)*24*3600+TIME_TO_SEC($time1)) - (TO_DAYS($time2)*24*3600+TIME_TO_SEC($time2)) )"
}

## get level based highscore with max $limit results
proc get_level_highscore {level {limit {}}} {
	variable handle
	set level [mysqlescape $level]
	
	set sql "SELECT cid,
			name,
			level,
			[timediff endtime starttime] AS diff
		FROM highscore
		WHERE level = $level
		ORDER BY diff, endtime"
	if {$limit != {}} { set sql "$sql LIMIT $limit" }

	return [mysqlsel $handle $sql -list]
}

## get highscore with max $limit results
## mysql 5
proc get_highscore {{limit {}}} {
	variable handle
	## select continuous levels only to avoid cheaters being ranked higher
	set sql "SELECT * FROM highscore h1
		WHERE level = 0 OR EXISTS (
			SELECT 1 FROM highscore
			WHERE cid = h1.cid AND level = (h1.level - 1)
		)"
	## select last completed level and time over all shortest previous games for all cids
	set sql "SELECT cid,
			name,
			MAX(level) as maxlevel,
			[timediff endtime starttime] AS diff,
			SUM(TIME_TO_SEC([timediff endtime starttime])) AS time
		FROM ($sql) h
		WHERE NOT EXISTS (
			SELECT [timediff endtime starttime] AS diff2 FROM highscore
				WHERE cid = h.cid AND level = h.level HAVING diff2 < diff
		)
		GROUP BY cid
		ORDER BY diff, endtime"
	## sort by level
	set sql "SELECT cid, name, maxlevel, SEC_TO_TIME(time)
		FROM ($sql) AS SQ2
		ORDER BY maxlevel DESC"
	## limit output
	if {$limit != {}} { set sql "$sql LIMIT $limit" }
	#puts $sql ;# REMOVE THIS!!
	return [mysqlsel $handle $sql -list]
}


##
}
##
