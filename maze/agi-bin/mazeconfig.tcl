##
## configuration file for the maze game
################################################################################

## language: de/en
set language de

## number of levels
set maxlevel 50

################################################################################
## highscore -- this requires a properly set up mysql server
set use_highscore 0
set mysql_db {	host 127.0.0.1 \
		port 3306 \
		user maze \
		password "test123" \
		db maze}


################################################################################
## secret maze generation and code algorithms
set gen_maze_srand {srand($cid+$level+1)}
set code_algo {int(pow(($cid+$level)%9023,2))%9493}

