ASTERISK_BASEDIR=/var/lib/asterisk
ASTERISK_AGI_BIN=$(ASTERISK_BASEDIR)/agi-bin
ASTERISK_SOUNDS=$(ASTERISK_BASEDIR)/sounds
YATE_BASEDIR=/usr/local/share/yate
YATE_SCRIPTS=$(YATE_BASEDIR)/scripts
YATE_SOUNDS=$(YATE_BASEDIR)/sounds
WWW_DIR=/var/www/asterisk

all:
	-@echo "nothing to be done."
	-@echo "use make install-asterisk or install-yate or install-yate-symlink"
	-@echo "INFO for Yate users: better convert .wav sounds to .slin format, e.g. with sox"

install-asterisk:
	-[ -d agi-bin ] && cp -r ./agi-bin/* $(ASTERISK_AGI_BIN)/
	-[ -d htdocs ] && cp -r ./htdocs/* $(WWW_DIR)/
	-[ -d sounds ] && cp -r ./sounds/* $(ASTERISK_SOUNDS)/

install-yate:
	-[ -d yate ] && cp -r ./yate/* $(YATE_SCRIPTS)/
	-[ -d sounds ] && cp -r ./sounds/* $(YATE_SOUNDS)/

install-yate-link:
	ln -s `pwd`/yate/mr.tcl $(YATE_SCRIPTS)/
	ln -s `pwd`/sounds/mr $(YATE_SOUNDS)/
	