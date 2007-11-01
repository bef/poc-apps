ASTERISK_BASEDIR=/var/lib/asterisk
ASTERISK_AGI_BIN=$(ASTERISK_BASEDIR)/agi-bin
ASTERISK_SOUNDS=$(ASTERISK_BASEDIR)/sounds
WWW_DIR=/var/www/asterisk

all:
	@echo "nothing to be done. use 'make install' to install."

install:
	-[ -d agi-bin ] && cp -r ./agi-bin/* $(ASTERISK_AGI_BIN)/
	-[ -d htdocs ] && cp -r ./htdocs/* $(WWW_DIR)/
	-[ -d sounds ] && cp -r ./sounds/* $(ASTERISK_SOUNDS)/

