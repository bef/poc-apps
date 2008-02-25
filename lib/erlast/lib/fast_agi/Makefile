include ../../support/subdir.mk
include ./vsn.mk

APPNAME=fast_agi
DOC_OPTS=[{def,{vsn,"$(FAST_AGI_VSN)"}}]

docs:
	erl -noshell -pa $(BINDIR) -run edoc_run application \
	    "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
