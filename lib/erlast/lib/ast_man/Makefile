SUBDIRS=src

include ../../support/subdir.mk
include ./vsn.mk

APPNAME=ast_man
DOC_OPTS=[{def,{vsn,"$(AST_MAN_VSN)"}}]

docs:
	erl -noshell -pa $(BINDIR) -run edoc_run application \
            "'$(APPNAME)'" '"."' '$(DOC_OPTS)' -s init stop
