include ../vsn.mk

VSN=$(FAST_AGI_VSN)

include ../../../support/include.mk

all: $(ERL_OBJECTS) ../ebin/fast_agi.app

clean:
	-rm $(ERL_OBJECTS)

$(EBIN_DIR)/fast_agi_socket.beam: ../include/fast_agi.hrl