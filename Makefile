######################################################################
# Makefile
######################################################################
unexport LDFLAGS
unexport CFLAGS
unexport CXXFLAGS

TARGETS = c_src/libcozo_c.so \
	c_src/cozo_c.h \
	priv/cozo_nif.so \
	priv/libcozo_c.so \
	c_src/cozo_nif.so

PRIV_DIR ?= $(shell pwd)/priv

.PHONY += all
all: $(TARGETS)

.PHONY += test
shell:
	LD_LIBRARY_PATH=$(PRIV_DIR) \
	CFLAG_RUNTIME_LIBRARY_PATH=$(PRIV_DIR) \
	rebar3 shell

.PHONY += test
test:
	LD_LIBRARY_PATH=$(PRIV_DIR) \
	CFLAG_RUNTIME_LIBRARY_PATH=$(PRIV_DIR) \
	rebar3 ct

.PHONY += doc
doc:
	LD_LIBRARY_PATH=$(PRIV_DIR) \
	CFLAG_RUNTIME_LIBRARY_PATH=$(PRIV_DIR) \
	rebar3 edoc

.PHONY += clean
clean:
	-cd c_src && make clean
	-rm priv/*.so

c_src/cozo_nif.so:
	cd c_src && make all

c_src/libcozo_c.so:
	cd c_src && make libcozo_c.so

c_src/cozo_c.h:
	cd c_src && make cozo_c.h

priv:
	mkdir $@

priv/cozo_nif.so: priv
	cp c_src/cozo_nif.so $@

priv/libcozo_c.so: priv
	cp c_src/libcozo_c.so $@

.PHONY: $(.PHONY)
