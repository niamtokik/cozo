################################################################################
# Makefile
################################################################################
unexport LDFLAGS
unexport CFLAGS
unexport CXXFLAGS

################################################################################
# GLOBAL Configuration
################################################################################
LOCAL_DIR ?= $(shell pwd)
PRIV_DIR ?= priv
CSRC_DIR ?= c_src
CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)
PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

################################################################################
# ERTS Configuration
################################################################################
ifdef ASDF_DIR
	ASDF_ERLANG_VERSION ?= $(shell asdf current erlang | awk '{print $$2}')
	ERTS_INCLUDE_DIR ?= $(ASDF_DIR)/installs/erlang/$(ASDF_ERLANG_VERSION)/usr/include
	ERL_INTERFACE_LIB_DIR ?= $(ASDF_DIR)/installs/erlang/$(ASDF_ERLANG_VERSION)/usr/lib
else
	ERL_BIN_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/bin/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
	ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
	ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)])." -s init stop)
	ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)])." -s init stop)
endif

################################################################################
# COZODB Configuration
################################################################################
COZO_REPOSITORY = https://github.com/cozodb/cozo/releases/download
COZO_VERSION ?= 0.7.2
COZO_LIB_CHECKSUM ?= 472921fc7a944fe5bdf040aa154cafdd6b23ce6401b4ad96abb9a41747c84df6

# Architecture Auto configuration
UNAME_M := $(shell uname -m)
ifeq ($(UNAME_M), x86_64)
	COZO_LIB_ARCH = x86_64
else ifeq ($(UNAME_M), aarch64)
	COZO_LIB_ARCH = aarch64
else ifeq ($(UNAME_M), arm64)
	COZO_LIB_ARCH = aarch64
else ifeq ($(UNAME_M), armv7l)
	COZO_LIB_ARCH = armv7
else
	$(error "Cozo doesn't support $(UNAME_M) architecture")
endif

# Operating System Auto Configuration
UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	COZO_LIB_OS = apple-darwin
	COZO_LIBC_EXT = dylib
	LDFLAGS = -dynamiclib -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), Linux)
	COZO_LIB_OS = unknown-linux-gnu
	COZO_LIBC_EXT = so
	LDFLAGS = -shared
else
	$(error "Cozo does not support $(UNAME_SYS) operating system")
endif

# Configure checksum based on the os/arch
ifeq ($(UNAME_SYS)-$(UNAME_M), unknown-linux-gnu-x86_64)
	COZO_LIB_CHECKSUM ?= 472921fc7a944fe5bdf040aa154cafdd6b23ce6401b4ad96abb9a41747c84df6
endif

# Cozo Path Building
COZO_LIB_PREFIX = libcozo_c-$(COZO_VERSION)-$(COZO_LIB_ARCH)-$(COZO_LIB_OS)
COZO_LIB_NAME = libcozo_c-$(COZO_VERSION)-$(COZO_LIB_ARCH)-$(COZO_LIB_OS).$(COZO_LIBC_EXT)
COZO_LIB_PACKAGE = $(COZO_LIB_NAME).gz
COZO_LIB_URL = $(COZO_REPOSITORY)/v$(COZO_VERSION)/$(COZO_LIB_PACKAGE)
COZO_HEADER_NAME = cozo_c.h
COZO_HEADER_CHECKSUM = 2cc2d84f626825000294f138983aa8fb374be6ab1c045d0ce1a1748753a37243
COZO_HEADER_URL = $(COZO_REPOSITORY)/v$(COZO_VERSION)/$(COZO_HEADER_NAME)

################################################################################
# COMPILER Configuration
################################################################################
CC_FLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes -I $(ERTS_INCLUDE_DIR) -L $(ERL_INTERFACE_LIB_DIR)
CC_INCLUDE = -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(LOCAL_DIR)/c_src -I $(LOCAL_DIR)/$(PRIV_DIR)
LDLIBS = -L $(LOCAL_DIR)/c_src -L $(LOCAL_DIR)/$(PRIV_DIR)
CC_OPTS ?= $(CC_INCLUDE) $(LDLIBS) -lei -lcozo_c -fPIC $(CC_FLAGS) $(LDFLAGS)

################################################################################
# Default Targets
################################################################################
TARGETS = $(CSRC_DIR)/cozo_c.h \
	$(PRIV_DIR)/libcozo_c.so \
	$(PRIV_DIR)/cozo_nif.so

# Used to start rebar3 and erlang
LD_LIBRARY_PATH ?= $(LOCAL_DIR)/$(PRIV_DIR)
DYLD_FALLBACK_LIBRARY_PATH ?= $(LD_LIBRARY_PATH) # for macOS
CFLAG_RUNTIME_LIBRARY_PATH ?= $(LOCAL_DIR)/$(PRIV_DIR)
ENV_BOOTSTRAP ?= LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) CFLAG_RUNTIME_LIBRARY_PATH=$(CFLAG_RUNTIME_LIBRARY_PATH)

################################################################################
# Makefile body
################################################################################
.PHONY += help
help:
	@echo "Usage: make [all|deps|compile|test|doc|shell|clean]"

.PHONY += all
all: deps compile test doc

deps: $(PRIV_DIR)/cozo_nif.so

.PHONY += compile
compile:
	$(ENV_BOOTSTRAP) rebar3 compile

.PHONY += test
test:
	$(ENV_BOOTSTRAP) rebar3 ct

.PHONY += doc
doc:
	$(ENV_BOOTSTRAP) rebar3 ex_doc skip_deps=true

.PHONY += shell
shell:
	$(ENV_BOOTSTRAP) rebar3 shell

.PHONY += clean
clean:
	-rm $(TARGETS)

.PHONY: $(.PHONY)

################################################################################
# Main Targets
################################################################################
$(CSRC_DIR)/cozo_c.h:
	wget -qO $@ $(COZO_HEADER_URL)

$(PRIV_DIR):
	mkdir $@

$(PRIV_DIR)/$(COZO_LIB_PACKAGE): | $(PRIV_DIR)
	wget -qO $@ $(COZO_LIB_URL)

$(PRIV_DIR)/$(COZO_LIB_NAME): $(PRIV_DIR)/$(COZO_LIB_PACKAGE)
	cd $(PRIV_DIR) && gunzip --keep $(COZO_LIB_PACKAGE)

$(PRIV_DIR)/libcozo_c.so: $(PRIV_DIR)/$(COZO_LIB_NAME)
	cd $(PRIV_DIR) && ln $(COZO_LIB_NAME) libcozo_c.so

$(PRIV_DIR)/cozo_nif.so: $(CSRC_DIR)/cozo_c.h $(PRIV_DIR)/libcozo_c.so
ifeq ($(UNAME_SYS), Darwin)
	@# Required for dlopen() to find libcozo_c on macOS.
	@# Changes the dynamic shared library install name and rpath recorded in the
	@# binary
	install_name_tool -id \
	$(LOCAL_DIR)/$(PRIV_DIR)/libcozo_c.so \
	$(LOCAL_DIR)/$(PRIV_DIR)/libcozo_c.so
endif
	$(CC) c_src/cozo_nif.c $(CC_OPTS) -o $(@)

