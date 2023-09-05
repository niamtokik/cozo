######################################################################
# GNU Makefile
######################################################################

COZO_REPOSITORY = https://github.com/cozodb/cozo/releases/download
COZO_VERSION = 0.7.2
COZO_CHECKSUM = 472921fc7a944fe5bdf040aa154cafdd6b23ce6401b4ad96abb9a41747c84df6
COZO_ARCH = x86_64
COZO_OS = unknown-linux-gnu
COZO_LIBC = libcozo_c-$(COZO_VERSION)-$(COZO_ARCH)-$(COZO_OS)

# CC_OPTS = -fpic -shared
CC_OPTS = -shared -fpic

ASDF_ERLANG_VERSION = 26.0.2

all: cozo_nif.so

clean:
	-rm cozo_nif.so

clean-all: clean
	-rm cozo_c.h
	-rm $(COZO_LIBC).so.gz
	-rm $(COZO_LIBC).so
	-rm libcozo_c.so

cozo_nif.so:
	cc cozo_nif.c -o $@ \
	  -I $(HOME)/.asdf/installs/erlang/$(ASDF_ERLANG_VERSION)/usr/include \
	  -L $(HOME)/.asdf/installs/erlang/$(ASDF_ERLANG_VERSION)/usr/lib \
	  -L $$(pwd) \
	  -lei -lcozo_c -shared -fPIC 

cozo_c.h:
	wget "$(COZO_REPOSITORY)/v$(COZO_VERSION)/$@"

$(COZO_LIBC).so.gz:
	wget "$(COZO_REPOSITORY)/v$(COZO_VERSION)/$@"

$(COZO_LIBC).so: $(COZO_LIBC).so.gz
	gunzip $(COZO_LIBC).so.gz

libcozo_c.so: $(COZO_LIBC).so
	mv $(COZO_LIBC).so $@
