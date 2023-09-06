TARGET = c_src/cozo_nif.so c_src/libcozo_c.so c_src/cozo_c.h
LIBS = priv/cozo_nif.so priv/libcozo_c.so

all: $(TARGET) $(LIBS)

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

