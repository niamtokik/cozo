all: c_src/cozo_nif.so c_src/libcozo_c.so

c_src/cozo_nif.so:
	cd c_src && make all

c_src/libcozo_c.so:
	cd c_src && make libcozo_c.so


