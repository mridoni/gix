#include ../../gix.cf

CC=g++
CPPFLAGS=-fPIC -g -I /usr/include/postgresql -I ../libgixsql -std=c++11
DEPS = DbInterfacePGSQL.h
OBJS = DbInterfacePGSQL.o  dblib.o

LDFLAGS=
LIBS=-lm -lpq -ldl

all: $(OUTPUT_DIR)/libgixsql-pgsql.so

%.o: %.cpp $(DEPS)
	$(CC) -c -o  $@ $< $(CPPFLAGS)

$(OUTPUT_DIR)/libgixsql-pgsql.so: $(OBJS)
	$(CC) -shared -fPIC -o $@ $^ $(CFLAGS) $(LDFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f $(OUTPUT_DIR)/libgixsql-pgsql.so *.o

#install: libgixsql-pgsql.so
#	mkdir -p $(GIX_INSTALL_DIR)/lib && \
#	cp libgixsql-pgsql.so $(GIX_INSTALL_DIR)/lib
