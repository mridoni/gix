#include ../../gix.cf

CC=g++
CPPFLAGS=-fPIC -g -I ../libgixsql -I /usr/include/mysql -std=c++11
DEPS = DbInterfaceMySQL.h
OBJS = DbInterfaceMySQL.o  dblib.o

LDFLAGS=-L /usr/lib/x86_64-linux-gnu
LIBS=-lm -lmysqlclient -ldl

all: $(OUTPUT_DIR)/libgixsql-mysql.so

%.o: %.cpp $(DEPS)
	$(CC) -c -o  $@ $< $(CPPFLAGS)

$(OUTPUT_DIR)/libgixsql-mysql.so: $(OBJS)
	$(CC) -shared -fPIC -o $@ $^ $(CFLAGS) $(LDFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f $(OUTPUT_DIR)/libgixsql-mysql.so *.o

#install: libgixsql-mysql.so
#	mkdir -p $(GIX_INSTALL_DIR)/lib && \
#	cp libgixsql-mysql.so $(GIX_INSTALL_DIR)/lib
