#include ../../gix.cf

CC=g++
CPPFLAGS=-fPIC -g -I ../libgixsql -std=c++11
DEPS = DbInterfaceODBC.h
OBJS = DbInterfaceODBC.o  dblib.o utils.o

LDFLAGS=
LIBS=-lm -lodbc -ldl

all: $(OUTPUT_DIR)/libgixsql-odbc.so

%.o: %.cpp $(DEPS)
	$(CC) -c -o  $@ $< $(CPPFLAGS)

$(OUTPUT_DIR)/libgixsql-odbc.so: $(OBJS)
	$(CC) -shared -fPIC -o $@ $^ $(CFLAGS) $(LDFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f libgixsql-odbc.so *.o

install: libgixsql-odbc.so
	mkdir -p $(GIX_INSTALL_DIR)/lib && \
	cp libgixsql-odbc.so $(GIX_INSTALL_DIR)/lib
