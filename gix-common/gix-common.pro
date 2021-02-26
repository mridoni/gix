QT += network
TEMPLATE = lib
TARGET = gix-common

CONFIG(release,debug|release) DESTDIR = ../x64/Release
CONFIG(debug,debug|release) DESTDIR = ../x64/Debug

DEFINES += GIXCOMMON_LIB
LIBS += -L"."
DEPENDPATH += .

MOC_DIR += $$DESTDIR/GeneratedFiles
OBJECTS_DIR += $$DESTDIR/build
UI_DIR += $$DESTDIR/GeneratedFiles
RCC_DIR += $$DESTDIR/GeneratedFiles

include(gix-common.pri)
