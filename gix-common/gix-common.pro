QT += widgets network xml xmlpatterns
TEMPLATE = lib
TARGET = gix-common
INCLUDEPATH += . ./buildsystem ./projectsystem ./metadata ../libgixutils ../libgixpp

CONFIG(release,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Release
CONFIG(debug,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Debug

DEFINES += GIXCOMMON_LIB

linux:QMAKE_LFLAGS_DEBUG += -rdynamic -Wl,--whole-archive -lgixutils -lgixpp -Wl,--no-whole-archive
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -Wno-unknown-pragmas 

linux:QMAKE_LFLAGS_RELEASE+= -Wl,--whole-archive -lgixutils -lgixpp -Wl,--no-whole-archive
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas

win32:QMAKE_LFLAGS_RELEASE+= -Wl,--whole-archive -lgixutils -lgixpp -Wl,--no-whole-archive
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -Wno-unknown-pragmas  -I ../build-tools/grammar-tools
win32:QMAKE_LIBS = -lgixutils -lgixpp
win32:DEFINES -= UNICODE _UNICODE

DEFINES += GIXCOMMON_LIB
LIBS += -L$(DESTDIR) 
DEPENDPATH += .

MOC_DIR += GeneratedFiles
OBJECTS_DIR += build
UI_DIR += GeneratedFiles
RCC_DIR += GeneratedFiles

include(gix-common.pri)
