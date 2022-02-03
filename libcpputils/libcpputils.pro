TEMPLATE = lib
CONFIG += staticlib
TARGET = cpputils
win32:TARGET = libcpputils
INCLUDEPATH += .

CONFIG(debug,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Debug
CONFIG(release,debug|release) DESTDIR = ../$$(HOST_PLATFORM)/Release

linux:QMAKE_LFLAGS_DEBUG += -rdynamic
linux:QMAKE_CXXFLAGS_DEBUG += -O0 -std=c++17 -D_DEBUG

linux:QMAKE_LFLAGS_RELEASE+= 
linux:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17

win32:QMAKE_LFLAGS_RELEASE+= 
win32:QMAKE_CXXFLAGS_RELEASE+= -O3 -std=c++17 -D_HAS_STD_BYTE=0
win32:DEFINES -= UNICODE _UNICODE

HEADERS += CopyResolver.h \
			ErrorData.h \
			libcpputils.h \

SOURCES += CopyResolver.cpp \
		    libcpputils.cpp
		   
