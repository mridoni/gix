QT       += core gui
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = ScintillaEdit
TEMPLATE = lib
CONFIG += lib_bundle c++17

VERSION = 4.3.2

QMAKE_CXXFLAGS = -std=c++17

SOURCES += \
    ScintillaEdit.cpp \
    ScintillaDocument.cpp \
    ../ScintillaEditBase/PlatQt.cpp \
    ../ScintillaEditBase/ScintillaQt.cpp \
    ../ScintillaEditBase/ScintillaEditBase.cpp \
    ../../src/XPM.cxx \
    ../../src/ViewStyle.cxx \
    ../../src/UniqueString.cxx \
    ../../src/UniConversion.cxx \
    ../../src/Style.cxx \
    ../../src/Selection.cxx \
    ../../src/ScintillaBase.cxx \
    ../../src/RunStyles.cxx \
    ../../src/RESearch.cxx \
    ../../src/PositionCache.cxx \
    ../../src/PerLine.cxx \
    ../../src/MarginView.cxx \
    ../../src/LineMarker.cxx \
    ../../src/KeyMap.cxx \
    ../../src/Indicator.cxx \
    ../../src/ExternalLexer.cxx \
    ../../src/EditView.cxx \
    ../../src/Editor.cxx \
    ../../src/EditModel.cxx \
    ../../src/Document.cxx \
    ../../src/Decoration.cxx \
    ../../src/DBCS.cxx \
    ../../src/ContractionState.cxx \
    ../../src/CharClassify.cxx \
    ../../src/CellBuffer.cxx \
    ../../src/Catalogue.cxx \
    ../../src/CaseFolder.cxx \
    ../../src/CaseConvert.cxx \
    ../../src/CallTip.cxx \
    ../../src/AutoComplete.cxx \
    ../../lexlib/WordList.cxx \
    ../../lexlib/StyleContext.cxx \
    ../../lexlib/PropSetSimple.cxx \
    ../../lexlib/LexerSimple.cxx \
    ../../lexlib/LexerNoExceptions.cxx \
    ../../lexlib/LexerModule.cxx \
    ../../lexlib/LexerBase.cxx \
    ../../lexlib/DefaultLexer.cxx \
    ../../lexlib/CharacterSet.cxx \
    ../../lexlib/CharacterCategory.cxx \
    ../../lexlib/Accessor.cxx \
    $$files(../../lexers/*.cxx, false)

HEADERS  += \
    ScintillaEdit.h \
    ScintillaDocument.h \
    ../ScintillaEditBase/ScintillaEditBase.h \
    ../ScintillaEditBase/ScintillaQt.h

OTHER_FILES +=

INCLUDEPATH += ../ScintillaEditBase ../../include ../../src ../../lexlib

DEFINES += SCINTILLA_QT=1 MAKING_LIBRARY=1 SCI_LEXER=1 _CRT_SECURE_NO_DEPRECATE=1
CONFIG(release, debug|release) {
    DEFINES += NDEBUG=1
}

win32:DEFINES -= UNICODE _UNICODE

win32:CONFIG(release,debug|release) DESTDIR = ../../../../$$(HOST_PLATFORM)/Release
win32:CONFIG(debug,debug|release) DESTDIR = ../../../../$$(HOST_PLATFORM)/Debug

linux:CONFIG(release,debug|release) DESTDIR = ../../../../$$(HOST_PLATFORM)/Release
linux:CONFIG(debug,debug|release) DESTDIR = ../../../../$$(HOST_PLATFORM)/Debug

macx {
	QMAKE_LFLAGS_SONAME = -Wl,-install_name,@executable_path/../Frameworks/
}
