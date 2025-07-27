#-------------------------------------------------
#
# Project created by QtCreator 2014-03-26T10:33:25
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += opengl widgets openglwidgets

TARGET = qt_library_tester
TEMPLATE = app

# put all compiled files into "bin" subfolder
DESTDIR = bin

CgePath = "$$PWD/../../../src/deprecated_library"
INCLUDEPATH += $$CgePath

DEFINES += QT_BUILD=1

!msvc: QMAKE_CXXFLAGS += "-Wno-unused-parameter"

SOURCES += main.cpp\
        mainwindow.cpp \
    $$CgePath/castlelib_c_loader.cpp \
    glwidget.cpp

HEADERS  += mainwindow.h \
    glwidget.h

FORMS    += mainwindow.ui

# copy the castle engine dll next to exe
win32: QMAKE_POST_LINK += copy /Y \"$$CgePath\\castleengine.dll\" \"$$DESTDIR\\\"
linux: QMAKE_POST_LINK += cp -f \"$$CgePath/libcastleengine.so\" \"$$DESTDIR/\"
mac {
  APP_DYLIB_FILES.files = $$CgePath/libcastleengine.dylib
  APP_DYLIB_FILES.path = Contents/MacOS
  QMAKE_BUNDLE_DATA += APP_DYLIB_FILES
}
