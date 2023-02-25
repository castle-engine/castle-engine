#-------------------------------------------------
#
# Project created by QtCreator 2014-03-26T10:33:25
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += opengl widgets openglwidgets

TARGET = qt_library_tester
TEMPLATE = app

DEFINES += QT_BUILD=1

QMAKE_CXXFLAGS += "-Wno-unused-parameter"

SOURCES += main.cpp\
        mainwindow.cpp \
    ../../../src/deprecated_library/castlelib_c_loader.cpp \
    glwidget.cpp

HEADERS  += mainwindow.h \
    ../../../src/deprecated_library/castleengine.h \
    glwidget.h

FORMS    += mainwindow.ui


mac {
  APP_DYLIB_FILES.files = ../../../src/deprecated_library/libcastleengine.dylib
  APP_DYLIB_FILES.path = Contents/MacOS
  QMAKE_BUNDLE_DATA += APP_DYLIB_FILES
}
