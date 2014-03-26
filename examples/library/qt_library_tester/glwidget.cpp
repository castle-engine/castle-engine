/*
  Copyright 2014 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

#include <QtWidgets>
#include <QtOpenGL>

#include "glwidget.h"
#include "mainwindow.h"
#include "../../../src/library/castleengine.h"

GLWidget *g_pThis = NULL;

GLWidget::GLWidget(QWidget *parent) :
    QGLWidget(QGLFormat(QGL::SampleBuffers), parent)
{
    g_pThis = this;
    m_bAfterInit = false;
    setMouseTracking(true);

    QTimer *pUpdateTimer = new QTimer(this);
    connect(pUpdateTimer, SIGNAL(timeout()), this, SLOT(OnUpdateTimer()));
    pUpdateTimer->start(20);    // 50 fps
}

GLWidget::~GLWidget()
{
    CGE_Close();
    g_pThis = NULL;
}

void GLWidget::OpenScene(QString const &sFilename)
{
    if (!m_bAfterInit)
    {
        m_sSceneToOpen = sFilename;
        return;
    }

    CGE_LoadSceneFromFile(sFilename.toUtf8());
    ((MainWindow*)g_pThis->parent())->UpdateAfterSceneLoaded();
}

QSize GLWidget::minimumSizeHint() const
{
    return QSize(200, 200);
}

QSize GLWidget::sizeHint() const
{
    return QSize(400, 400);
}

int __cdecl OpenGlLibraryCallback(int eCode, int iParam1, int iParam2)
{
    switch (eCode)
    {
    case ecgelibNeedsDisplay:
        g_pThis->updateGL();
        return 1;

    case ecgelibSetMouseCursor:
        {
            QCursor aNewCur;
            switch (iParam1)
            {
            case ecgecursorWait: aNewCur.setShape(Qt::WaitCursor); break;
            case ecgecursorHand: aNewCur.setShape(Qt::PointingHandCursor); break;
            case ecgecursorText: aNewCur.setShape(Qt::IBeamCursor); break;
            default: aNewCur.setShape(Qt::ArrowCursor);
            }
            g_pThis->setCursor(aNewCur);
        }
        return 1;

    case ecgelibNavigationTypeChanged:
        ((MainWindow*)g_pThis->parent())->UpdateNavigationButtons();
        return 1;
    }
    return 0;
}

void GLWidget::initializeGL()
{
    CGE_LoadLibrary();
    CGE_Open(ecgeofLog);
    CGE_SetUserInterface(false, 96);
    CGE_SetLibraryCallbackProc(OpenGlLibraryCallback);
    m_bAfterInit = true;
    if (!m_sSceneToOpen.isEmpty())
        OpenScene(m_sSceneToOpen);
}

void GLWidget::OnUpdateTimer()
{
    CGE_Update();
}

void GLWidget::paintGL()
{
    CGE_Render();
}

void GLWidget::resizeGL(int width, int height)
{
    CGE_Resize(width, height);
}

void GLWidget::mousePressEvent(QMouseEvent *event)
{
    CGE_MouseDown(event->x(), height() - 1 - event->y(), event->button()==Qt::LeftButton, 0);
}

void GLWidget::mouseMoveEvent(QMouseEvent *event)
{
    CGE_Motion(event->x(), height() - 1 - event->y(), 0);
}

void GLWidget::mouseReleaseEvent(QMouseEvent *event)
{
    CGE_MouseUp(event->x(), height() - 1 - event->y(), event->button()==Qt::LeftButton, 0);
}

#ifndef QT_NO_WHEELEVENT
void GLWidget::wheelEvent(QWheelEvent *event)
{
    CGE_MouseWheel(event->delta(), event->orientation()==Qt::Vertical);
}
#endif

