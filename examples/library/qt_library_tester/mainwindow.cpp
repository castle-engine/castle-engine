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

#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "glwidget.h"

#include <QDialog>
#include <QFileDialog>
#include <QPlainTextEdit>
#include <QVBoxLayout>

#include "../../../src/library/castleengine.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    m_nViewpointCount = m_iCurrentViewpoint = 0;
    m_pConsoleWnd = NULL;

    m_pGlWidget = new GLWidget(this);
    setCentralWidget(m_pGlWidget);

    connect(ui->actionOpen, SIGNAL(triggered()), this, SLOT(OnFileOpenClick()));
    connect(ui->actionWalk, SIGNAL(triggered()), this, SLOT(OnWalkClick()));
    connect(ui->actionFly, SIGNAL(triggered()), this, SLOT(OnFlyClick()));
    connect(ui->actionExamine, SIGNAL(triggered()), this, SLOT(OnExamineClick()));
    connect(ui->actionTurntable, SIGNAL(triggered()), this, SLOT(OnTurntableClick()));
    connect(ui->actionNextView, SIGNAL(triggered()), this, SLOT(OnNextViewClick()));
    connect(ui->actionPrevView, SIGNAL(triggered()), this, SLOT(OnPrevViewClick()));

    connect(ui->actionSSAO, SIGNAL(triggered()), this, SLOT(MenuSoftShadowsClick()));
    connect(ui->actionHead_Bobbing, SIGNAL(triggered()), this, SLOT(MenuWalkingEffectClick()));
    connect(ui->actionMouse_Look, SIGNAL(triggered()), this, SLOT(MenuMouseLookClick()));
    connect(ui->actionOpenGL_Information, SIGNAL(triggered()), this, SLOT(MenuOpenGLInfoClick()));
    connect(ui->actionShow_Log, SIGNAL(triggered()), this, SLOT(MenuShowLogClick()));

    m_pGlWidget->OpenScene("../../../../demo_models/navigation/type_walk.wrl");
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::OnFileOpenClick()
{
    QString sFile = QFileDialog::getOpenFileName(this, tr("Open Scene"), ".", tr("3D scenes") + " (*.wrl *.x3d)");
    if (!sFile.isEmpty())
        m_pGlWidget->OpenScene(sFile);
}

void MainWindow::UpdateNavigationButtons()
{
    ECgeNavigationType eNav = (ECgeNavigationType)CGE_GetNavigationType();
    ui->actionWalk->setChecked(eNav == ecgenavWalk);
    ui->actionFly->setChecked(eNav == ecgenavFly);
    ui->actionExamine->setChecked(eNav == ecgenavExamine);
    ui->actionTurntable->setChecked(eNav == ecgenavTurntable);
}

void MainWindow::OnWalkClick()
{
    CGE_SetNavigationType(ecgenavWalk);
}

void MainWindow::OnFlyClick()
{
    CGE_SetNavigationType(ecgenavFly);
}

void MainWindow::OnExamineClick()
{
    CGE_SetNavigationType(ecgenavExamine);
}

void MainWindow::OnTurntableClick()
{
    CGE_SetNavigationType(ecgenavTurntable);
}

void MainWindow::UpdateAfterSceneLoaded()
{
    ui->menuViewpoints->clear();
    // show viewpoints available
    int nCount = CGE_GetViewpointsCount();
    for (int i = 0; i < nCount; i++)
    {
        char sName[512];
        CGE_GetViewpointName(i, sName, 512);
        ActionWithTag *pAct = new ActionWithTag(QString::fromUtf8(sName), i, ui->menuViewpoints);
        connect(pAct, SIGNAL(triggered()), this, SLOT(OnMoveToViewpointClick()));
        ui->menuViewpoints->addAction(pAct);
    }
    m_iCurrentViewpoint = 0;
    m_nViewpointCount = nCount;

    ui->actionHead_Bobbing->setChecked(CGE_GetVariableInt(ecgevarWalkHeadBobbing)>0);
    ui->actionSSAO->setChecked(CGE_GetVariableInt(ecgevarEffectSSAO)>0);
}

void MainWindow::OnMoveToViewpointClick()
{
    ActionWithTag *pAct = qobject_cast<ActionWithTag*>(sender());
    if (pAct == NULL) return;
    MoveToViewpoint(pAct->m_nTag);
}

void MainWindow::OnNextViewClick()
{
    MoveToViewpoint(m_iCurrentViewpoint+1);
}

void MainWindow::OnPrevViewClick()
{
    MoveToViewpoint(m_iCurrentViewpoint-1);
}

void MainWindow::MoveToViewpoint(int nView)
{
    if (nView < 0) nView = m_nViewpointCount-1; // for cycling
    if (nView > m_nViewpointCount) nView = 0;
    m_iCurrentViewpoint = nView;
    CGE_MoveToViewpoint(m_iCurrentViewpoint, true);
}

ActionWithTag::ActionWithTag(QString const& sCaption, int nTag, QObject * parent)
    : QAction(parent), m_nTag(nTag)
{
    setText(sCaption);
}

void MainWindow::MenuSoftShadowsClick()
{
    bool bSwitchOn = ui->actionSSAO->isChecked();
    CGE_SetVariableInt(ecgevarEffectSSAO, bSwitchOn ? 1 : 0);
}

void MainWindow::MenuAntiAliasingClick()
{
    // TODO
}

void MainWindow::MenuWalkingEffectClick()
{
    bool bSwitchOn = ui->actionHead_Bobbing->isChecked();
    CGE_SetVariableInt(ecgevarWalkHeadBobbing, bSwitchOn ? 1 : 0);
}

void MainWindow::MenuMouseLookClick()
{
    CGE_SetVariableInt(ecgevarMouseLook, 1);
    CGE_SetVariableInt(ecgevarCrossHair, 1);
}

void MainWindow::AddNewWarning(QString const& sWarning)
{
    if (m_pConsoleWnd==NULL)
        MenuShowLogClick();
    QPlainTextEdit *pEdit = qobject_cast<QPlainTextEdit*>(m_pConsoleWnd->layout()->itemAt(0)->widget());
    if (pEdit!=NULL)
        pEdit->appendPlainText(sWarning);
}

void MainWindow::MenuShowLogClick()
{
    if (m_pConsoleWnd==NULL)
    {
        m_pConsoleWnd = new QDialog(this);
        m_pConsoleWnd->setWindowTitle(tr("Log - Warnings"));
        m_pConsoleWnd->setWindowFlags(m_pConsoleWnd->windowFlags() & ~Qt::WindowContextHelpButtonHint);

        QPlainTextEdit *pEdit = new QPlainTextEdit(m_pConsoleWnd);
        pEdit->setMinimumSize(600, 500);
        pEdit->setReadOnly(true);

        QVBoxLayout *pLayout = new QVBoxLayout(m_pConsoleWnd);
        pLayout->setContentsMargins(0, 0, 0, 0);
        m_pConsoleWnd->setLayout(pLayout);
        pLayout->insertWidget(0, pEdit);
        m_pConsoleWnd->resize(m_pConsoleWnd->minimumSize());
    }
    m_pConsoleWnd->show();
}

void MainWindow::MenuOpenGLInfoClick()
{
    char szBuf[8192];
    CGE_GetOpenGLInformation(szBuf, 8192);

    QDialog aDlg(this);
    aDlg.setWindowTitle(tr("OpenGL Information"));
    aDlg.setWindowFlags(aDlg.windowFlags() & ~Qt::WindowContextHelpButtonHint);

    QPlainTextEdit *pEdit = new QPlainTextEdit(&aDlg);
    pEdit->setPlainText(QString::fromUtf8(szBuf));
    pEdit->setMinimumSize(500, 500);
    pEdit->setReadOnly(true);

    QVBoxLayout *pLayout = new QVBoxLayout(&aDlg);
    pLayout->setContentsMargins(0, 0, 0, 0);
    aDlg.setLayout(pLayout);
    pLayout->insertWidget(0, pEdit);
    aDlg.resize(aDlg.minimumSize());
    aDlg.exec();
}
