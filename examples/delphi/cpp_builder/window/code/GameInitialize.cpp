//---------------------------------------------------------------------------
#pragma hdrstop

#include "CastleWindow.hpp"

#include "GameInitialize.h"
#include "GameViewMain.h"

#pragma link "GameViewMain"
//---------------------------------------------------------------------------
#pragma package(smart_init)

TCastleWindow* window;

_fastcall void ApplicationInitialize()
{
    // Adjust container settings for a scalable UI (adjusts to any window size in a smart way).
    window->Container->LoadSettings("castle-data:/CastleSettings.xml");

    // Create views (see https://castle-engine.io/views ).
    ViewMain = new TViewMain(Application());

    window->Container->View = ViewMain;
}

void GameInitialize()
{
    Application()->OnInitialize = ApplicationInitialize;

    window = new TCastleWindow(Application());
    Application()->MainWindow = window;
    window->ParseParameters();
}
