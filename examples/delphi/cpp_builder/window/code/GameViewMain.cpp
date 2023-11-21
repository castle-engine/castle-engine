//---------------------------------------------------------------------------

#pragma hdrstop

#include "GameViewMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

TViewMain *ViewMain;
//---------------------------------------------------------------------------


__fastcall TViewMain::TViewMain(TComponent* Owner) : TCastleView(Owner)
{
    DesignUrl = "castle-data:/gameviewmain.castle-user-interface";
}

// TODO: implement rest

__fastcall void TViewMain::ClickButton1(TObject* Sender)
{
}

__fastcall void TViewMain::ClickMove(TObject* Sender)
{
}

__fastcall void TViewMain::ClickBunny(TObject* Sender)
{
}

__fastcall void TViewMain::Start()
{
}

__fastcall void TViewMain::Update(const float SecondsPassed, bool &HandleInput)
{
}

__fastcall bool TViewMain::Press(const TInputPressRelease Event)
{
	bool result = TCastleView::Press(Event);
	if (result) {
		return result;
	}

    return result;
}

