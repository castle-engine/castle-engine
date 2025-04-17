//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Fmx.CastleControl"
// just test
//#pragma link "CastleScene"
//#pragma link "X3DNodes"
#pragma resource "*.fmx"
TForm1 *Form1;

// TUiTest --------------------------------------------------------------

/*
  Define simple TCastleUserInterface descendant, just to capture
  the moment OpenGL(ES) context is created and fill Memo1 will information.
*/
class TUiTest : public TCastleUserInterface
{
public:
	__fastcall TUiTest(TComponent* Owner) : TCastleUserInterface(Owner)
	{
	}

	virtual void __fastcall GLContextOpen()
	{
		Form1->Memo1->Lines->Add(GLInformationString());
	}
};

//---------------------------------------------------------------------------

__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
	CastleControl1->Container->DesignUrl =
	  "castle-data:/test_3d.castle-user-interface";
	  //"castle-data:/test_2d.castle-user-interface";
	  //"castle-data:/test_ui.castle-user-interface";
	CastleControl1->Container->Controls()->InsertFront(new TUiTest(this));
}

//---------------------------------------------------------------------------
