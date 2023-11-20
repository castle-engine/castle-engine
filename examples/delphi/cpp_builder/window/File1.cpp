#include <iostream>
#include <tchar.h>

#pragma link "CastleWindow"

#include "CastleWindow.hpp"
#include "CastleControls.hpp"

int _tmain(int argc, _TCHAR* argv[])
{
	TCastleWindow* window = new TCastleWindow(Application());

	TCastleButton* button = new TCastleButton(Application());
	button->Caption = "My Button";
	window->Container->Controls()->InsertFront(button);

	window->Open();
	Application()->Run();
}
