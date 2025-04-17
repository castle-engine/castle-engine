//---------------------------------------------------------------------------

#ifndef GameViewMainH
#define GameViewMainH
//---------------------------------------------------------------------------
#include "CastleUIControls.hpp"
#include "CastleControls.hpp"
#include "CastleSoundEngine.hpp"
#include "CastleScene.hpp"
#include "CastleViewport.hpp"
//---------------------------------------------------------------------------
class TViewMain : public TCastleView
{
__published:
    // Components designed using CGE editor.
    // These fields will be automatically initialized at Start.
    TCastleLabel* LabelFps;
    TCastleButton* ButtonMakeSound;
    TCastleButton* ButtonMove;
    TCastleButton* ButtonLoadBunny;
    TCastleSound* SoundZap;
	TCastleScene* Scene1;
	TCastleViewport* Viewport1;
private:
    __fastcall void ClickMakeSound(TObject* Sender);
    __fastcall void ClickMove(TObject* Sender);
    __fastcall void ClickBunny(TObject* Sender);
public:
    __fastcall TViewMain(TComponent* Owner);
    virtual __fastcall void Start();
    virtual __fastcall void Update(const float SecondsPassed, bool &HandleInput);
    virtual __fastcall bool Press(const TInputPressRelease &Event);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewMain *ViewMain;
//---------------------------------------------------------------------------
#endif
