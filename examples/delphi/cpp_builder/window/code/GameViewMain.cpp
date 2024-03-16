//---------------------------------------------------------------------------
#pragma hdrstop

#include "GameViewMain.h"
#include "CastleVectors.hpp"
#include "CastleTransform.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)

TViewMain *ViewMain;
//---------------------------------------------------------------------------

__fastcall TViewMain::TViewMain(TComponent* Owner) : TCastleView(Owner)
{
    DesignUrl = "castle-data:/gameviewmain.castle-user-interface";
}

__fastcall void TViewMain::ClickMakeSound(TObject* Sender)
{
    WritelnLog("Button with sound clicked!");
    SoundEngine()->Play(SoundZap);
}

__fastcall void TViewMain::ClickMove(TObject* Sender)
{
    Scene1->Translation = Scene1->Translation + Vector3(0, 1, 0);
}

__fastcall void TViewMain::ClickBunny(TObject* Sender)
{
    Scene1->Url = "castle-data:/Bunny.gltf";
}

__fastcall void TViewMain::Start()
{
    TCastleView::Start();
    ButtonMakeSound->OnClick = ClickMakeSound;
    ButtonMove->OnClick = ClickMove;
    ButtonLoadBunny->OnClick = ClickBunny;
}

__fastcall void TViewMain::Update(const float SecondsPassed, bool &HandleInput)
{
    TCastleView::Update(SecondsPassed, HandleInput);
    // This virtual method is executed every frame (many times per second).

    assert(LabelFps != NULL); // "If you remove LabelFps from the design, remember to remove also the assignment \"LabelFps.Caption := ...\" from code");
    LabelFps->Caption = "FPS: " + Container()->Fps->ToString();
}

__fastcall bool TViewMain::Press(const TInputPressRelease &Event)
{
    /* To call methods, like IsKey, you need to copy Event contents.
       Otherwise C++ cannot know that the methods, like IsKey, preserve
       the object contents constant. */
    TInputPressRelease LocalEvent = Event;

    WritelnLog("Pressed: " + LocalEvent.ToString());

    bool result = TCastleView::Press(Event);
    if (result) {
        return result;
    }

    /* This virtual method is executed when user presses
      a key, a mouse button, or touches a touch-screen.

      Note that each UI control has also events like OnPress and OnClick.
      These events can be used to handle the "press", if it should do something
      specific when used in that UI control.
      The TViewMain.Press method should be used to handle keys
      not handled in children controls.
    */

    // Push up model on key press
    if (LocalEvent.IsKey(keyX)) {
        if (Viewport1->TransformUnderMouse() != NULL) {
            TCastleRigidBody* Body = (TCastleRigidBody*)
                Viewport1->TransformUnderMouse()->FindBehavior(__classid(TCastleRigidBody));
            if (Body != NULL) {
                Body->ApplyImpulse(
                    Vector3(0, 10, 0),
                    Viewport1->TransformUnderMouse()->Translation
                );
            }
        }
        return TRUE; // key was handled
    }

    // instantiate model on key press, push it along sight
    if (LocalEvent.IsKey(keyZ)) {
        TCastleTransform* NewTransform =
            TransformLoad("castle-data:/my_box.castle-transform", FreeAtStop());
        NewTransform->Translation = Viewport1->Camera->Translation +
            Viewport1->Camera->Direction * 3.0;
        NewTransform->Direction = Viewport1->Camera->Direction;

        // could be done cleaner by loading NewTransform in new owner and looking for name
        TCastleRigidBody* Body = (TCastleRigidBody*)
            NewTransform->Items[0]->FindBehavior(__classid(TCastleRigidBody));
        Viewport1->Items->Add(NewTransform);
        Body->ApplyImpulse(
            Viewport1->Camera->Direction * 0.5,
            Viewport1->Camera->Translation);
        return TRUE; // key was handled
    }

    return result;
}

