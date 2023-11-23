//---------------------------------------------------------------------------
#pragma hdrstop

#include "GameViewMain.h"
#include "CastleVectors.hpp"
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
	// TODO: Kraft Vector3 is exposed in C++ (unlike Pascal) and forces
	// to define Vector3 by CastleVectors::Vector3.
	// Can we avoid it, moving Kraft to CastleTransform implementation only?

	Scene1->Translation = Scene1->Translation + Castlevectors::Vector3(0, 1, 0);
}

__fastcall void TViewMain::ClickBunny(TObject* Sender)
{
	// TODO: We prefer Url now, but URL is case-sensitive.
	// Rename to Url in CGE consistently.

    Scene1->URL = "castle-data:/Bunny.gltf";
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

__fastcall bool TViewMain::Press(const TInputPressRelease Event)
{
    bool result = TCastleView::Press(Event);
    if (result) {
        return result;
    }

// TODO
/*
var
  Body: TCastleRigidBody;
  NewTransform: TCastleTransform;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Push up model on key press
  if Event.IsKey(keyX) then
  begin
    if Viewport1.TransformUnderMouse <> nil then
    begin
      Body := Viewport1.TransformUnderMouse.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
      if Body <> nil then
        Body.ApplyImpulse(
          Vector3(0, 10, 0),
          Viewport1.TransformUnderMouse.Translation
        );
    end;

    Exit(true); // key was handled
  end;

  // instantiate model on key press, push it along sight
  if Event.IsKey(keyZ) then
  begin
   NewTransform := TransformLoad('castle-data:/my_box.castle-transform', FreeAtStop);
   NewTransform.Translation := Viewport1.Camera.Translation +
     Viewport1.Camera.Direction * 3;
   NewTransform.Direction := Viewport1.Camera.Direction;
   // could be done cleaner by loading NewTransform in new owner and looking for name
   Body := NewTransform.Items[0].FindBehavior(TCastleRigidBody) as TCastleRigidBody;
   Viewport1.Items.Add(NewTransform);
   Body.ApplyImpulse(
     Viewport1.Camera.Direction * 0.5,
     Viewport1.Camera.Translation);
  end;
end;

*/
    return result;
}

