{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main user interface class.
  This implements the majority of this application functionality. }
unit GameStateMain;

interface

uses CastleUIState, CastleComponentSerialize, CastleControls;

type
  { Main user interface class.
    This implements the majority of this application functionality. }
  TStateMain = class(TUIState)
  private
    SoundSourceUiTemplate: TSerializedComponent;
    LabelSoundSources: TCastleLabel;
    GroupSoundBuffers, GroupSoundSources: TCastleVerticalGroup;
    ButtonExit: TCastleButton;
    procedure ClickExit(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, Classes,
  CastleLog, CastleSoundEngine, CastleUIControls, CastleWindow;

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find useful components by name }
  LabelSoundSources := UiOwner.FindRequiredComponent('LabelSoundSources') as TCastleLabel;
  GroupSoundBuffers := UiOwner.FindRequiredComponent('GroupSoundBuffers') as TCastleVerticalGroup;
  GroupSoundSources := UiOwner.FindRequiredComponent('GroupSoundSources') as TCastleVerticalGroup;
  ButtonExit := UiOwner.FindRequiredComponent('ButtonExit') as TCastleButton;

  LabelSoundSources.Caption := Format('Currently playing sound sources (max %d):',
    [SoundEngine.MaxAllocatedSources]);
  ButtonExit.OnClick := @ClickExit;

  SoundSourceUiTemplate := TSerializedComponent.Create('castle-data:/part_sound_source.castle-user-interface');
end;

procedure TStateMain.ClickExit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
