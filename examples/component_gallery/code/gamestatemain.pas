{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    const
      PagesCount = 12;

      { Suffixes of component names in the design,
        to find ButtonXxx and PageXxx matching controls. }
      PageNames: array [1..PagesCount] of String = (
        'Intro',
        'EmptyRectangle',
        'ColorRectangle',
        'Label',
        'Image',
        'Button',
        'Button2',
        'Checkbox',
        'Edit',
        'HorizontalVerticalGroup',
        'ScrollView',
        'Shape'
      );

    var
      { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
      LabelFps: TCastleLabel;
      PageButtons: array [1..PagesCount] of TCastleButton;
      Pages: array [1..PagesCount] of TCastleUserInterface;

    procedure ClickPageButton(Sender: TObject);
    procedure ClickToggle(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleColors;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
var
  I: Integer;
  PageButtons2: TCastleDesign;
  ButtonToggle: TCastleButton;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  for I := 1 to PagesCount do
  begin
    PageButtons[I] := DesignedComponent('Button' + PageNames[I]) as TCastleButton;
    PageButtons[I].Tag := I; // use button's tag to store page index
    PageButtons[I].OnClick := {$ifdef FPC}@{$endif}ClickPageButton;
    Pages[I] := DesignedComponent('Page' + PageNames[I]) as TCastleUserInterface;
  end;

  { Find components inside TCastleDesigns (this needs 2 steps - first find the TCastleDesign) }
  PageButtons2 := DesignedComponent('PageButton2') as TCastleDesign;
  ButtonToggle := PageButtons2.DesignedComponent('ButtonToggle') as TCastleButton;

  ButtonToggle.OnClick := {$ifdef FPC}@{$endif}ClickToggle;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.ClickPageButton(Sender: TObject);
var
  I, CurrentPage: Integer;
  PageButton: TCastleButton;
  PageLabel: TCastleLabel;
begin
  PageButton := Sender as TCastleButton;
  CurrentPage := PageButton.Tag;
  for I := 1 to PagesCount do
  begin
    PageButtons[I].Pressed := I = CurrentPage;
    // adjust color of 1st label inside the button
    if (PageButtons[I].ControlsCount >= 1) and
       (PageButtons[I].Controls[0] is TCastleLabel) then
    begin
      PageLabel := PageButtons[I].Controls[0] as TCastleLabel;
      if I = CurrentPage then
        PageLabel.Color := White
      else
        PageLabel.Color := Black;
    end;
    Pages[I].Exists := I = CurrentPage;
  end;
end;

procedure TStateMain.ClickToggle(Sender: TObject);
var
  Button: TCastleButton;
begin
  Button := Sender as TCastleButton;
  Button.Pressed := not Button.Pressed;
end;

end.
