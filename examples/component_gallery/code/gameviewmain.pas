{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    PageButton2: TCastleDesign;
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
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleColors;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  I: Integer;
  ButtonToggle: TCastleButton;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  for I := 1 to PagesCount do
  begin
    PageButtons[I] := DesignedComponent('Button' + PageNames[I]) as TCastleButton;
    PageButtons[I].Tag := I; // use button's tag to store page index
    PageButtons[I].OnClick := {$ifdef FPC}@{$endif}ClickPageButton;
    Pages[I] := DesignedComponent('Page' + PageNames[I]) as TCastleUserInterface;
  end;

  { Find components inside TCastleDesigns (use TCastleDesigns.DesignedComponent) }
  ButtonToggle := PageButton2.DesignedComponent('ButtonToggle') as TCastleButton;

  ButtonToggle.OnClick := {$ifdef FPC}@{$endif}ClickToggle;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickPageButton(Sender: TObject);
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

procedure TViewMain.ClickToggle(Sender: TObject);
var
  Button: TCastleButton;
begin
  Button := Sender as TCastleButton;
  Button.Pressed := not Button.Pressed;
end;

end.
