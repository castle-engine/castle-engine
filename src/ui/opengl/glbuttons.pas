{
  Copyright 2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Button drawn inside OpenGL context.

  This is TUIControl descendant, so to use it just add it to
  the TGLUIWindow.Controls or TKamOpenGLControl.Controls list.
  You will also want to adjust position/size (TGLButton.Left,
  TGLButton.Bottom, TGLButton.Width, TGLButton.Height)
  and assign TGLButton.OnClick (or ovevrride TGLButton.DoClick). }
unit GLButtons;

interface

uses UIControls, OpenGLFonts, KeysMouse, Classes;

type
  TGLButton = class(TUIControl)
  private
    Font: TGLBitmapFont_Abstract;
    FLeft: Integer;
    FBottom: Integer;
    FWidth: Cardinal;
    FHeight: Cardinal;
    FOnClick: TNotifyEvent;
    FCaption: string;
  public
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw(const Focused: boolean); override;
    function PositionInside(const X, Y: Integer): boolean; override;
    procedure GLContextInit; override;
    procedure GLContextClose; override;
    function MouseDown(const Button: TMouseButton): boolean; override;
    { Called when user clicks the button. In this class, simply calls
      OnClick callback. }
    procedure DoClick; virtual;
  published
    property Left: Integer read FLeft write FLeft default 0;
    property Bottom: Integer read FBottom write FBottom default 0;
    property Width: Cardinal read FWidth write FWidth default 100;
    property Height: Cardinal read FHeight write FHeight default 100;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Caption: string read FCaption write FCaption;
  end;

{ Create and destroy the default UI interface bitmap font.

  They don't actually create new font each time --- first create
  creates the font, next ones only increase the internal counter.
  Destroy decreases the counter and only really frees when it goes to zero.

  The bottom line: you should use them just like normal create / destroy
  (always pair a destroy with a create; destroying @nil is allowed NOOP
  for comfort). But they work fast.

  @groupBegin }
function CreateUIFont: TGLBitmapFont_Abstract;
procedure DestroyUIFont(var Font: TGLBitmapFont_Abstract);
{ @groupEnd }

implementation

uses SysUtils, GL, BFNT_BitstreamVeraSans_Unit, OpenGLBmpFonts;

{ TGLButton ------------------------------------------------------------------ }

function TGLButton.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure TGLButton.Draw(const Focused: boolean);
begin
  glColor3f(1, 1, 0);
  glRectf(Left, Bottom, Left + Width, Bottom + Height);

  glColor3f(0.2, 0.2, 0.2);
  glRasterPos2i(20, 80);
  Font.Print(Caption);
end;

function TGLButton.PositionInside(const X, Y: Integer): boolean;
begin
  Result :=
    (X >= Left) and
    (X  < Left + Width) and
    (ContainerHeight - Y >= Bottom) and
    (ContainerHeight - Y  < Bottom + Height);
end;

procedure TGLButton.GLContextInit;
begin
  inherited;
  Font := CreateUIFont;
end;

procedure TGLButton.GLContextClose;
begin
  DestroyUIFont(Font);
  inherited;
end;

function TGLButton.MouseDown(const Button: KeysMouse.TMouseButton): boolean;
begin
  { TODO: it would be better to make "mouse capture" in containers,
    and generate there click only when you press moue button over this control,
    and then release over this control. (and in between, all mouse events
    go to the "captured" control.) }
  DoClick;
  Result := true;
end;

procedure TGLButton.DoClick;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

{ UIFont --------------------------------------------------------------------- }

var
  UIFont: TGLBitmapFont_Abstract;
  UIFontUsed: Cardinal;

function CreateUIFont: TGLBitmapFont_Abstract;
begin
  if UIFont = nil then
  begin
    UIFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
    UIFontUsed := 0;
  end;

  Inc(UIFontUsed);
  Result := UIFont;
end;

procedure DestroyUIFont(var Font: TGLBitmapFont_Abstract);
begin
  if Font <> nil then
  begin
    Assert(Font = UIFont, 'You can pass to DestroyUIFont only fonts created with CreateUIFont');
    Dec(UIFontUsed);
    if UIFontUsed = 0 then
      FreeAndNil(UIFont);
    Font := nil;
  end;
end;

end.
