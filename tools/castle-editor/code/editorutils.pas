{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple castle-editor utilities. }
unit EditorUtils;

{$mode objfpc}{$H+}

interface

uses Classes, Types, Controls, StdCtrls;

procedure ErrorBox(const Message: String);
procedure WarningBox(const Message: String);
function YesNoBox(const Message: String): Boolean;

type
  TOutputKind = (
    okInfo,
    okImportantInfo,
    okWarning,
    okError
  );

  { Instance of this takes control of your TListBox to display messages. }
  TOutputList = class
  strict private
    type
      TOutputInfo = class
        Kind: TOutputKind;
      end;
    var
      OutputInfoPerKind: array [TOutputKind] of TOutputInfo;
      FList: TListBox;
    procedure DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    constructor Create(const AList: TListBox);
    property List: TListBox read FList;

    procedure AddLine(const S: String; const Kind: TOutputKind);
    { Split S into multiple lines.
      Add each of them as separate line.
      Avoids adding empty lines at the very end (since they would
      make the look uglier; instead,
      we make nice separators with AddSeparator). }
    procedure SplitAddLines(const S: String; const Kind: TOutputKind);
    procedure AddSeparator;
    procedure Clear;
    procedure ScrollBottom;
  end;

implementation

uses SysUtils, Dialogs, Graphics;

procedure ErrorBox(const Message: String);
begin
  MessageDlg('Error', Message, mtError, [mbOK], 0);
end;

procedure WarningBox(const Message: String);
begin
  MessageDlg('Warning', Message, mtWarning, [mbOK], 0);
end;

function YesNoBox(const Message: String): Boolean;
begin
  Result := MessageDlg('Question', Message, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

{ TOutputList --------------------------------------------------------------- }

procedure TOutputList.DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);

  { Copied from LCL stdctrls.pp }
  procedure InternalDrawItem(Control: TControl;
    Canvas: TCanvas; ARect: TRect; const Text: string);
  var
    OldBrushStyle: TBrushStyle;
    OldTextStyle: TTextStyle;
    NewTextStyle: TTextStyle;
  begin
    OldBrushStyle := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;

    OldTextStyle := Canvas.TextStyle;
    NewTextStyle := OldTextStyle;
    NewTextStyle.Layout := tlCenter;
    NewTextStyle.RightToLeft := Control.UseRightToLeftReading;
    if Control.UseRightToLeftAlignment then
    begin
      NewTextStyle.Alignment := taRightJustify;
      ARect.Right := ARect.Right - 2;
    end
    else
    begin
      NewTextStyle.Alignment := taLeftJustify;
      ARect.Left := ARect.Left + 2;
    end;

    Canvas.TextStyle := NewTextStyle;

    Canvas.TextRect(ARect, ARect.Left, ARect.Top, Text);
    Canvas.Brush.Style := OldBrushStyle;
    Canvas.TextStyle := OldTextStyle;
  end;

var
  C: TCanvas;
  OutputInfo: TOutputInfo;
begin
  C := List.Canvas;
  OutputInfo := List.Items.Objects[Index] as TOutputInfo;

  case OutputInfo.Kind of
    okImportantInfo: C.Font.Bold := true;
    okWarning      : C.Brush.Color := clYellow;
    okError        : C.Brush.Color := clRed;
  end;

  C.FillRect(ARect);
  InternalDrawItem(List, C, ARect, List.Items[Index]);
end;

constructor TOutputList.Create(const AList: TListBox);
var
  Kind: TOutputKind;
begin
  inherited Create;

  FList := AList;
  FList.Style := lbOwnerDrawFixed;
  FList.OnDrawItem := @DrawItem;

  { Create OutputInfoPerKind instances.
    The idea is that every AddLine call should not construct new
    TOutputInfo, this could eat quite some memory for a lot of lines.
    Instead most AddLine calls can just take OutputInfoPerKind[Kind]. }
  for Kind in TOutputKind do
  begin
    OutputInfoPerKind[Kind] := TOutputInfo.Create;
    OutputInfoPerKind[Kind].Kind := Kind;
  end;
end;

procedure TOutputList.AddLine(const S: String; const Kind: TOutputKind);
begin
  List.Items.AddObject(S, OutputInfoPerKind[Kind]);
end;

procedure TOutputList.SplitAddLines(const S: String; const Kind: TOutputKind);
var
  SL: TStringList;
  Line: String;
begin
  SL := TStringList.Create;
  try
    SL.Text := S;
    { remove empty lines at end }
    while (SL.Count <> 0) and (SL[SL.Count - 1] = '') do
      SL.Delete(SL.Count - 1);
    for Line in SL do
      AddLine(Line, Kind);
  finally FreeAndNil(SL) end;
end;

procedure TOutputList.AddSeparator;
begin
  AddLine('', okInfo);
end;

procedure TOutputList.Clear;
begin
  List.Items.Clear;
end;

procedure TOutputList.ScrollBottom;
begin
  List.TopIndex := List.Items.Count - 1;
end;

end.
