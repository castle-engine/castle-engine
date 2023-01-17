unit mbColorList;

interface

{$MODE DELPHI}

uses
  SysUtils, LCLIntf, LCLType, Classes, Controls, StdCtrls,
  Graphics, GraphUtil, Forms, Themes,
  HTMLColors, RGBHSLUtils, RGBHSVUtils, RGBCMYKUtils, RGBCIEUtils,  PalUtils;

type
  TmbColor = record
   name: string;
   value: TColor;
  end;

  TDrawCaptionEvent = procedure (Sender: TObject; AIndex: integer; AFont: TFont; var AText: string; Selected: boolean) of object;
  TGetHintEvent = procedure (AIndex: integer; var AHint: string; var Handled: boolean) of object;

  TmbColorList = class(TCustomListBox)
  private
   FDraw: TDrawCaptionEvent;
   mx, my: integer;
   FGetHint: TGetHintEvent;
  protected
   procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
   Colors: array of TmbColor;

   constructor Create(AOwner: TComponent); override;
   procedure UpdateColors;
   procedure AddColor(AName: string; AValue: TColor; ARefresh: boolean = true);
   procedure ClearColors;
   function ColorCount: integer;
   procedure DeleteColor(AIndex: integer; ARefresh: boolean = true);
   procedure DeleteColorByName(AName: string; All: boolean);
   procedure DeleteColorByValue(Value: TColor; All: boolean);
   procedure InsertColor(AIndex: integer; AName: string; AValue: TColor);
  published
   property ParentColor default False;
   property TabStop default True;
   property Align;
   property Anchors;
   property BiDiMode;
   property BorderStyle;
   property Color;
   property Columns;
   property Constraints;
   property DragCursor;
   property DragKind;
   property DragMode;
   property Enabled;
   property ExtendedSelect;
   property Font;
   property IntegralHeight default true;
   property ItemHeight default 48;
   //property Items;          // wp: removed
   property MultiSelect;
   property ParentBiDiMode;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property Sorted;
   property TabOrder;
   property Visible;
   property OnDrawCaption: TDrawCaptionEvent read FDraw write FDraw;
   property OnGetHint: TGetHintEvent read FGetHint write FGetHint;
   property OnContextPopup;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnDrawItem;
   property OnEndDock;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMeasureItem;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnStartDock;
   property OnStartDrag;
  end;

implementation

constructor TmbColorList.Create(AOwner: TComponent);
begin
  inherited;
  style := lbOwnerDrawFixed;
  SetLength(Colors, 0);
  ItemHeight := 48;
  IntegralHeight := true;
  mx := -1;
  my := -1;
end;

procedure TmbColorList.AddColor(AName: string; AValue: TColor;
  ARefresh: boolean = true);
var
  l: integer;
begin
  l := Length(Colors);
  SetLength(Colors, l + 1);
  Colors[l].name := AName;
  Colors[l].value := AValue;
  if ARefresh then
    UpdateColors;
end;

procedure TmbColorList.CMHintShow(var Message: TCMHintShow);
var
  Handled: boolean;
  i: integer;
begin
  if PtInRect(ClientRect, Point(mx, my)) and ShowHint then
  begin
    i := ItemAtPos(Point(mx, my), true);
    if i > -1 then
      with TCMHintShow(Message) do
        if not ShowHint then
          Message.Result := 1
        else
          with HintInfo^ do
          begin
            Result := 0;
            ReshowTimeout := 2000;
            HideTimeout := 1000;
            Handled := false;
            if Assigned(FGetHint) then FGetHint(i, HintStr, Handled);
            if Handled then
              HintStr := FormatHint(HintStr, Colors[i].Value)
            else
              HintStr := Colors[i].Name;
          end;
  end;
  inherited;
end;

procedure TmbColorList.ClearColors;
begin
  SetLength(Colors, 0);
  UpdateColors;
end;

function TmbColorList.ColorCount: integer;
begin
  Result := Length(Colors);
end;

procedure TmbColorList.DeleteColor(AIndex: integer; ARefresh: boolean = true);
var
  i: integer;
begin
  if Length(Colors) = 0 then
    raise Exception.Create('There''s nothing to delete! The length of the array is 0.');

  if AIndex > Length(Colors) - 1 then
    raise Exception.Create(Format('List index out of bounds (%d)', [AIndex]));

  for i := AIndex to Length(Colors) - 2 do
    Colors[i] := Colors[i+1];
  SetLength(Colors, Length(Colors) - 1);
  if ARefresh then
    UpdateColors;
end;

procedure TmbColorList.DeleteColorByName(AName: string; All: boolean);
var
  i: integer;
begin
  for i := Length(Colors) - 1 downto 0 do
    if SameText(Colors[i].name, AName) then
    begin
      DeleteColor(i, false);
      if not All then
      begin
        UpdateColors;
        Exit;
      end;
    end;
  UpdateColors;
end;

procedure TmbColorList.DeleteColorByValue(Value: TColor; All: boolean);
var
  i: integer;
begin
  for i := Length(Colors) - 1 downto 0 do
  if Colors[i].Value = Value then
  begin
    DeleteColor(i, false);
    if not All then
    begin
      UpdateColors;
      Exit;
    end;
  end;
  UpdateColors;
end;

procedure TmbColorList.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  SR, TR, R: TRect;
  itemText: string;
begin
  if Length(Colors) = 0 then Exit;
  R := Rect;
  with Canvas do
  begin
    //background
    Pen.Color := clWindow;
    if odSelected in State then
      Brush.Color := clHighlight
    else
      Brush.Color := self.Color; //clBtnFace;
    FillRect(R);
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Right, R.Bottom - 1);
    //swatches
    SR := Classes.Rect(R.Left + 6, R.Top + 6, R.Left + ItemHeight - 6, R.Top + ItemHeight - 6);
    Brush.Color := Self.Colors[Index].value;
    if odSelected in State then
    begin
      if ThemeServices.ThemesEnabled then
      begin
        ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(teEditTextNormal), SR);
        InflateRect(SR, -2, -2);
        Brush.Color := Blend(Self.Colors[Index].value, clBlack, 80);
        FillRect(SR);
        InflateRect(SR, -1, -1);
        Brush.Color := Blend(self.Colors[Index].value, clBlack, 90);
        FillRect(SR);
        InflateRect(SR, -1, -1);
        Brush.Color := Self.Colors[Index].value;
        FillRect(SR);
      end
      else
      //windows 9x
      begin
        Pen.Color := clBackground;
        Brush.Color := clWindow;
        Rectangle(SR);
        InflateRect(SR, -1, -1);
        FillRect(SR);
        InflateRect(SR, 1, 1);
        InflateRect(SR, -2, -2);
        Brush.Color := Blend(Self.Colors[Index].value, clBlack, 75);
        FillRect(SR);
        InflateRect(SR, -1, -1);
        Brush.Color := Blend(Self.Colors[Index].value, clBlack, 87);
        FillRect(SR);
        InflateRect(SR, -1, -1);
        Brush.Color := Self.Colors[Index].value;
        FillRect(SR);
      end;
    end
    else
    //not selected
    begin
      //windows XP
      if ThemeServices.ThemesEnabled then
      begin
        ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(teEditTextNormal), SR);
        InflateRect(SR, -2, -2);
        Brush.Color := Self.Colors[Index].value;
        FillRect(SR);
      end
      else
      //windows 9x
      begin
        DrawEdge(Canvas.Handle, SR, BDR_SUNKENOUTER, BF_RECT);
        InflateRect(SR, -2, -2);
        Brush.Color := Self.Colors[Index].value;
        Pen.Color := clBlack;
        Rectangle(SR);
        InflateRect(SR, -1, -1);
        FillRect(SR);
        InflateRect(SR, 1, 1);
      end;
    end;
    //names
    Font.Style := [fsBold];
    if odSelected in State then
    begin
      Brush.Color := clHighlight;
      Pen.Color := clHighlightText;
      Font.Color := clHighlightText;
    end
    else
    begin
      Brush.Color := clBtnFace;
      Pen.Color := clWindowText;
      Font.Color := clWindowText;
    end;
    itemText := Items.Strings[Index];
    Canvas.Brush.Style := bsClear;
    TR := Classes.Rect(R.Left + ItemHeight, R.Top + (ItemHeight - TextHeight(itemText)) div 2, R.Right, R.Bottom - (ItemHeight - TextHeight(itemText)) div 2);
    if Assigned(FDraw) then FDraw(Self, Index, Canvas.Font, itemText, odSelected in State);
    DrawText(Canvas.Handle, PChar(itemText), Length(itemText), TR, DT_LEFT or DT_NOCLIP or DT_END_ELLIPSIS);
  end;
end;

procedure TmbColorList.InsertColor(AIndex: integer; AName: string; AValue: TColor);
var
  i: integer;
begin
  if AIndex > Length(Colors) - 1 then
    raise Exception.Create(Format('List index out of bounds (%d)', [AIndex]));

  SetLength(Colors, Length(Colors) + 1);
  for i := Length(Colors) - 1 downto AIndex do
    Colors[i] := Colors[i-1];

  Colors[AIndex].Name := AName;
  Colors[AIndex].Value := AValue;

  UpdateColors;
end;

procedure TmbColorList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  mx := x;
  my := y;
end;

procedure TmbColorList.UpdateColors;
var
  i: integer;
begin
  Items.Clear;
  for i := 0 to Length(Colors) - 1 do
    Items.Add(Colors[i].name);
end;

end.
