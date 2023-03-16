unit mbColorTree;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, ComCtrls, Graphics, Themes,
  GraphUtil, ImgList, Forms,
  HTMLColors;

type
  TmbColor = record
    Name: string;
    Value: TColor;
  end;

  TDrawCaptionEvent = procedure (Sender: TObject; AIndex: integer; AFont: TFont; var AText: string; Selected: boolean) of object;
  TDrawLabelEvent = procedure (Sender: TObject; AIndex: integer; AFont: TFont; var AText: string) of object;
  TGetHintEvent = procedure (AIndex: integer; var AHint: string; var Handled: boolean) of object;

  TmbColorTree = class(TCustomTreeView)
  private
    FInfo1, FInfo2: string;
    FInfoLabel: string;
    FDraw: TDrawCaptionEvent;
    FDraw1, FDraw2, FDraw3: TDrawLabelEvent;
    mx, my: integer;
    FGetHint: TGetHintEvent;
    FOnStartDrag: TStartDragEvent;
    FOnEndDrag: TEndDragEvent;
    procedure SetInfo1(Value: string);
    procedure SetInfo2(Value: string);
    procedure SetInfoLabel(Value: string);
  protected
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      {%H-}Stage: TCustomDrawStage; var {%H-}PaintImages: Boolean): Boolean; override;
    procedure DoArrow(c: TCanvas; dir: TScrollDirection; p: TPoint; sel: boolean);
    procedure DrawColorItem(R: TRect; Selected: boolean; AIndex: Integer;
      AItemText: String; Expanded: boolean); dynamic;
    procedure DrawInfoItem(R: TRect; Index: integer); dynamic;
    function IsCustomDrawn({%H-}Target: TCustomDrawTarget; {%H-}Stage: TCustomDrawStage): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    Colors: array of TmbColor;
    constructor Create(AOwner: TComponent); override;
    procedure AddColor(AName: string; AValue: TColor; ARefresh: boolean = true);
    procedure ClearColors;
    function ColorCount: integer;
    procedure DeleteColor(AIndex: integer; ARefresh: boolean = true);
    procedure DeleteColorByName(AName: string; All: boolean);
    procedure DeleteColorByValue(AValue: TColor; All: boolean);
    procedure InsertColor(AIndex: integer; AName: string; AValue: TColor);
    procedure UpdateColors;
  published
    property InfoLabelText: string read FInfoLabel write SetInfoLabel;
    property InfoDisplay1: string read FInfo1 write SetInfo1;
    property InfoDisplay2: string read FInfo2 write SetInfo2;
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderStyle;
    property BorderWidth;
    property Constraints;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Indent;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightClickSelect;
    property ShowHint;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    property OnGetHint: TGetHintEvent read FGetHint write FGetHint;
    property OnDrawCaption: TDrawCaptionEvent read FDraw write FDraw;
    property OnDrawInfoLabel: TDrawLabelEvent read FDraw1 write FDraw1;
    property OnDrawInfoDisplay1: TDrawLabelEvent read FDraw2 write FDraw2;
    property OnDrawInfoDisplay2: TDrawLabelEvent read FDraw3 write FDraw3;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;
    property Items;
  end;

implementation

uses
  PalUtils, mbUtils;

{  TmbColorTree  }

constructor TmbColorTree.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csDisplayDragImage];
  ReadOnly := true;
  ShowButtons := false;
  ShowLines := false;
  ShowRoot := true;
  RowSelect := true;
  HotTrack := false;
  SetLength(Colors, 0);
  Images := TImageList.Create(Self);
  Images.Width := 48;
  Images.Height := 48;
  FInfoLabel := 'Color Values:';
  FInfo1 := 'RGB: %r.%g.%b';
  FInfo2 := 'HEX: #%hex';
end;

procedure TmbColorTree.AddColor(AName: string; AValue: TColor;
  ARefresh: boolean = true);
var
  L: integer;
begin
  L := Length(Colors);
  SetLength(Colors, L + 1);
  Colors[L].Name := AName;
  Colors[L].Value := AValue;
  if ARefresh then
    UpdateColors;
end;

procedure TmbColorTree.ClearColors;
begin
  SetLength(Colors, 0);
  UpdateColors;
end;

procedure TmbColorTree.CMHintShow(var Message: TCMHintShow);
var
  Handled: boolean;
  i: integer;
  n: TTreeNode;
begin
  if PtInRect(ClientRect, Point(mx, my)) and ShowHint and not Dragging then
  begin
    n := GetNodeAt(mx, my);
    if n <> nil then
    begin
      if not n.HasChildren then
        i := n.Parent.Index
      else
        i := n.Index;
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
            if Assigned(FGetHint) then
              FGetHint(i, HintStr, Handled);
            if Handled then
              HintStr := FormatHint(HintStr, Colors[i].Value)
            else
              HintStr := Colors[i].Name;
          end;
    end;
  end;
  inherited;
end;

function TmbColorTree.ColorCount: integer;
begin
  Result := Length(Colors);
end;

function TmbColorTree.CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean;
begin
  Result := true;
  if Length(Colors) = 0 then Exit;
  if Node.HasChildren then
    DrawColorItem(Node.DisplayRect(false), cdsSelected in State, node.Index, node.Text, node.Expanded)
  else
    DrawInfoItem(Node.DisplayRect(false), node.Parent.Index);
end;

procedure TmbColorTree.DeleteColorByValue(AValue: TColor; All: boolean);
var
  i: integer;
begin
  for i := Length(Colors) - 1 downto 0 do
    if Colors[i].Value = AValue then
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

procedure TmbColorTree.DoArrow(c: TCanvas; dir: TScrollDirection; p: TPoint;
  sel: boolean);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    b.Height := 12;
    b.Width := 12;
    if Sel then
    begin
      b.Canvas.Brush.Color := clHighlight;
      b.Canvas.Pen.Color := clHighlightText;
    end
    else
    begin
      b.Canvas.Brush.Color := clFuchsia;
      b.Canvas.Pen.Color := clWindowText;
      b.Transparent := true;
      b.TransparentColor := clFuchsia;
    end;
    b.Canvas.FillRect(B.Canvas.ClipRect);
    case dir of
      sdDown  : DrawArrow(b.Canvas, dir, Point(2, 3), 3);
      sdRight : DrawArrow(b.Canvas, dir, Point(1, 2), 3);
    end;
    c.Draw(p.x, p.y, b);
  finally
    b.Free;
  end;
end;

procedure TmbColorTree.DrawColorItem(R: TRect; Selected: boolean; AIndex: integer;
  AItemText: string; Expanded: boolean);
const
  FLAGS = DT_LEFT or DT_NOCLIP or DT_END_ELLIPSIS;
var
  SR, TR: TRect;
begin
  with Canvas do
  begin
    //background
    Pen.Color := clWindow;
    if Selected then
      Brush.Color := clHighlight
    else
      Brush.Color := Color;
    FillRect(R);
    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Right, R.Bottom - 1);

    //swatches
    SR := Rect(R.Left + 6, R.Top + 6, R.Left + 42, R.Top + 42);
    Brush.Color := Self.Colors[AIndex].value;
    if Selected then
    begin
      if ThemeServices.ThemesEnabled then
      begin
        ThemeServices.DrawElement(Canvas.Handle,
          ThemeServices.GetElementDetails(teEditTextNormal), SR);
        InflateRect(SR, -2, -2);
        Brush.Color := Blend(Self.Colors[AIndex].value, clBlack, 80);
        FillRect(SR);
        InflateRect(SR, -1, -1);
        Brush.Color := Blend(Self.Colors[AIndex].value, clBlack, 90);
        FillRect(SR);
        InflateRect(SR, -1, -1);
        Brush.Color := Self.Colors[AIndex].value;
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
        Brush.Color := Blend(Self.Colors[AIndex].value, clBlack, 75);
        FillRect(SR);
        InflateRect(SR, -1, -1);
        Brush.Color := Blend(Self.Colors[AIndex].value, clBlack, 87);
        FillRect(SR);
        InflateRect(SR, -1, -1);
        Brush.Color := Self.Colors[AIndex].value;
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
        Brush.Color := Self.Colors[AIndex].value;
        FillRect(SR);
      end
      else
      //windows 9x
      begin
        DrawEdge(Canvas.Handle, SR, BDR_SUNKENOUTER, BF_RECT);
        InflateRect(SR, -2, -2);
        Brush.Color := Self.Colors[AIndex].value;
        Pen.Color := clBlack;
        Rectangle(SR);
        InflateRect(SR, -1, -1);
        FillRect(SR);
        InflateRect(SR, 1, 1);
      end;
    end;
    //names
    Font.Style := [fsBold];
    if Selected then
    begin
      //Brush.Color := clHighlightText;
      Pen.Color := clHighlightText;
      Font.Color := clHighlightText;
    end
    else
    begin
      //Brush.Color := clWindowText;
      Pen.Color := clWindowText;
      Font.Color := clWindowText;
    end;
    TR := Rect(R.Left + 48, R.Top + (48 - TextHeight(AItemText)) div 2, R.Right - 15, R.Bottom);
    if Assigned(FDraw) then FDraw(Self, AIndex, Canvas.Font, AItemText, Selected);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas.Handle, PChar(AItemText), Length(AItemText), TR, FLAGS);
    SetBkMode(Canvas.Handle, OPAQUE);
    if R.Right > 60 then
    begin
      if Expanded then
        DoArrow(Canvas, sdDown, Point(R.Right - 13, R.Top + 20), selected)
      else
        DoArrow(Canvas, sdRight, Point(R.Right - 10, R.Top + 18), selected);
    end;
  end;
end;

procedure TmbColorTree.DrawInfoItem(R: TRect; Index: integer);
const
  FLAGS = DT_LEFT or DT_END_ELLIPSIS or DT_NOCLIP;
  DELTA = 2;
var
  b: TBitmap;
  BR, TR: TRect;
  s: string;
  h: Integer;
begin
  b := TBitmap.Create;
  try
    b.Width := R.Right - R.Left;
    b.Height := R.Bottom - R.Top;
    BR := b.Canvas.ClipRect;
    with b.Canvas do
    begin
      Canvas.Font.Assign(Self.Font);
      Brush.Color := Blend(clBtnFace, clWindow, 30);
      FillRect(BR);
      BR := Rect(BR.Left + 42, BR.Top, BR.Right, BR.Bottom);
      FillRect(BR);
      Inc(BR.Left, 6);
      Font.Style := [];
      Font.Size := 7;

      s := FInfoLabel;
      h := TextHeight(s);
      TR := Rect(BR.Left, BR.Top{ + 2}, BR.Right, BR.Top + {2 + }h + DELTA);
      if Assigned(FDraw1) then FDraw1(Self, Index, Canvas.Font, s);
      DrawText(b.Canvas.Handle, PChar(s), Length(s), TR, FLAGS);

      DrawHorDottedLine(b.Canvas, BR.Left, BR.Right, TR.Bottom + DELTA, clGray);

      s := FormatHint(FInfo1, Self.Colors[Index].value);
      TR.Top := TR.Bottom + 2 * DELTA;
      TR.Bottom := TR.Top + h + DELTA;
      if Assigned(FDraw2) then FDraw2(Self, Index, Canvas.Font, s);
      DrawText(b.Canvas.Handle, PChar(s), Length(s), TR, FLAGS);

      DrawHorDottedLine(b.Canvas, BR.LEft, BR.Right, TR.Bottom + DELTA, clGray);

      s := FormatHint(FInfo2, Self.Colors[Index].value);
      TR.Top := TR.Bottom + 2 * DELTA;
      TR.Bottom := TR.Top + h + DELTA;
      if Assigned(FDraw3) then FDraw3(Self, Index, Canvas.Font, s);
        DrawText(b.Canvas.Handle, PChar(s), Length(s), TR, FLAGS);
    end;

    Canvas.Draw(R.Left, R.Top, b);
  finally
    b.Free;
  end;
end;

procedure TmbColorTree.InsertColor(AIndex: integer; AName: string; AValue: TColor);
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

function TmbColorTree.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  Result := true;
end;

procedure TmbColorTree.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  r: TRect;
begin
  inherited;
  if (ssShift in Shift) or (ssCtrl in Shift) then
    Exit;
  if Selected <> nil then
    r := Selected.DisplayRect(false)
  else
    exit;
  if (x > r.Right - 15) and (x < r.Right - 3) and (y > r.Top + 13) and (y < r.Top + 30) then
    if (Selected.HasChildren) and PtInRect(r, Point(x, y)) then
    begin
      if selected.Expanded then
        Selected.Collapse(false)
      else
        Selected.Expand(false);
      Invalidate;
    end;
end;

procedure TmbColorTree.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
begin
  inherited;
  mx := x;
  my := y;
  if GetNodeAt(x, y) <> nil then
    r := GetNodeAt(x, y).DisplayRect(false)
  else
  begin
    Cursor := crDefault;
    exit;
  end;

  if (x > r.Right - 15) and (x < r.Right - 3) and (y > r.Top + 13) and (y < r.Top + 30) then
  begin
    if (GetNodeAt(x, y).HasChildren) and PtInRect(r, Point(x, y)) then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end
  else
    Cursor := crDefault;
end;

procedure TmbColorTree.SetInfoLabel(Value: string);
begin
  if FInfoLabel <> Value then
  begin
    FInfoLabel := Value;
    Invalidate;
  end;
end;

procedure TmbColorTree.SetInfo1(Value: string);
begin
  if FInfo1 <> Value then
  begin
    FInfo1 := Value;
    Invalidate;
  end;
end;

procedure TmbColorTree.SetInfo2(Value: string);
begin
  if FInfo2 <> Value then
  begin
    FInfo2 := Value;
    Invalidate;
  end;
end;

procedure TmbColorTree.DeleteColor(AIndex: integer; ARefresh: boolean = true);
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

procedure TmbColorTree.DeleteColorByName(AName: string; All: boolean);
var
  i: integer;
begin
  for i := Length(Colors) - 1 downto 0 do
  if SameText(Colors[i].Name, AName) then
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

procedure TmbColorTree.UpdateColors;
var
  i: integer;
  n: TTreeNode;
begin
  Items.Clear;
  for i := 0 to Length(Colors) - 1 do
  begin
    n := Items.Add(TopItem, Colors[i].name);
    Items.AddChild(n, '');
  end;
end;

end.
