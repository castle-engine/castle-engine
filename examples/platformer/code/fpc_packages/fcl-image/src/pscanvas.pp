{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TPostScriptCanvas implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ ---------------------------------------------------------------------
  This code is heavily based on Tony Maro's initial TPostScriptCanvas
  implementation in the LCL, but was adapted to work with the custom
  canvas code and to work with streams instead of strings.
  ---------------------------------------------------------------------}


{$mode objfpc}
{$H+}

unit pscanvas;

interface

uses
  Classes, SysUtils,fpimage,fpcanvas;

type
  TPostScript = class;

  TPSPaintType = (ptColored, ptUncolored);
  TPSTileType = (ttConstant, ttNoDistortion, ttFast);
  TPostScriptCanvas = class; // forward reference

  {Remember, modifying a pattern affects that pattern for the ENTIRE document!}
  TPSPattern = class(TFPCanvasHelper)
  private
    FStream : TMemoryStream;
    FPatternCanvas : TPostScriptCanvas;
    FOldName: String;
    FOnChange: TNotifyEvent;
    FBBox: TRect;
    FName: String;
    FPaintType: TPSPaintType;
    FPostScript: TStringList;
    FTilingType: TPSTileType;
    FXStep: Real;
    FYStep: Real;
    function GetpostScript: TStringList;
    procedure SetBBox(const AValue: TRect);
    procedure SetName(const AValue: String);
    procedure SetPaintType(const AValue: TPSPaintType);
    procedure SetTilingType(const AValue: TPSTileType);
    procedure SetXStep(const AValue: Real);
    procedure SetYStep(const AValue: Real);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    property BBox: TRect read FBBox write SetBBox;
    property PaintType: TPSPaintType read FPaintType write SetPaintType;
    property TilingType: TPSTileType read FTilingType write SetTilingType;
    property XStep: Real read FXStep write SetXStep;
    property YStep: Real read FYStep write SetYStep;
    property Name: String read FName write SetName;
    property GetPS: TStringList read GetPostscript;
    property OldName: string read FOldName write FOldName; // used when notifying that name changed
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    Property PatternCanvas : TPostScriptCanvas Read FPatternCanvas;
  end;
  PPSPattern = ^TPSPattern; // used for array

  { Pen and brush object both right now...}
  TPSPen = class(TFPCustomPen)
  private
    FPattern: TPSPattern;
    procedure SetPattern(const AValue: TPSPattern);
  public
    destructor Destroy; override;
    property Pattern: TPSPattern read FPattern write SetPattern;
    function AsString: String;
  end;

  TPSBrush = Class(TFPCustomBrush)
  Private
    Function GetAsString : String;
  Public
    Property AsString : String Read GetAsString;
  end;

  TPSFont = Class(TFPCustomFont)
  end;

  { Custom canvas-like object that handles postscript code }
  TPostScriptCanvas = class(TFPCustomCanvas)
  private
    FHeight,FWidth : Integer;
    FStream : TStream;
    FLineSpacing: Integer;
    LastX: Integer;
    LastY: Integer;
    function TranslateY(Ycoord: Integer): Integer; // Y axis is backwards in postscript
    procedure AddFill;
    procedure ResetPos; // reset back to last moveto location
    procedure SetWidth (AValue : integer); override;
    function  GetWidth : integer; override;
    procedure SetHeight (AValue : integer); override;
    function  GetHeight : integer; override;
  Protected
    Procedure WritePS(Const Cmd : String);
    Procedure WritePS(Const Fmt : String; Args : Array of Const);
    procedure DrawRectangle(const Bounds: TRect; DoFill : Boolean);
    procedure DrawEllipse(const Bounds: TRect; DoFill : Boolean);
  public
    constructor Create(AStream : TStream);
    destructor Destroy; override;
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    property LineSpacing: Integer read FLineSpacing write FLineSpacing;
    Procedure DoMoveTo(X1,Y1 : Integer); override;
    Procedure DoLineTo(X1,Y1 : Integer); override;
    Procedure DoLine(X1,Y1,X2,Y2 : Integer); override;
    Procedure DoRectangle(Const Bounds : TRect); override;
    Procedure DoRectangleFill(Const Bounds : TRect); override;
    procedure DoPolyline(Const Points: Array of TPoint); override;
    procedure DoEllipse(const Bounds: TRect); override;
    procedure DoEllipseFill(const Bounds: TRect); override;
    procedure DoPie(x,y,awidth,aheight,angle1,angle2 : Integer);
    //procedure Pie(x,y,width,height,SX,SY,EX,EY : Integer);
    procedure Writeln(AString: String);
    procedure TextOut(X,Y: Integer; const Text: String);
    //procedure Chord(x,y,width,height,angle1,angle2 : Integer);
    //procedure Chord(x,y,width,height,SX,SY,EX,EY : Integer);
    //procedure PolyBezier(Points: PPoint; NumPts: Integer;
    //                     Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                     Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //procedure PolyBezier(const Points: array of TPoint;
    //                     Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                     Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //procedure PolyBezier(const Points: array of TPoint);
    //procedure Polygon(const Points: array of TPoint;
    //                  Winding: Boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                  StartIndex: Integer{$IFDEF VER1_1} = 0{$ENDIF};
    //                  NumPts: Integer {$IFDEF VER1_1} = -1{$ENDIF});
    //procedure Polygon(Points: PPoint; NumPts: Integer;
    //                  Winding: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //Procedure Polygon(const Points: array of TPoint);
    //Procedure FillRect(const Rect : TRect);
    //procedure FloodFill(X, Y: Integer; FillColor: TFPColor; FillStyle: TFillStyle);
    //Procedure RoundRect(X1, Y1, X2, Y2: Integer; RX,RY : Integer);
    //Procedure RoundRect(const Rect : TRect; RX,RY : Integer);
    Property Stream : TStream read FStream;
  end;

  { Encapsulates ALL the postscript and uses the TPostScriptCanvas object for a single page }
  TPostScript = class(TComponent)
  private
    FDocStarted : Boolean;
    FCreator : String;
    FStream : TStream;
    FCanvas: TPostScriptCanvas;
    FHeight: Integer;
    FLineSpacing: Integer;
    FPageNumber: Integer;
    FTitle: String;
    FWidth: Integer;
    FPatterns: TList;   // array of pointers to pattern objects
    procedure SetHeight(const AValue: Integer);
    procedure SetLineSpacing(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    procedure UpdateBoundingBox;
    procedure PatternChanged(Sender: TObject);
    procedure InsertPattern(APattern: TPSPattern); // adds the pattern to the postscript
    Procedure SetStream (Value : TStream);
    Function GetCreator : String;
  Protected
    Procedure WritePS(Const Cmd : String);
    Procedure WritePS(Const Fmt : String; Args : Array of Const);
    Procedure WriteDocumentHeader; virtual;
    Procedure WriteStandardFont; virtual;
    Procedure WritePage; virtual;
    Procedure FreePatterns;
    Procedure CheckStream;
  public
    Constructor Create(AOwner : TComponent);
    destructor Destroy; override;

    procedure AddPattern(APSPattern: TPSPattern);
    function FindPattern(AName: String): TPSPattern;
    function DelPattern(AName: String): Boolean;
    function NewPattern(AName: String): TPSPattern;
    property Canvas: TPostScriptCanvas read FCanvas;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property PageNumber: Integer read FPageNumber;
    property Title: String read FTitle write FTitle;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing;
    procedure BeginDoc;
    procedure NewPage;
    procedure EndDoc;
    Property Stream : TStream Read FStream Write SetStream;
    Property Creator : String Read GetCreator Write FCreator;
  end;

implementation

Resourcestring
  SErrNoStreamAssigned = 'Invalid operation: No stream assigned';
  SErrDocumentAlreadyStarted = 'Cannot start document twice.';


{ TPostScriptCanvas ----------------------------------------------------------}

Procedure TPostScriptCanvas.WritePS(const Cmd : String);
var
  ss : shortstring;
begin
  If length(Cmd)>0 then
    FStream.Write(Cmd[1],Length(Cmd));
  ss:=LineEnding;
  FStream.Write(ss[1],Length(ss));
end;

Procedure TPostScriptCanvas.WritePS(Const Fmt : String; Args : Array of Const);

begin
  WritePS(Format(Fmt,Args));
end;

{ Y coords in postscript are backwards... }
function TPostScriptCanvas.TranslateY(Ycoord: Integer): Integer;
begin
  Result:=Height-Ycoord;
end;

{ Adds a fill finishing line to any path we desire to fill }
procedure TPostScriptCanvas.AddFill;
begin
  WritePs('gsave '+(Brush as TPSBrush).AsString+' fill grestore');
end;

{ Return to last moveto location }
procedure TPostScriptCanvas.ResetPos;
begin
  WritePS(inttostr(LastX)+' '+inttostr(TranslateY(LastY))+' moveto');
end;

constructor TPostScriptCanvas.Create(AStream : TStream);

begin
  inherited create;
  FStream:=AStream;
  Height := 792; // length of page in points at 72 ppi
  { // Choose a standard font in case the user doesn't
  FFontFace := 'AvantGarde-Book';
  SetFontSize(10);
    FLineSpacing := MPostScript.LineSpacing;
  end;
  FPen := TPSPen.Create;
  FPen.Width := 1;
  FPen.FPColor := 0;
  FPen.OnChange := @PenChanged;

  FBrush := TPSPen.Create;
  FBrush.Width := 1;
  FBrush.FPColor := -1;
  // don't notify us that the brush changed...
  }
end;

destructor TPostScriptCanvas.Destroy;
begin
{
  FPostScript.Free;
  FPen.Free;
  FBrush.Free;
}
  inherited Destroy;
end;

procedure TPostScriptCanvas.SetWidth (AValue : integer);

begin
  FWidth:=AValue;
end;

function  TPostScriptCanvas.GetWidth : integer;

begin
  Result:=FWidth;
end;

procedure TPostScriptCanvas.SetHeight (AValue : integer);

begin
  FHeight:=AValue;
end;

function  TPostScriptCanvas.GetHeight : integer;

begin
  Result:=FHeight;
end;


{ Move draw location }
procedure TPostScriptCanvas.DoMoveTo(X1, Y1: Integer);

var
  Y: Integer;

begin
  Y := TranslateY(Y1);
  WritePS(inttostr(X1)+' '+inttostr(Y)+' moveto');
  LastX := X1;
  LastY := Y1;
end;

{ Draw a line from current location to these coords }
procedure TPostScriptCanvas.DoLineTo(X1, Y1: Integer);

var
   Y: Integer;

begin
  Y := TranslateY(Y1);
  WritePS(inttostr(X1)+' '+inttostr(Y)+' lineto');
  LastX := X1;
  LastY := Y1;
end;

procedure TPostScriptCanvas.DoLine(X1, Y1, X2, Y2: Integer);
var
  Y12, Y22: Integer;

begin
  Y12 := TranslateY(Y1);
  Y22 := TranslateY(Y2);
  WritePS('newpath '+inttostr(X1)+' '+inttostr(Y12)+' moveto '+
          inttostr(X2)+' '+inttostr(Y22)+' lineto closepath stroke');
  // go back to last moveto position
  ResetPos;
end;

{ Draw a rectangle }

procedure TPostScriptCanvas.DoRectangleFill(const Bounds: TRect);

begin
  DrawRectangle(Bounds,true)
end;

procedure TPostScriptCanvas.DoRectangle(const Bounds: TRect);

begin
  DrawRectangle(Bounds,False);
end;

procedure TPostScriptCanvas.DrawRectangle(const Bounds: TRect; DoFill : Boolean);

var
   Y12, Y22: Integer;

begin
  Y12 := TranslateY(Bounds.Top);
  Y22 := TranslateY(Bounds.Bottom);
  WritePS('stroke newpath');
  With Bounds do
    begin
    WritePS(inttostr(Left)+' '+inttostr(Y12)+' moveto');
    WritePS(inttostr(Right)+' '+inttostr(Y12)+' lineto');
    WritePS(inttostr(Right)+' '+inttostr(Y22)+' lineto');
    WritePS(inttostr(Left)+' '+inttostr(Y22)+' lineto');
    end;
  WritePS('closepath');
  If DoFill and (Brush.Style<>bsClear) then
    AddFill;
  WritePS('stroke');
  ResetPos;
end;

{ Draw a series of lines }
procedure TPostScriptCanvas.DoPolyline(Const Points: Array of TPoint);
var
  i : Longint;
begin
  MoveTo(Points[0].X, Points[0].Y);
  For i := 1 to High(Points) do
    LineTo(Points[i].X, Points[i].Y);
  ResetPos;
end;

{ This was a pain to figure out... }

procedure TPostScriptCanvas.DoEllipse(Const Bounds : TRect);

begin
  DrawEllipse(Bounds,False);
end;

procedure TPostScriptCanvas.DoEllipseFill(Const Bounds : TRect);

begin
  DrawEllipse(Bounds,true);
end;

procedure TPostScriptCanvas.DrawEllipse(Const Bounds : TRect; DoFill : Boolean);

var
  radius: Integer;
  YRatio: Real;
  centerX, centerY: Integer;

begin
  // set radius to half the width
  With Bounds do
    begin
    radius := (Right-Left) div 2;
    if radius <1 then
      exit;
    YRatio := (Bottom - Top) / (Right-Left);
    // find center
    CenterX := (Right+Left) div 2;
    CenterY := (Top+Bottom) div 2;
    end;
  WritePS('newpath '+inttostr(CenterX)+' '+inttostr(TranslateY(CenterY))+' translate');
  // move to edge
  WritePS(inttostr(radius)+' 0 moveto');
  // now draw it
  WritePS('gsave 1 '+format('%.3f',[YRatio])+' scale');
  WritePS('0 0 '+inttostr(radius)+' 0 360 arc');
  if DoFill and (Brush.Style<>bsClear) then
    AddFill;
  // reset scale for drawing line thickness so it doesn't warp
  YRatio := 1 / YRatio;
  WritePS('1 '+format('%.2f',[YRatio])+' scale stroke grestore');
  // move origin back
  WritePS(inttostr(-CenterX)+' '+inttostr(-TranslateY(CenterY))+' translate closepath stroke');
  ResetPos;
end;

procedure TPostScriptCanvas.DoPie(x, y, AWidth, AHeight, angle1, angle2: Integer);
begin
  // set zero at center
  WritePS('newpath '+inttostr(X)+' '+inttostr(TranslateY(Y))+' translate');
  // scale it
  WritePS('gsave '+inttostr(AWidth)+' '+inttostr(Aheight)+' scale');
  //WritePS('gsave 1 1 scale');
  // draw line to edge
  WritePS('0 0 moveto');
  WritePS('0 0 1 '+inttostr(angle1)+' '+inttostr(angle2)+' arc closepath');
  if Brush.Style<>bsClear then
    AddFill;
  // reset scale so we don't change the line thickness
  // adding 0.01 to compensate for scaling error - there may be a deeper problem here...
  WritePS(format('%.6f',[(real(1) / X)+0.01])+' '+format('%.6f',[(real(1) / Y)+0.01])+' scale stroke grestore');
  // close out and return origin
  WritePS(inttostr(-X)+' '+inttostr(-TranslateY(Y))+' translate closepath stroke');
  resetpos;
end;

{ Writes text with a carriage return }
procedure TPostScriptCanvas.Writeln(AString: String);
begin
  TextOut(LastX, LastY, AString);
  LastY := LastY+Font.Size+FLineSpacing;
  MoveTo(LastX, LastY);
end;


{ Output text, restoring draw location }
procedure TPostScriptCanvas.TextOut(X, Y: Integer; const Text: String);
var
   Y1: Integer;
begin
  Y1 := TranslateY(Y);
  WritePS(inttostr(X)+' '+inttostr(Y1)+' moveto');
  WritePS('('+Text+') show');
  ResetPos; // move back to last moveto location
end;

function TPostScriptCanvas.DoCreateDefaultFont : TFPCustomFont;

begin
  Result:=TPSFont.Create;
end;


function TPostScriptCanvas.DoCreateDefaultPen : TFPCustomPen;

begin
  Result:=TPSPen.Create;
end;

function TPostScriptCanvas.DoCreateDefaultBrush : TFPCustomBrush;

begin
  Result:=TPSBrush.Create;
end;



{ TPostScript -------------------------------------------------------------- }

procedure TPostScript.SetHeight(const AValue: Integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
  UpdateBoundingBox;
  // filter down to the canvas height property
  if assigned(FCanvas) then
    FCanvas.Height := FHeight;
end;

procedure TPostScript.SetLineSpacing(const AValue: Integer);
begin
  if FLineSpacing=AValue then exit;
  FLineSpacing:=AValue;
  // filter down to the canvas
  if assigned(FCanvas) then FCanvas.LineSpacing := AValue;
end;

procedure TPostScript.SetWidth(const AValue: Integer);
begin
  if FWidth=AValue then exit;
    FWidth:=AValue;
  UpdateBoundingBox;
end;

{ Take our sizes and change the boundingbox line }
procedure TPostScript.UpdateBoundingBox;
begin
{

     // need to not hard-link this to line 1
     FDocument[1] := '%%BoundingBox: 0 0 '+inttostr(FWidth)+' '+inttostr(FHeight);
}
end;

{ Pattern changed so update the postscript code }
procedure TPostScript.PatternChanged(Sender: TObject);
begin
     // called anytime a pattern changes.  Update the postscript code.
     // look for and delete the current postscript code for this pattern
     // then paste the pattern back into the code before the first page
     InsertPattern(Sender As TPSPattern);
end;

{ Places a pattern definition into the bottom of the header in postscript }
procedure TPostScript.InsertPattern(APattern: TPSPattern);
var
   I, J: Integer;
   MyStrings: TStringList;
begin
{     I := 0;
     if FDocument.Count < 1 then begin
        // added pattern when no postscript exists - this shouldn't happen
        raise exception.create('Pattern inserted with no postscript existing');
        exit;
     end;

     for I := 0 to FDocument.count - 1 do begin
         if (FDocument[I] = '%%Page: 1 1') then begin
            // found it!
            // insert into just before that
            MyStrings := APattern.GetPS;
            for J := 0 to MyStrings.Count - 1 do begin
                FDocument.Insert(I-1+J, MyStrings[j]);
            end;
            exit;
         end;
     end;
}
end;

constructor TPostScript.Create(AOwner : TComponent);
begin
  inherited create(AOwner);
  // Set some defaults
  FHeight := 792; // 11 inches at 72 dpi
  FWidth := 612; // 8 1/2 inches at 72 dpi
end;

Procedure TPostScript.WritePS(const Cmd : String);
var
  ss : shortstring;
begin
  If length(Cmd)>0 then
    FStream.Write(Cmd[1],Length(Cmd));
  ss:=LineEnding;
  FStream.Write(ss[1],Length(ss));
end;

Procedure TPostScript.WritePS(Const Fmt : String; Args : Array of Const);

begin
  WritePS(Format(Fmt,Args));
end;

Procedure TPostScript.WriteDocumentHeader;

begin
  WritePS('%!PS-Adobe-3.0');
  WritePS('%%BoundingBox: 0 0 612 792');
  WritePS('%%Creator: '+Creator);
  WritePS('%%Title: '+FTitle);
  WritePS('%%Pages: (atend)');
  WritePS('%%PageOrder: Ascend');
  WriteStandardFont;
end;

Procedure TPostScript.WriteStandardFont;

begin
  // Choose a standard font in case the user doesn't
  WritePS('/AvantGarde-Book findfont');
  WritePS('10 scalefont');
  WritePS('setfont');
end;

Procedure TPostScript.FreePatterns;

Var
  i : Integer;

begin
  If Assigned(FPatterns) then
    begin
    For I:=0 to FPatterns.Count-1 do
      TObject(FPatterns[i]).Free;
    FreeAndNil(FPatterns);
    end;
end;

destructor TPostScript.Destroy;

begin
  Stream:=Nil;
  FreePatterns;
  inherited Destroy;
end;

{ add a pattern to the array }
procedure TPostScript.AddPattern(APSPattern: TPSPattern);
begin
  If Not Assigned(FPatterns) then
    FPatterns:=Tlist.Create;
  FPatterns.Add(APSPattern);
end;

{ Find a pattern object by it's name }

function TPostScript.FindPattern(AName: String): TPSPattern;

var
   I: Integer;

begin
  Result := nil;
  If Assigned(FPatterns) then
    begin
    I:=Fpatterns.Count-1;
    While (Result=Nil) and (I>=0) do
      if TPSPattern(FPatterns[I]).Name = AName then
        result := TPSPattern(FPatterns[i])
      else
        Dec(i)
   end;
end;

function TPostScript.DelPattern(AName: String): Boolean;
begin
  // can't do that yet...
  Result:=false;
end;


{ Create a new pattern and inserts it into the array for safe keeping }
function TPostScript.NewPattern(AName: String): TPSPattern;
var
   MyPattern: TPSPattern;
begin
  MyPattern := TPSPattern.Create;
  AddPattern(MyPattern);
  MyPattern.Name := AName;
  MyPattern.OnChange := @PatternChanged;
  MyPattern.OldName := '';
  // add this to the postscript now...
  InsertPattern(MyPattern);
  result := MyPattern;
end;

{ Start a new document }
procedure TPostScript.BeginDoc;

var
   I: Integer;

begin
  CheckStream;
  If FDocStarted then
    Raise Exception.Create(SErrDocumentAlreadyStarted);
  FCanvas:=TPostScriptCanvas.Create(FStream);
  FCanvas.Height:=Self.Height;
  FCanvas.Width:=Self.width;
  FreePatterns;
  WriteDocumentHeader;
  // start our first page
  FPageNumber := 1;
  WritePage;
  UpdateBoundingBox;
end;

Procedure TPostScript.WritePage;

begin
  WritePS('%%Page: '+inttostr(FPageNumber)+' '+inttostr(FPageNumber));
  WritePS('newpath');
end;

{ Copy current page into the postscript and start a new one }
procedure TPostScript.NewPage;
begin
  // dump the current page into our postscript first
  // put end page definition...
  WritePS('stroke');
  WritePS('showpage');
  FPageNumber := FPageNumber+1;
  WritePage;
end;

{ Finish off the document }
procedure TPostScript.EndDoc;
begin
  // Start printing the document after closing out the pages
  WritePS('stroke');
  WritePS('showpage');
  WritePS('%%Pages: '+inttostr(FPageNumber));
  // okay, the postscript is all ready, so dump it to the text file
  // or to the printer
  FDocStarted:=False;
  FreeAndNil(FCanvas);
end;

Function TPostScript.GetCreator : String;

begin
  If (FCreator='') then
    Result:=ClassName
  else
    Result:=FCreator;
end;


Procedure TPostScript.SetStream (Value : TStream);

begin
  if (FStream<>Value) then
    begin
    If (FStream<>Nil) and FDocStarted then
      EndDoc;
    FStream:=Value;
    FDocStarted:=False;
    end;
end;

Procedure TPostScript.CheckStream;

begin
  If Not Assigned(FStream) then
    Raise Exception.Create(SErrNoStreamAssigned);
end;

{ TPSPen }

procedure TPSPen.SetPattern(const AValue: TPSPattern);
begin
  if FPattern<>AValue then
    begin
    FPattern:=AValue;
    // NotifyCanvas;
    end;
end;


destructor TPSPen.Destroy;
begin
  // Do NOT free the pattern object from here...
  inherited Destroy;
end;


{ Return the pen definition as a postscript string }
function TPSPen.AsString: String;

begin
  Result:='';
  if FPattern <> nil then
    begin
    if FPattern.PaintType = ptColored then
      Result:='/Pattern setcolorspace '+FPattern.Name+' setcolor '
    else
      begin
      Result:='[/Pattern /DeviceRGB] setcolorspace '+inttostr(FPColor.Red)+' '+inttostr(FPColor.Green)+' '+
       inttostr(FPColor.Blue)+' '+FPattern.Name+' setcolor ';
      end;
    end
  else // no pattern do this:
    Result:=inttostr(FPColor.Red)+' '+inttostr(FPColor.Green)+' '+
           inttostr(FPColor.Blue)+' setrgbcolor ';
  Result := Result + format('%f',[Width])+' setlinewidth ';
end;

{ TPSPattern }

{ Returns the pattern definition as postscript }
function TPSPattern.GetpostScript: TStringList;

var
   I: Integer;
   S : String;

begin
  // If nothing in the canvas, error
  if FStream.Size=0 then
    raise exception.create('Empty pattern');
  FPostScript.Clear;
  With FPostScript do
    begin
    add('%% PATTERN '+FName);
    add('/'+FName+'proto 12 dict def '+FName+'proto begin');
    add('/PatternType 1 def');
    add(Format('/PaintType %d def',[ord(FPaintType)+1]));
    add(Format('/TilingType %d def',[ord(FTilingType)+1]));
    add('/BBox ['+inttostr(FBBox.Left)+' '+inttostr(FBBox.Top)+' '+inttostr(FBBox.Right)+' '+inttostr(FBBox.Bottom)+'] def');
    add('/XStep '+format('%f',[FXStep])+' def');
    add('/YStep '+format('%f',[FYstep])+' def');
    add('/PaintProc { begin');
    // insert the canvas
    SetLength(S,FStream.Size);
    FStream.Seek(0,soFromBeginning);
    FStream.Read(S[1],FStream.Size);
    Add(S);
    // add support for custom matrix later
    add('end } def end '+FName+'proto [1 0 0 1 0 0] makepattern /'+FName+' exch def');
    add('%% END PATTERN '+FName);
    end;
  Result := FPostScript;
end;

procedure TPSPattern.SetBBox(const AValue: TRect);
begin
{  if FBBox<>AValue then
    begin
    FBBox:=AValue;
    FPatternCanvas.Height := FBBox.Bottom - FBBox.Top;
//    NotifyCanvas;
    end;
}
end;

procedure TPSPattern.SetName(const AValue: String);
begin
  FOldName := FName;
  if (FName<>AValue) then
    begin
    FName:=AValue;
    // NotifyCanvas;
    end;
end;

procedure TPSPattern.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPSPattern.SetPaintType(const AValue: TPSPaintType);
begin
  if FPaintType=AValue then exit;
  FPaintType:=AValue;
  changed;
end;

procedure TPSPattern.SetTilingType(const AValue: TPSTileType);
begin
  if FTilingType=AValue then exit;
  FTilingType:=AValue;
  changed;
end;

procedure TPSPattern.SetXStep(const AValue: Real);
begin
  if FXStep=AValue then exit;
  FXStep:=AValue;
  changed;
end;

procedure TPSPattern.SetYStep(const AValue: Real);
begin
  if FYStep=AValue then exit;
  FYStep:=AValue;
  changed;
end;

constructor TPSPattern.Create;
begin
  FPostScript := TStringList.Create;
  FPaintType := ptColored;
  FTilingType := ttConstant;
  FStream:=TmemoryStream.Create;
  FPatternCanvas := TPostScriptCanvas.Create(FStream);
  FName := 'Pattern1';
end;

destructor TPSPattern.Destroy;
begin
  FPostScript.Free;
  FPatternCanvas.Free;
  FStream.Free;
  inherited Destroy;
end;

{ ---------------------------------------------------------------------
    TPSBrush
  ---------------------------------------------------------------------}


Function TPSBrush.GetAsString : String;

begin
  Result:='';
end;



end.
