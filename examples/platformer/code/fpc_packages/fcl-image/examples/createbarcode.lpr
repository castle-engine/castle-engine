program createbarcode;

{$mode objfpc}
{$H+}

uses
  Classes, SysUtils, CustApp, fpbarcode, fpimgbarcode, fpimage,
  fpwritepng, fpwritebmp,fpwritejpeg,FPWritePNM,fpwritexpm;

type

  { TCreateBarcodeApplication }

  TCreateBarcodeApplication = class(TCustomApplication)
  Private
    FWidth : Cardinal;
    FHeight : Cardinal;
    FUnit : Cardinal;
    FWeight : Double;
    FText : string;
    FFileName : String;
    FEncoding : TBarcodeEncoding;
    flist : Boolean;
  protected
    procedure DoRun; override;
    Procedure ListEncodings;
    Procedure CreateBarCode;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp(S: String); virtual;
    Procedure AnalyzeParams;
  end;

{ TCreateBarcodeApplication }

procedure TCreateBarcodeApplication.DoRun;

begin
  AnalyzeParams;
  if FList then
    ListEncodings
  else
    CreateBarCode;
  Terminate;
end;

procedure TCreateBarcodeApplication.ListEncodings;

Var
  E : TBarcodeEncoding;
  S : String;

begin
  Writeln('Known encodings : ');
  For E in TBarcodeEncoding do
    begin
    Str(E,S);
    Delete(S,1,2);
    Writeln(S:16,':  ',BarcodeEncodingNames[E]);
    end;
end;

procedure TCreateBarcodeApplication.CreateBarCode;

Var
  Img : TFPCustomImage;

begin
  Img:=TFPCompactImgGray16Bit.Create(FWidth,FHeight);
  try
    DrawBarCode(Img,FText,FEncoding,FUnit,FWeight);
    Writeln('Writing to file : ',FFilename);
    Img.SaveToFile(FFileName);
  finally
    Img.Free;
  end;
end;

constructor TCreateBarcodeApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

procedure TCreateBarcodeApplication.WriteHelp(S : String);
begin
  if (S<>'') then
    Writeln('Error : ',S);
  writeln('Usage: ', ExeName, ' -h');
  Free;
  Halt(Ord(S<>''));
end;

procedure TCreateBarcodeApplication.AnalyzeParams;

Var
  S,ES : String;
  E : TBarcodeEncoding;
  B : Boolean;

begin
  S:=CheckOptions('hw:h:t:o:lu:i:e:', ['help','width:','height:','text:','encoding','output:','list','unit:','weight:']);
  if (S<>'') or HasOption('h', 'help') then
    WriteHelp(S);
  if HasOption('h','height') then
    FHeight:=StrToInt(GetOptionValue('h','height'));
  if HasOption('w','width') then
    FWidth:=StrToInt(GetOptionValue('w','width'));
  if HasOption('u','unit') then
    FUnit:=StrToInt(GetOptionValue('u','unit'));
  if HasOption('i','weight') then
    FWeight:=StrToFloat(GetOptionValue('i','weight'));
  FText:=GetOptionValue('t','text');
  FFileName:=GetOptionValue('o','output');
  S:=GetOptionValue('e','encoding');
  FList:=HasOption('l','list');
  if FList then
     exit;
  // Sanitize
  if (S='') then
    WriteHelp('Need barcode encoding');
  if FText='' then
    WriteHelp('Need a text');
  E:=Low(TBarCodeEncoding);
  B:=False;
  While (Not B) and (E<=High(TBarcodeEncoding)) do
    begin
    Str(E,ES);
    delete(ES,1,2);
    B:=SameText(S,ES);
    if B then
      FEncoding:=E;
    E:=Succ(E);
    end;
  if not B then
    WriteHelp('Invalid barcode encoding: '+S);
  if FWidth=0 then
    FWidth:=200;
  if Fheight=0 then
    FHeight:=30;
  if FUnit=0 then
    FUnit:=1;
  if FWeight=0 then
    FWeight:=2.0;
end;

var
  Application: TCreateBarcodeApplication;
begin
  Application:=TCreateBarcodeApplication.Create(nil);
  Application.Title:='Create Barcodes';
  Application.Run;
  Application.Free;
end.

