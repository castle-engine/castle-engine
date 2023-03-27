program createqrcode;

{$mode objfpc}
{$H+}

uses
  Classes, SysUtils, CustApp, fpimage, fpqrcodegen, fpimgqrcode,
  fpwritepng,fpwritebmp,fpwritexpm, FPWriteJPEG, FPWritePCX,
  FPWritePNM, FPWriteTIFF;

type

  { TCreateQRApplication }

  TCreateQRApplication = class(TCustomApplication)
  Private
    FText : UTF8String;
    Foutput : String;
    FGenerator : TImageQRCodeGenerator;
  protected

    function ParseOptions : Boolean;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;
    procedure WriteHelp(Msg : String); virtual;
  end;

{ TCreateQRApplication }

function TCreateQRApplication.ParseOptions: Boolean;

Var
  ErrorMsg : String;

begin
  Result:=False;
  ErrorMsg:=CheckOptions('b:ht:o:m:ep:', ['help','text:','output:','mask:','error-correctionlevel:','pixel-size','border']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    WriteHelp(ErrorMsg);
    Exit;
    end;
  FText:=GetOptionValue('t','text');
  FGenerator.PixelSize:=StrToIntDef(GetOptionValue('p','pixel-size'),4);
  FGenerator.Border:=StrToIntDef(GetOptionValue('b','border'),0);
  FOutput:=GetOptionValue('o','output');
  if Foutput='' then
    Foutput:='qrcode.png';
  Case LowerCase(GetOptionValue('e','error-correctionlevel')) of
    'low' : FGenerator.ErrorCorrectionLevel:=EccLOW;
    'high' : FGenerator.ErrorCorrectionLevel:=EccHigh;
    'medium' : FGenerator.ErrorCorrectionLevel:=EccMEDIUM;
    'quartile' : FGenerator.ErrorCorrectionLevel:=EccQUARTILE;
  else
    FGenerator.ErrorCorrectionLevel:=EccMEDIUM;
  end;
  Case LowerCase(GetOptionValue('m','mask')) of
    '0' : FGenerator.Mask:=mp0;
    '1' : FGenerator.Mask:=mp1;
    '2' : FGenerator.Mask:=mp2;
    '3' : FGenerator.Mask:=mp3;
    '4' : FGenerator.Mask:=mp4;
    '5' : FGenerator.Mask:=mp5;
    '6' : FGenerator.Mask:=mp6;
    '7' : FGenerator.Mask:=mp7;
  else
    FGenerator.Mask:=mpAuto;
  end;
  Result:=True;
end;

procedure TCreateQRApplication.DoRun;

begin
  Terminate;
  // quick check parameters
  if not ParseOptions then
     exit;
  FGenerator.Generate(FText);
  FGenerator.SaveToFile(Foutput);
  Terminate;
end;

constructor TCreateQRApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FGenerator:=TImageQRCodeGenerator.Create;

end;

destructor TCreateQRApplication.Destroy;
begin
  FreeAndNil(FGenerator);
  inherited Destroy;
end;

procedure TCreateQRApplication.WriteHelp(Msg : string);
begin
  if (Msg<>'') then
    Writeln('Error : ',Msg);
  writeln('Usage: ', ExeName, ' -h');
end;


var
  Application: TCreateQRApplication;
begin
  Application:=TCreateQRApplication.Create(nil);
  Application.Title:='Create QR code';
  Application.Run;
  Application.Free;
end.

