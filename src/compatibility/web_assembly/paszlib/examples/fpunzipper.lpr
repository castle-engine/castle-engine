program fpzipper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, zipper
  { you can add units after this };

type

  { TFPUnZipApplication }

  TFPUnZipApplication = class(TCustomApplication)
  Private
    FUnZipper: TUnZipper;
    FFiles : TStrings;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFPUnZipApplication }

procedure TFPUnZipApplication.DoRun;

Var
  I : Integer;
  F : TFileStream;

begin
  If ParamCount<1 then
    begin
    Writeln('Usage ',ParamStr(0),' zipfile [file1 [file2 [...]]]');
    Terminate;
    exit;
    end;
  FUnZipper.FileName:=ParamStr(1);
  FUnZipper.UseUTF8:=True;
  FUnZipper.Examine;
  if ParamCount=1 then
    FUnZipper.UnZipAllFiles
  else
    For I:=2 to ParamCount do
      FFiles.Add(ParamStr(I));
  FUnZipper.UnZipFiles(FFiles);
  Terminate;
end;

constructor TFPUnZipApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FUnZipper:=TUnZipper.Create;
  FFiles:=TStringList.Create;
end;

destructor TFPUnZipApplication.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FUNZipper);
  inherited Destroy;
end;

procedure TFPUnZipApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TFPUnZipApplication;
begin
  Application:=TFPUnZipApplication.Create(nil);
  Application.Title:='UnZip application';
  Application.Run;
  Application.Free;
end.

