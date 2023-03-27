program fpzipper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, zipper;

type

  { TFPZipApplication }

  TFPZipApplication = class(TCustomApplication)
  Private
    FZipper: TZipper;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TFPZipApplication }

procedure TFPZipApplication.DoRun;

Var
  I : Integer;
  F : TFileStream;
begin
  If ParamCount<=1 then
    begin
    Writeln('Usage ',ParamStr(0),' zipfile file1 [file2 [...]]');
    Terminate;
    exit;
    end;
  FZipper.FileName:=ParamStr(1);
  For I:=2 to ParamCount do
    begin
    F:=TFileStream.Create(ParamStr(i),fmOpenRead);
    FZipper.Entries.AddFileEntry(F,ParamStr(i));
    end;
  FZipper.ZipAllFiles;
  For I:=0 to FZipper.Entries.Count-1 do
    FZipper.Entries[I].Stream.Free;
  Terminate;
end;

constructor TFPZipApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FZipper:=TZipper.Create;
end;

destructor TFPZipApplication.Destroy;
begin
  FreeAndNil(FZipper);
  inherited Destroy;
end;

procedure TFPZipApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TFPZipApplication;
begin
  Application:=TFPZipApplication.Create(nil);
  Application.Title:='Zip application';
  Application.Run;
  Application.Free;
end.

