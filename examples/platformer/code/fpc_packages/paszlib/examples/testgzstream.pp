program example;

uses
  SysUtils, ZStream, Classes;

procedure TestGZip;
var
  SS: TStringStream;
  CS: TGZipCompressionStream;
  FS: TFileStream;
begin
  SS := TStringStream.Create('hello, hello!');
  FS := TFileStream.Create(GetTempDir+'test.gz', fmCreate);
  CS := TGZipCompressionStream.Create(FS);
  CS.CopyFrom(SS, 0);
  CS.Free;
  FS.Free;
  SS.Free;
end;

procedure TestGUnzip;
var
  FS: TFileStream;
  DS: TGZipDecompressionStream;
  SS: TStringStream;
begin
  FS := TFileStream.Create(GetTempDir+'test.gz', fmOpenRead);
  DS := TGZipDecompressionStream.Create(FS);
  SS := TStringStream.Create('');
  SS.CopyFrom(DS, 0);
  DS.Free;
  FS.Free;
  WriteLn(SS.DataString);
  SS.Free;
end;

begin
  TestGZip;
  TestGUnZip;
end. 

