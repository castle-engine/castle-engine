unit DemoUtils;

{$I ImagingOptions.inc}

interface

uses
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging, 
  ImagingUtility;

const
  SDataDir = 'Data';
  SSourceDir = 'Source';


function ExpandFileTo(const FileName, BasePath: string): string;
function SwapPathDelims(const FileName: string; const NewDelim: string = PathDelim): string;

function GetDataDir: string;
function GetRootDir: string;

// Returns next valid image format.
function NextFormat(Format: TImageFormat): TImageFormat;

implementation

function ExpandFileTo(const FileName, BasePath: string): string;
var
 OldPath: string;
begin
  GetDir(0, OldPath);
  try
   if SysUtils.DirectoryExists(BasePath) then
   begin
     ChDir(BasePath);
     Result:= ExpandFileName(FileName);
   end
   else
    Result:=FileName;
  finally
   ChDir(OldPath);
  end;
end;

function SwapPathDelims(const FileName, NewDelim: string): string;
begin
  Result := FileName;
  Result := StringReplace(Result, '\', NewDelim, [rfReplaceAll]);
  Result := StringReplace(Result, '/', NewDelim, [rfReplaceAll]);
end;

function GetDataDir: string;
var
  Iter: Integer;
begin
  Iter := 0;
  Result := GetAppDir;
  while not DirectoryExists(Result + PathDelim + SDataDir) and (Iter < 7) do
  begin
    Result := ExtractFileDir(Result);
    Inc(Iter);
  end;
  Result := Result + PathDelim + SDataDir;
end;

function GetRootDir: string;
var
  Iter: Integer;
begin
  Iter := 0;
  Result := GetAppDir;
  while not DirectoryExists(Result + PathDelim + SSourceDir) and (Iter < 7) do
  begin
    Result := ExtractFileDir(Result);
    Inc(Iter);
  end;
end;

function NextFormat(Format: TImageFormat): TImageFormat;
var
  Info: TImageFormatInfo;
begin
  repeat
    if Format < High(TImageFormat) then
      Format := Succ(Format)
    else
      Format := ifIndex8;
  until GetImageFormatInfo(Format, Info);
  Result := Format;
end;

end.
