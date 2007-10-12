{
  Copyright 2002-2004,2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Operations on filenames (i.e. they don't TOUCH, even read, real files) }

{$ifdef read_interface}

{ Usuwa z filename ostatnie rozszerzenie (razem z kropka).
  Jesli plik ma kilka rozszerzen, obcina TYLKO ostatnie.
  Jesli plik nie mial rozszerzenia, nic nie obcina.
  Kiedys jako FilenameWithoutExt. }
function DeleteFileExt(const filename:string):string;

{ wyciaga z filename nazwe pliku bez dysku, katalogu i ostatniego rozszerzenia }
function ExtractOnlyFilename(const filename:string):string;

{ ChangeFilePath zwraca fname ze sciezka ustawiona na NewPath.
  (newPath musi zawierac koncowy PathDelim) }
function ChangeFilePath(const fname, NewPath:string):string;

{ podwaja w stringu znaki PathDelim }
function DuplicateSlash(const s:string):string;

{ zwraca true jesli c = PathDelim, pod Windows zwraca true takze gdy c = '/'.
  ( Bo pod Windows '/' nie jest PathDelim ale tak naprawde jest akceptowane przez funkcje
  WinAPI (wiec w zasadzie przez wszystko) zamiast PathDelim, wiec gdzieniegdzie jest
  uzywane (takze przeze mnie) zamiast PathDelim - przynajmniej w moim przypadku
  jest to przyzwyczajenie z Linuxa.)  }
function IsPathDelim(c:char):boolean;

{ Moje funkcje Incl/ExclPathDelim robia prawie to samo co Include/ExcludeTrailingPathDelimiter
  a maja mniej meczace nazwy. A wiec, zeby powiedziec to jasno :
  InclPathDelim jesli s nie zawiera na koncu IsPathDelim to zwraca s + PathDelim,
  wpp. zwraca s.
  ExclPathDelim jesli s zawiera na koncu IsPathDelim to zwraca s skrocone o jedna
  literke, wpp. zwraca s.

  Pewne roznice z wersjami SysUtils moga byc: te funkcje nie dzialaja na
  stringach MBCS. Ponadto uzywaja mojej IsPathDelim ktora uwzglednia fakt
  ze pod Windows '/' jest tak naprawde akceptowalnym path delimitorem. }
function InclPathDelim(const s:string):string;
function ExclPathDelim(const s:string):string;

{ fname moze byc bezwzgledne lub wzgledne wzgledem CurrentDir.
  Zwroci fname wzgledne wzgledem CurrentDir, tzn. odpowiednio zmodyfikuje
  fname bezwzgledne lub przepisze bez zmian fname wzgledne. }
function RelativeFilename(const fname:string):string;

{ fname is absolute filename or relative (with respect to CurrentDir).
  This function maybe will convert fname to absolute filename and
  maybe will convert it to relative filename -- whichever gives shorter
  result, i.e. "nicer to show the user".
  E.g. if CurrentDir = '/usr/share/' and
          fname = '/usr/share/some_file'
       Result will be a relative filename 'some_file'
  If CurrentDir = '/usr/share/' and
          fname = '/lib/bla/bla'
       Result will be equal to fname '/lib/bla/bla'
  If CurrentDir = '/usr/share/' and
          fname = '../../lib/bla/bla'
       Result will be absolute filename '/lib/bla/bla'
  The exact operations are not specified here. Current implementation
  returns relative filename if fname is in CurrentDir or subdirectory,
  else it returns absolute filename. }
function NiceFileName(const fname:string):string;

{ sprawdza czy sciezka podana w path jest absolutna. path moze
  wskazywac na katalog (i byc zakoncone PathDelim lub nie), moze
  tez wskazywac na plik. Czyli zupelnie dowolnie.
  Uwaga pod Windowsa : sciezka 'c:\autoexec.bat' jest absolutna, naturalnie.
    Sciezka 'autoexec.bat' naturalnie nie jest absolutna.
    Ale co ze sciezka '\autoexec.bat' ? Okresla ona katalog (w postaci
    bezwzglednej) ale nie okresla dysku. Jest to jedna z zalet UNIXowego
    podejscia (spojne drzewo katalogow), tam nie ma takiego problemu.
    W kazdym razie, sciezka '\autoexec.bat' NIE JEST absolutna no bo nie
    uwzglednia wszystkiego.

    Tym niemniej, zaimplementowalem tez funkcje IsPathAbsoluteOnDrive
    ktora dziala tak samo jak IsPathAbsolute tyle ze DODATKOWO
    dla sciezek w rodzaju '\autoexec.bat' odpowiada true (dla sciezek
    'c:\autoexec.bat' oczywiscie tez ciagle odpowiada true; pod nie-Windowsem
    obie te funkcje robia to samo.)  }
function IsPathAbsolute(const path:string):boolean;
function IsPathAbsoluteOnDrive(const path:string):boolean;

{ czy nazwa katalogu to nazwa specjalna ? Np. '.' albo '..'. dirname nie moze
  byc zadna sciezka ani nic w tym stylu, zadnych ozdobnikow, to jest
  twarde sprawdzanie czy dirname='.' lub '..' (plus pewne zabezpieczenia,
  patrz implementacja; Uzywaj tej f-cji zamiast wykonywac te porownania recznie). }
function SpecialDirName(const dirname:string):boolean;

{ Returns DeleteFileExt(FileName) +Suffix +ExtractFileExt(FileName); }
function AppendToFilename(const FileName, Suffix:string):string;

{ Same as ExpandFileName, but knows that FilePath contains only a path
  and will ALWAYS work as it should (not Delphi's nor FPC's guarantee
  that ExpandFileName will work corrrectly for paths ended with PathDelim).
  FilePath may, but doesn't have to, end with PathDelim.
  Result will always end with PathDelim.

  Note: '' is treated as current dir (as it always should be, because
  ExtractFilePath('file') = '' and 'file' is in current dir. }
function ExpandFilePath(const FilePath:string):string;

{$endif read_interface}

{$ifdef read_implementation}

function DeleteFileExt(const filename:string):string;
var i:integer;
begin
 for i:=Length(filename) downto 1 do
  if filename[i]='.' then begin result:=Copy(filename,1,i-1); exit end else
   if filename[i]=PathDelim then break;
 result:=filename;
end;

function ExtractOnlyFilename(const filename:string):string;
begin
 result:=DeleteFileExt(ExtractFilename(filename));
end;

function ChangeFilePath(const fname, NewPath:string):string;
begin
 result:=NewPath + ExtractFileName(fname);
end;

function DuplicateSlash(const s:string):string;
var i:integer;
begin
 result:=s;
 i:=1;
 while i<=length(result) do
 begin
  if result[i]=PathDelim then begin Insert(PathDelim,result,i); Inc(i) end;
  Inc(i);
 end;
end;

function IsPathDelim(c:char):boolean;
begin
 result:=(c=PathDelim) {$ifdef MSWINDOWS} or (c='/') {$endif};
end;

function InclPathDelim(const s:string):string;
var L:Integer;
begin
 L:=Length(S);
 if (L=0) or (not IsPathDelim(S[L])) then result:=S+PathDelim else result:=S;
end;

function ExclPathDelim(const s:string):string;
var L:Integer;
begin
 L:=Length(S);
 if (L>0) and IsPathDelim(S[L]) then Dec(L);
 Result:=Copy(S,1,L);
end;

function RelativeFilename(const fname:string):string;
begin
 if IsPathAbsolute(fname) then
  Result:=ExtractRelativePath( InclPathDelim(GetCurrentDir) + 'dummy_file',
    fname) else
 { TODO: MSWINDOWS - chyba nie trzeba uwzgledniac tu
   IsPathAbsoluteOnDrive(fname) ? }
  Result:=fname;
end;

function NiceFileName(const fname:string):string;
var AbsoluteFname:string;
begin
 AbsoluteFname:=ExpandFileName(fname);
 { It's important that we end InclPathDelim(GetCurrentDir) and
   ExtractFilePath(AbsoluteFname) always with PathDelim, it makes sure that
   IsPrefix actually checks is one file in subdirectory of another. }
 if IsPrefix( InclPathDelim(GetCurrentDir), ExtractFilePath(AbsoluteFname) ) then
  Result:=RelativeFilename(fname) else
  Result:=AbsoluteFname;
end;

function IsPathAbsolute(const path:string):boolean;
begin
 {$ifdef UNIX} result:=SCharIs(path,1,PathDelim)
 {$else} result:=SCharIs(path,2,DriveDelim)
 {$endif}
end;

function IsPathAbsoluteOnDrive(const path:string):boolean;
begin
 result:=IsPathAbsolute(path)
   {$ifdef MSWINDOWS} or SCharIs(path,1,PathDelim) {$endif}
end;

function SpecialDirName(const dirname:string):boolean;
begin
 { Microsoft swego czasu straszyl ze w systemie beda zdefiniowane katalogi ...
     i .... jako katalog-dziadek i pradziadek tak jak obecnie sa .. jako rodzic i
     . jako aktualny. Sprawdzilem to i przynajmniej pod win95/98 nie zrobili tego
     bezuzytecznego kretynstwa, ale zawsze lepiej sie zabezpieczyc - stad powyzsze
     checki '...' i '....'. One niczemu nie przeszkadzaja, bo i tak
     ... i .... to nieprawidlowe nazwy plikow pod Windows wiec ten check
     (poza zabieraniem chwilki czasu) nic nie psuje.

   Pod linuxem (i prawdopodobnie kazdym innym nie-Windowsem)
   ... i .... to zupelnie legalne nazwy plikow dlatego
   ten check nie moze istniec pod nie-Windowsem. }
 result:=ArrayPosStr(dirname,['.','..' {$ifdef MSWINDOWS},'...','....'{$endif}])>=0;
end;

function AppendToFilename(const FileName, Suffix:string):string;
begin
 { I know, this can be optimized, as DeleteFileExt and ExtractFileExt
   do almost the same work twice. }
 Result:=DeleteFileExt(FileName) +Suffix +ExtractFileExt(FileName);
end;

function ExpandFilePath(const FilePath:string):string;
begin
 if FilePath = '' then
  Result:=InclPathDelim(GetCurrentDir) else
  Result:=ExtractFilePath( ExpandFileName(InclPathDelim(FilePath) +
    'dummy_file_name' ));

 { note : doing here something like
     ExtractFilePath( ExpandFileName(ExclPathDelim(FilePath)) )
   (i.e., pretending that FilePath points to a file, not directory)
   would not be safe, because we don't know what ExpandFileName
   will do with 'c:' }
end;

{$endif read_implementation}
