{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Dialog windows. }
unit CastleDialogs;

{$I castleconf.inc}

interface

uses Classes, Dialogs, ExtDlgs;

type
  { General open dialog that uses URL.
    The URL is a file: or castle-data: URL. }
  TCastleOpenDialog = class(TOpenDialog)
  private
    FAdviceDataDirectory: Boolean;
    FUseCastleDataProtocol: Boolean;
    function GetURL: string;
    procedure SetURL(AValue: string);
  protected
    function DoExecute: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property URL: string read GetURL write SetURL stored false;
    { Warn (but still allow) if user selects URL outside of data directory. }
    property AdviceDataDirectory: Boolean read FAdviceDataDirectory write FAdviceDataDirectory default false;
    { If the URL is detected inside data directory ( https://castle-engine.io/manual_data_directory.php ),
      make sure it starts with protocol castle-data:/ . }
    property UseCastleDataProtocol: Boolean read FUseCastleDataProtocol write FUseCastleDataProtocol default true;
  end;

  { General save dialog that uses URL.
    The URL is a file: or castle-data: URL. }
  TCastleSaveDialog = class(TSaveDialog)
  private
    FAdviceDataDirectory: Boolean;
    FUseCastleDataProtocol: Boolean;
    function GetURL: string;
    procedure SetURL(AValue: string);
  protected
    function DoExecute: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property URL: string read GetURL write SetURL stored false;
    { Warn (but still allow) if user selects URL outside of data directory. }
    property AdviceDataDirectory: Boolean read FAdviceDataDirectory write FAdviceDataDirectory default false;
    { If the URL is detected inside data directory ( https://castle-engine.io/manual_data_directory.php ),
      make sure it starts with protocol castle-data:/ . }
    property UseCastleDataProtocol: Boolean read FUseCastleDataProtocol write FUseCastleDataProtocol default true;
  end;

  { Dialog to open scene (select a file that can be loaded using TCastleScene.Load).
    It uses an URL, and additionally initializes the filters
    to include all the scene types we can load (through
    LoadNode, TCastleScene.Load, TCastleScene.URL and so on). }
  TCastleOpen3DDialog = class(TOpenDialog)
  private
    FAdviceDataDirectory: Boolean;
    FUseCastleDataProtocol: Boolean;
    InitialFilterIndex: Integer;
    InitialFilter: string;
    function GetURL: string;
    procedure SetURL(AValue: string);
    function StoreFilterAndFilterIndex: boolean;
  protected
    function DoExecute: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property URL: string read GetURL write SetURL stored false;
    { Warn (but still allow) if user selects URL outside of data directory. }
    property AdviceDataDirectory: Boolean read FAdviceDataDirectory write FAdviceDataDirectory default false;
    { If the URL is detected inside data directory ( https://castle-engine.io/manual_data_directory.php ),
      make sure it starts with protocol castle-data:/ . }
    property UseCastleDataProtocol: Boolean read FUseCastleDataProtocol write FUseCastleDataProtocol default true;
    property Filter stored StoreFilterAndFilterIndex;
    property FilterIndex stored StoreFilterAndFilterIndex;
  end;

  { Image open dialog. It uses an URL, and additionally initializes the filters
    to include all the image types our engine can load through
    @link(CastleImages.LoadImage). }
  TCastleOpenImageDialog = class(TOpenPictureDialog)
  private
    FAdviceDataDirectory: Boolean;
    FUseCastleDataProtocol: Boolean;
    InitialFilterIndex: Integer;
    InitialFilter: string;
    function PrepareURL(const AFileName: String): String;
    function GetURL: String;
    function GetURLWithIndex(const Index: Integer): String;
    procedure SetURL(AValue: String);
    function StoreFilterAndFilterIndex: boolean;
  protected
    function DoExecute: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    { Number of selected images to open (useful when multi-selection is allowed). }
    function URLCount: Integer;

    { Get URL of a selected image to open (useful when multi-selection is allowed). }
    property URLs[Index: Integer]: String read GetURLWithIndex;
  published
    property URL: string read GetURL write SetURL stored false;
    { Warn (but still allow) if user selects URL outside of data directory. }
    property AdviceDataDirectory: Boolean read FAdviceDataDirectory write FAdviceDataDirectory default false;
    { If the URL is detected inside data directory ( https://castle-engine.io/manual_data_directory.php ),
      make sure it starts with protocol castle-data:/ . }
    property UseCastleDataProtocol: Boolean read FUseCastleDataProtocol write FUseCastleDataProtocol default true;
    property Filter stored StoreFilterAndFilterIndex;
    property FilterIndex stored StoreFilterAndFilterIndex;
  end;

  { Image save dialog. It uses an URL, and additionally initializes the filters
    to include all the image types our engine can save through
    @link(CastleImages.SaveImage). }
  TCastleSaveImageDialog = class(TSavePictureDialog)
  private
    FAdviceDataDirectory: Boolean;
    FUseCastleDataProtocol: Boolean;
    InitialFilterIndex: Integer;
    InitialFilter: string;
    function GetURL: string;
    procedure SetURL(AValue: string);
    function StoreFilterAndFilterIndex: boolean;
  protected
    function DoExecute: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property URL: string read GetURL write SetURL stored false;
    { Warn (but still allow) if user selects URL outside of data directory. }
    property AdviceDataDirectory: Boolean read FAdviceDataDirectory write FAdviceDataDirectory default false;
    { If the URL is detected inside data directory ( https://castle-engine.io/manual_data_directory.php ),
      make sure it starts with protocol castle-data:/ . }
    property UseCastleDataProtocol: Boolean read FUseCastleDataProtocol write FUseCastleDataProtocol default true;
    property Filter stored StoreFilterAndFilterIndex;
    property FilterIndex stored StoreFilterAndFilterIndex;
  end;

  TCastleOpenPascalUnitDialog = class(TOpenDialog)
  private
    const
      InitialFilterIndex = 0;
      InitialFilter = 'Pascal unit (*.pas, *.pp)|*.pas;*.pp|All Files|*';
    function StoreFilterAndFilterIndex: boolean;
  protected
    function DoExecute: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Filter stored StoreFilterAndFilterIndex;
    property FilterIndex stored StoreFilterAndFilterIndex;
  end;

procedure Register;

implementation

uses SysUtils,
  CastleURIUtils, CastleLCLUtils, X3DLoad, CastleImages, CastleFilesUtils,
  CastleStringUtils, CastleUtils;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleOpenDialog,
    TCastleSaveDialog,
    TCastleOpen3DDialog,
    TCastleOpenImageDialog,
    TCastleSaveImageDialog,
    TCastleOpenPascalUnitDialog
  ]);
end;

procedure WarningIfOutsideDataDirectory(const URL: String);
begin
  if URIProtocol(URL) <> 'castle-data' then
    MessageDlg('File outside data', 'You are saving or opening a file outside of the project''s "data" directory.' + NL +
      NL +
      'The file is: ' + URL + NL +
      NL +
      'The "data" directory is: ' + ResolveCastleDataURL('castle-data:/') + NL +
      NL +
      'While it is allowed, it is not encouraged for cross-platform applications:' + NL +
      '- You will not be able to open this file using castle-data:/ URL (or ApplicationData function).' + NL +
      '- The file will not be packaged with your distributed application automatically.' + NL +
      'Unless you really know what you''re doing, we advice to instead open or save inside the project "data" directory.',
      mtWarning, [mbOK], 0);
end;

function CleanupFileName(const FileName: String): String;
begin
  { Using GTK 2 file open dialog under Ubuntu Mate,
    you can get filenames with // .
    They are mostly harmless (opening such filenames with various
    routines work) but they make MaybeUseDataProtocol less often possible
    (since sometimes paths will not match, because one contains //),
    and thus would cause unnecessary calls to WarningIfOutsideDataDirectory . }
  Result :=
    {$ifdef UNIX}
    StringReplace(FileName, '//', '/', [rfReplaceAll]);
    {$else}
    FileName;
    {$endif}
end;

{ TCastleOpen3DDialog ----------------------------------------------------- }

function TCastleOpen3DDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(CleanupFileName(FileName));
  if UseCastleDataProtocol then
    Result := MaybeUseDataProtocol(Result);
end;

procedure TCastleOpen3DDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
  InitialDir := ExtractFileDir(FileName);
end;

function TCastleOpen3DDialog.StoreFilterAndFilterIndex: boolean;
begin
  Result := (Filter <> InitialFilter) or (FilterIndex <> InitialFilterIndex);
end;

function TCastleOpen3DDialog.DoExecute: boolean;
begin
  Result := inherited DoExecute;
  if Result and AdviceDataDirectory then
    WarningIfOutsideDataDirectory(URL);
end;

constructor TCastleOpen3DDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseCastleDataProtocol := true;
  FileFiltersToDialog(LoadScene_FileFilters, Self);
  InitialFilter := Filter;
  InitialFilterIndex := FilterIndex;
end;

{ TCastleSaveImageDialog ------------------------------------------------- }

function TCastleSaveImageDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(CleanupFileName(FileName));
  if UseCastleDataProtocol then
    Result := MaybeUseDataProtocol(Result);
end;

procedure TCastleSaveImageDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
  InitialDir := ExtractFileDir(FileName);
end;

function TCastleSaveImageDialog.StoreFilterAndFilterIndex: boolean;
begin
  Result := (Filter <> InitialFilter) or (FilterIndex <> InitialFilterIndex);
end;

function TCastleSaveImageDialog.DoExecute: boolean;
begin
  Result := inherited DoExecute;
  if Result and AdviceDataDirectory then
    WarningIfOutsideDataDirectory(URL);
end;

constructor TCastleSaveImageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseCastleDataProtocol := true;
  FileFiltersToDialog(SaveImage_FileFilters, Self);
  InitialFilter := Filter;
  InitialFilterIndex := FilterIndex;
end;

{ TCastleOpenImageDialog --------------------------------------------------- }

function TCastleOpenImageDialog.PrepareURL(const AFileName: String): String;
begin
  Result := FilenameToURISafeUTF8(CleanupFileName(AFileName));
  if UseCastleDataProtocol then
    Result := MaybeUseDataProtocol(Result);
end;

function TCastleOpenImageDialog.GetURL: String;
begin
  Result := PrepareURL(FileName);
end;

function TCastleOpenImageDialog.GetURLWithIndex(const Index: Integer): String;
begin
  Result := PrepareURL(Files[Index]);
end;

procedure TCastleOpenImageDialog.SetURL(AValue: String);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
  InitialDir := ExtractFileDir(FileName);
end;

function TCastleOpenImageDialog.StoreFilterAndFilterIndex: boolean;
begin
  Result := (Filter <> InitialFilter) or (FilterIndex <> InitialFilterIndex);
end;

function TCastleOpenImageDialog.DoExecute: boolean;
begin
  Result := inherited DoExecute;
  if Result and AdviceDataDirectory then
    WarningIfOutsideDataDirectory(URL);
end;

constructor TCastleOpenImageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseCastleDataProtocol := true;
  FileFiltersToDialog(LoadImage_FileFilters, Self);
  InitialFilter := Filter;
  InitialFilterIndex := FilterIndex;
end;

function TCastleOpenImageDialog.URLCount: Integer;
begin
  Result := Files.Count;
end;

{ TCastleSaveDialog -------------------------------------------------------- }

function TCastleSaveDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(CleanupFileName(FileName));
  if UseCastleDataProtocol then
    Result := MaybeUseDataProtocol(Result);
end;

procedure TCastleSaveDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
  InitialDir := ExtractFileDir(FileName);
end;

function TCastleSaveDialog.DoExecute: boolean;
begin
  Result := inherited DoExecute;
  if Result and AdviceDataDirectory then
    WarningIfOutsideDataDirectory(URL);
end;

constructor TCastleSaveDialog.Create(AOwner: TComponent);
begin
  inherited;
  FUseCastleDataProtocol := true;
end;

{ TCastleOpenDialog ---------------------------------------------------------- }

function TCastleOpenDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(CleanupFileName(FileName));
  if UseCastleDataProtocol then
    Result := MaybeUseDataProtocol(Result);
end;

procedure TCastleOpenDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
  InitialDir := ExtractFileDir(FileName);
end;

function TCastleOpenDialog.DoExecute: boolean;
begin
  Result := inherited DoExecute;
  if Result and AdviceDataDirectory then
    WarningIfOutsideDataDirectory(URL);
end;

constructor TCastleOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FUseCastleDataProtocol := true;
end;

{ TCastleOpenPascalUnitDialog ----------------------------------------------------- }

function TCastleOpenPascalUnitDialog.StoreFilterAndFilterIndex: boolean;
begin
  Result := (Filter <> InitialFilter) or (FilterIndex <> InitialFilterIndex);
end;

function TCastleOpenPascalUnitDialog.DoExecute: boolean;
begin
  Result := inherited DoExecute;
end;

constructor TCastleOpenPascalUnitDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Filter := InitialFilter;
  FilterIndex := InitialFilterIndex;
end;

end.
