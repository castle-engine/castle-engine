{
  Copyright 2008-2017 Michalis Kamburelis.

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
  { General open dialog that uses URL. }
  TCastleOpenDialog = class(TOpenDialog)
  private
    function GetURL: string;
    procedure SetURL(AValue: string);
  public
    property URL: string read GetURL write SetURL stored false;
  end;

  { General save dialog that uses URL. }
  TCastleSaveDialog = class(TSaveDialog)
  private
    function GetURL: string;
    procedure SetURL(AValue: string);
  public
    property URL: string read GetURL write SetURL stored false;
  end;

  { 3D model open dialog. It uses an URL, and additionally initializes the filters
    to include all the 3D model types our engine can load (through
    Load3D, through setting TCastleScene.URL and other functions). }
  TCastleOpen3DDialog = class(TOpenDialog)
  private
    InitialFilterIndex: Integer;
    InitialFilter: string;
    function GetURL: string;
    procedure SetURL(AValue: string);
    function StoreFilterAndFilterIndex: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property URL: string read GetURL write SetURL stored false;
  published
    property Filter stored StoreFilterAndFilterIndex;
    property FilterIndex stored StoreFilterAndFilterIndex;
  end;

  { Image open dialog. It uses an URL, and additionally initializes the filters
    to include all the image types our engine can load through
    @link(CastleImages.LoadImage). }
  TCastleOpenImageDialog = class(TOpenPictureDialog)
  private
    InitialFilterIndex: Integer;
    InitialFilter: string;
    function GetURL: string;
    procedure SetURL(AValue: string);
    function StoreFilterAndFilterIndex: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property URL: string read GetURL write SetURL stored false;
  published
    property Filter stored StoreFilterAndFilterIndex;
    property FilterIndex stored StoreFilterAndFilterIndex;
  end;

  { Image save dialog. It uses an URL, and additionally initializes the filters
    to include all the image types our engine can save through
    @link(CastleImages.SaveImage). }
  TCastleSaveImageDialog = class(TSavePictureDialog)
  private
    InitialFilterIndex: Integer;
    InitialFilter: string;
    function GetURL: string;
    procedure SetURL(AValue: string);
    function StoreFilterAndFilterIndex: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property URL: string read GetURL write SetURL stored false;
  published
    property Filter stored StoreFilterAndFilterIndex;
    property FilterIndex stored StoreFilterAndFilterIndex;
  end;

procedure Register;

implementation

uses CastleURIUtils, CastleLCLUtils, X3DLoad, CastleImages;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleOpenDialog,
    TCastleSaveDialog,
    TCastleOpen3DDialog,
    TCastleOpenImageDialog,
    TCastleSaveImageDialog
  ]);
end;

function TCastleOpen3DDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(FileName);
end;

procedure TCastleOpen3DDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
end;

function TCastleOpen3DDialog.StoreFilterAndFilterIndex: boolean;
begin
  Result := (Filter <> InitialFilter) or (FilterIndex <> InitialFilterIndex);
end;

constructor TCastleOpen3DDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FileFiltersToDialog(Load3D_FileFilters, Self);
  InitialFilter := Filter;
  InitialFilterIndex := FilterIndex;
end;

function TCastleSaveImageDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(FileName);
end;

procedure TCastleSaveImageDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
end;

function TCastleSaveImageDialog.StoreFilterAndFilterIndex: boolean;
begin
  Result := (Filter <> InitialFilter) or (FilterIndex <> InitialFilterIndex);
end;

constructor TCastleSaveImageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FileFiltersToDialog(SaveImage_FileFilters, Self);
  InitialFilter := Filter;
  InitialFilterIndex := FilterIndex;
end;

function TCastleOpenImageDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(FileName);
end;

procedure TCastleOpenImageDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
end;

function TCastleOpenImageDialog.StoreFilterAndFilterIndex: boolean;
begin
  Result := (Filter <> InitialFilter) or (FilterIndex <> InitialFilterIndex);
end;

constructor TCastleOpenImageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FileFiltersToDialog(LoadImage_FileFilters, Self);
  InitialFilter := Filter;
  InitialFilterIndex := FilterIndex;
end;

function TCastleSaveDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(FileName);
end;

procedure TCastleSaveDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
end;

procedure TCastleOpenDialog.SetURL(AValue: string);
begin
  FileName := URIToFilenameSafeUTF8(AValue);
end;

function TCastleOpenDialog.GetURL: string;
begin
  Result := FilenameToURISafeUTF8(FileName);
end;

end.
