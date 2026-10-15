{
  Copyright 2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Form to import packages from Castle Library. }
unit FormImportPackage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Generics.Collections,
  CastleDownload, CastleImages, CastleStringUtils, CastleLog;

type
  TDescriptionRun = record
    Text: String;
    Url: String;
  end;

  TDescriptionLinkArea = record
    Bounds: TRect;
    Url: String;
  end;

  TPackageDescription = class(TCustomControl)
  private
    FDescriptionRuns: specialize TList<TDescriptionRun>;
    FLinkAreas: specialize TList<TDescriptionLinkArea>;
    procedure AddRun(const RunText, Url: String);
    function CleanHtml(const Html: String): String;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetHtml(const Html: String);
  end;

  { Record to hold component information from library. }
  TPackageInfo = record
    Name: String;
    Description: String;
    ImageUrl: String;
    RepoUrl: String;
  end;

  { Component panel template class. }
  TPackagePanel = class(TPanel)
    Image: TImage;
    LabelName: TLabel;
    LabelDescription: TPackageDescription;
    LabelRepo: TLabel;
    ButtonAdd: TButton;
  public
    RepoUrl: String;
    procedure ButtonAddClick(Sender: TObject);
    procedure LabelRepoClick(Sender: TObject);
  end;

  TPackageImageDownload = class(TCastleDownload)
  public
    ImageControl: TImage;
  end;

  { Form to import package from Castle Library. }
  TImportPackageForm = class(TForm)
    ButtonClose: TButton;
    ButtonRefresh: TButton;
    ImageListPackages: TImageList;
    ScrollBoxPackages: TScrollBox;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ComponentList: specialize TList<TPackageInfo>;
    ImageDownloads: specialize TList<TPackageImageDownload>;
    ImageDownloadUpdateTimer: TTimer;
    procedure DownloadAndParseLibrary;
    procedure DisplayComponents;
    procedure CancelImageDownloads;
    procedure ImageDownloadUpdateTimerTimer(Sender: TObject);
    procedure StartImageDownload(const ImageControl: TImage;
      const Url: String);
    procedure ImageDownloadFinished(const Sender: TCastleDownload;
      var FreeSender: Boolean);
    function ParseComponentBlock(const Block: String): TPackageInfo;
    procedure ParseJsonLibrary(const Content: String);
    procedure ParseFallbackLibrary(const Content: String);
  public
  end;

var
  ImportPackageForm: TImportPackageForm;

const
  LibraryUrl = 'https://raw.githubusercontent.com/castle-engine/cge-www/refs/heads/master/htdocs/additional_components.json';
  LibraryFallbackUrl = 'https://raw.githubusercontent.com/castle-engine/cge-www/refs/heads/master/htdocs/additional_components.php';
  ImageBaseUrl = 'https://raw.githubusercontent.com/castle-engine/cge-www/refs/heads/master/htdocs/images/original_size/';
  GalleryImageBaseUrl = 'https://raw.githubusercontent.com/castle-engine/cge-www/refs/heads/master/htdocs/images/gallery_size/';

implementation

{$R *.lfm}

uses
  RegExpr, FPImgCmn, Types, IntfGraphics, GraphType, FPImage, fpjson, jsonparser,
  CastleOpenDocument, CastleControls, CastleFilesUtils,
  CastleApplicationProperties,
  editorutils;

{ TPackageDescription ----------------------------------------------------- }

constructor TPackageDescription.Create(AOwner: TComponent);
begin
  inherited;
  FDescriptionRuns := specialize TList<TDescriptionRun>.Create;
  FLinkAreas := specialize TList<TDescriptionLinkArea>.Create;
  Color := clWindow;
  ParentColor := true;
  Font.Size := 10;
  ControlStyle := ControlStyle + [csOpaque];
  Self.DoubleBuffered := true;
end;

destructor TPackageDescription.Destroy;
begin
  FreeAndNil(FLinkAreas);
  FreeAndNil(FDescriptionRuns);
  inherited;
end;

procedure TPackageDescription.AddRun(const RunText, Url: String);
var
  Run: TDescriptionRun;
begin
  if RunText = '' then
    Exit;
  Run.Text := RunText;
  Run.Url := Url;
  FDescriptionRuns.Add(Run);
end;

function TPackageDescription.CleanHtml(const Html: String): String;
var
  Regex: TRegExpr;
begin
  Result := StringReplace(Html, '<br>', LineEnding, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '<br/>', LineEnding, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '</p>', LineEnding, [rfReplaceAll, rfIgnoreCase]);
  Regex := TRegExpr.Create;
  try
    Regex.ModifierG := true;
    Regex.Expression := '<[^>]*>';
    Result := Regex.Replace(Result, '', true);
  finally
    Regex.Free;
  end;
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&#39;', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
end;

procedure TPackageDescription.SetHtml(const Html: String);
var
  Regex: TRegExpr;
  Remaining, BeforeLink: String;
  LinkUrl, LinkText: String;
  LinkPosition, LinkLength: Integer;
begin
  FDescriptionRuns.Clear;
  Remaining := Html;
  Regex := TRegExpr.Create;
  try
    Regex.ModifierI := true;
    Regex.ModifierS := true;
    Regex.Expression := '<a\b[^>]*href\s*=\s*["' + #39 +
      ']([^"' + #39 + ']*)["' + #39 + '][^>]*>(.*?)</a\s*>';
    while Regex.Exec(Remaining) do
    begin
      LinkPosition := Regex.MatchPos[0];
      LinkLength := Regex.MatchLen[0];
      BeforeLink := Copy(Remaining, 1, LinkPosition - 1);
      AddRun(CleanHtml(BeforeLink), '');
      LinkUrl := CleanHtml(Regex.Match[1]);
      LinkText := CleanHtml(Regex.Match[2]);
      AddRun(LinkText, LinkUrl);
      Remaining := Copy(Remaining, LinkPosition + LinkLength, MaxInt);
    end;
    AddRun(CleanHtml(Remaining), '');
  finally
    Regex.Free;
  end;
  Invalidate;
end;

procedure TPackageDescription.Paint;
var
  RunIndex, I, Start, LineHeight, X, Y, AvailableWidth: Integer;
  Run: TDescriptionRun;
  WordText, SpaceText: String;
  TextWidth: Integer;
  LinkArea: TDescriptionLinkArea;
  IsLink: Boolean;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Canvas.Font.Assign(Font);
  FLinkAreas.Clear;
  LineHeight := Canvas.TextHeight('Ag');
  X := 4;
  Y := 2;
  AvailableWidth := ClientWidth - 4;

  for RunIndex := 0 to FDescriptionRuns.Count - 1 do
  begin
    Run := FDescriptionRuns[RunIndex];
    IsLink := Run.Url <> '';
    I := 1;
    while I <= Length(Run.Text) do
    begin
      if (Run.Text[I] = #10) or (Run.Text[I] = #13) then
      begin
        if Run.Text[I] = #13 then
        begin
          Inc(I);
          if (I <= Length(Run.Text)) and (Run.Text[I] = #10) then
            Inc(I);
        end else
          Inc(I);
        X := 4;
        Inc(Y, LineHeight);
        Continue;
      end;

      Start := I;
      while (I <= Length(Run.Text)) and
        (Run.Text[I] > ' ') do
        Inc(I);
      WordText := Copy(Run.Text, Start, I - Start);

      if IsLink then
      begin
        Canvas.Font.Color := clBlue;
        Canvas.Font.Style := Font.Style + [fsUnderline];
      end else
      begin
        Canvas.Font.Color := Font.Color;
        Canvas.Font.Style := Font.Style;
      end;
      TextWidth := Canvas.TextWidth(WordText);
      if (X > 4) and (X + TextWidth > AvailableWidth) then
      begin
        X := 4;
        Inc(Y, LineHeight);
      end;
      Canvas.TextOut(X, Y, WordText);
      if IsLink and (TextWidth > 0) then
      begin
        LinkArea.Bounds := Rect(X, Y, X + TextWidth, Y + LineHeight);
        LinkArea.Url := Run.Url;
        FLinkAreas.Add(LinkArea);
      end;
      Inc(X, TextWidth);

      Start := I;
      while (I <= Length(Run.Text)) and
        (Run.Text[I] > #0) and (Run.Text[I] <= ' ') and
        (Run.Text[I] <> #10) and (Run.Text[I] <> #13) do
        Inc(I);
      SpaceText := Copy(Run.Text, Start, I - Start);
      if SpaceText <> '' then
      begin
        Canvas.Font.Color := Font.Color;
        Canvas.Font.Style := Font.Style;
        Inc(X, Canvas.TextWidth(SpaceText));
      end;
    end;
  end;
end;

procedure TPackageDescription.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  for I := 0 to FLinkAreas.Count - 1 do
    if PtInRect(FLinkAreas[I].Bounds, Point(X, Y)) then
    begin
      OpenUrl(FLinkAreas[I].Url);
      Exit;
    end;
end;

procedure TPackageDescription.MouseMove(Shift: TShiftState; X, Y: Integer);
var
    I: Integer;
begin
    inherited;
    Cursor := crDefault;
    for I := 0 to FLinkAreas.Count - 1 do
      if PtInRect(FLinkAreas[I].Bounds, Point(X, Y)) then
      begin
        Cursor := crHandPoint;
        Exit;
      end;
end;

{ TPackagePanel ----------------------------------------------------------- }

procedure TPackagePanel.ButtonAddClick(Sender: TObject);
begin
  { Empty click handler for now - logic will be added later. }
  WritelnLog('ImportPackage', 'Add button clicked for: ' + LabelName.Caption);
end;

procedure TPackagePanel.LabelRepoClick(Sender: TObject);
begin
  OpenUrl(RepoUrl);
end;

{ TImportPackageForm ------------------------------------------------------- }

procedure TImportPackageForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TImportPackageForm.ButtonRefreshClick(Sender: TObject);
begin
  DownloadAndParseLibrary;
end;

procedure TImportPackageForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  { Do not free at Close anymore, keep data. }
  ImageDownloadUpdateTimer.Enabled := false;
  CloseAction := caHide;
end;

procedure TImportPackageForm.FormCreate(Sender: TObject);
begin
  ComponentList := specialize TList<TPackageInfo>.Create;
  ImageDownloads := specialize TList<TPackageImageDownload>.Create;
  ImageDownloadUpdateTimer := TTimer.Create(Self);
  ImageDownloadUpdateTimer.Interval := 30;
  ImageDownloadUpdateTimer.OnTimer := @ImageDownloadUpdateTimerTimer;
  ScrollboxPackages.DoubleBuffered := true;
  ScrollBoxPackages.VertScrollBar.Tracking := True;
  ScrollBoxPackages.HorzScrollBar.Tracking := True;
end;

procedure TImportPackageForm.FormDestroy(Sender: TObject);
begin
  ImageDownloadUpdateTimer.Enabled := false;
  CancelImageDownloads;
  FreeAndNil(ImageDownloads);
  FreeAndNil(ComponentList);
end;

procedure TImportPackageForm.FormShow(Sender: TObject);
begin
  ImageDownloadUpdateTimer.Enabled := true;
  if ComponentList.Count = 0 then
    DownloadAndParseLibrary;
end;

procedure TImportPackageForm.ImageDownloadUpdateTimerTimer(Sender: TObject);
begin
  ApplicationProperties._Update;
end;

procedure TImportPackageForm.CancelImageDownloads;
var
  I: Integer;
begin
  for I := ImageDownloads.Count - 1 downto 0 do
    ImageDownloads[I].Free;
  ImageDownloads.Clear;
end;

procedure TImportPackageForm.StartImageDownload(const ImageControl: TImage;
  const Url: String);
var
  ImageDownload: TPackageImageDownload;
begin
  ImageDownload := TPackageImageDownload.Create(nil);
  ImageDownload.ImageControl := ImageControl;
  ImageDownload.Url := Url;
  ImageDownload.OnFinish := @ImageDownloadFinished;
  ImageDownloads.Add(ImageDownload);
  try
    WritelnLog('ImportPackage', 'Starting asynchronous image download: ' + Url);
    ImageDownload.Start;
  except
    on E: Exception do
    begin
      ImageDownloads.Remove(ImageDownload);
      ImageDownload.Free;
      WritelnWarning('ImportPackage', 'Failed to start package image download from ' +
        Url + ': ' + E.Message);
      ImageControl.Canvas.Brush.Color := clGray;
      ImageControl.Canvas.FillRect(0, 0, ImageControl.Width, ImageControl.Height);
      ImageControl.Canvas.Font.Color := clWhite;
      ImageControl.Canvas.TextOut(10, (ImageControl.Height div 2) - 5, 'No Image');
    end;
  end;
end;

procedure TImportPackageForm.ImageDownloadFinished(
  const Sender: TCastleDownload; var FreeSender: Boolean);

  procedure DisplayImage(const Image: TCastleImage;
    const Destination: TImage);
  var
    IntfImage: TLazIntfImage;
    RawImage: TRawImage;
    CustomFPImage: TFPCustomImage;
    Bitmap: TBitmap;
  begin
    RawImage.Init;
    {$ifdef LCLWIN32}
    RawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(Image.Width, Image.Height);
    {$else}
    RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(Image.Width, Image.Height);
    {$endif}
    RawImage.CreateData(True);
    IntfImage := TLazIntfImage.Create(0, 0);
    try
      IntfImage.SetRawImage(RawImage);
      CustomFPImage := Image.ToFpImage;
      try
        IntfImage.CopyPixels(CustomFPImage);
      finally
        FreeAndNil(CustomFPImage);
      end;

      Bitmap := TBitmap.Create;
      try
        Bitmap.LoadFromIntfImage(IntfImage);
        Destination.Picture.Assign(Bitmap);
        if (Destination.Picture.Graphic = nil) or
          (Destination.Picture.Width <> Image.Width) or
          (Destination.Picture.Height <> Image.Height) then
          raise Exception.Create('Decoded image was not assigned correctly to TImage');
        Destination.Invalidate;
        WritelnLog('ImportPackage', Format(
          'Assigned package image (%dx%d) to TImage',
          [Destination.Picture.Width, Destination.Picture.Height]));
      finally
        Bitmap.Free;
      end;
    finally
      IntfImage.Free;
    end;
  end;

var
  ImageDownload: TPackageImageDownload;
  ImageControl: TImage;
  ImageLoader: TCastleImageControl;
  TemporaryImageFile: String;
  ImageExtension: String;
  QueryPosition: SizeInt;
  FragmentPosition: SizeInt;
begin
  ImageDownload := TPackageImageDownload(Sender);
  ImageDownloads.Remove(ImageDownload);
  ImageControl := ImageDownload.ImageControl;

  try
    if Sender.Status <> dsSuccess then
      raise Exception.Create(Sender.ErrorMessage);

    WritelnLog('ImportPackage', Format(
      'Image download finished: %s (%d bytes, MIME type "%s")',
      [Sender.Url, Sender.Contents.Size, Sender.MimeType]));

    { Load through TCastleImageControl like the editor's file preview. This
      uses the same URL-based image decoder for GIF and other formats. }
    ImageExtension := Sender.Url;
    QueryPosition := Pos('?', ImageExtension);
    FragmentPosition := Pos('#', ImageExtension);
    if (QueryPosition > 0) and
      ((FragmentPosition = 0) or (QueryPosition < FragmentPosition)) then
      Delete(ImageExtension, QueryPosition, Length(ImageExtension));
    FragmentPosition := Pos('#', ImageExtension);
    if FragmentPosition > 0 then
      Delete(ImageExtension, FragmentPosition, Length(ImageExtension));
    ImageExtension := ExtractFileExt(ImageExtension);
    TemporaryImageFile := GetTempFileNameCheck + ImageExtension;
    try
      Sender.Contents.Position := 0;
      StreamSaveToFile(Sender.Contents, TemporaryImageFile);

      ImageLoader := TCastleImageControl.Create(nil);
      try
        ImageLoader.Cache := false;
        ImageLoader.URL := TemporaryImageFile;
        if not (ImageLoader.Content.Image is TCastleImage) then
          raise Exception.Create('Image URL did not decode to a TCastleImage');
        DisplayImage(TCastleImage(ImageLoader.Content.Image), ImageControl);
      finally
        ImageLoader.Free;
      end;
    finally
      CheckDeleteFile(TemporaryImageFile, true);
    end;
  except
    on E: Exception do
    begin
      WritelnWarning('ImportPackage', 'Failed to load package image from ' +
        Sender.Url + ': ' + E.Message);
      ImageControl.Canvas.Brush.Color := clGray;
      ImageControl.Canvas.FillRect(0, 0, ImageControl.Width, ImageControl.Height);
      ImageControl.Canvas.Font.Color := clWhite;
      ImageControl.Canvas.TextOut(10, (ImageControl.Height div 2) - 5, 'No Image');
    end;
  end;
  FreeSender := true;
end;

function TImportPackageForm.ParseComponentBlock(const Block: String): TPackageInfo;
var
  ArgPosition: Integer;

  procedure SkipWhitespaceAndComments;
  begin
    while ArgPosition <= Length(Block) do
    begin
      if Block[ArgPosition] <= ' ' then
        Inc(ArgPosition)
      else
      if (Block[ArgPosition] = '/') and (ArgPosition < Length(Block)) and
        (Block[ArgPosition + 1] = '/') then
      begin
        Inc(ArgPosition, 2);
        while (ArgPosition <= Length(Block)) and
          (Block[ArgPosition] <> #10) and (Block[ArgPosition] <> #13) do
          Inc(ArgPosition);
      end else
      if (Block[ArgPosition] = '/') and (ArgPosition < Length(Block)) and
        (Block[ArgPosition + 1] = '*') then
      begin
        Inc(ArgPosition, 2);
        while (ArgPosition < Length(Block)) and
          ((Block[ArgPosition] <> '*') or (Block[ArgPosition + 1] <> '/')) do
          Inc(ArgPosition);
        if ArgPosition < Length(Block) then
          Inc(ArgPosition, 2);
      end else
      if Block[ArgPosition] = '#' then
      begin
        while (ArgPosition <= Length(Block)) and
          (Block[ArgPosition] <> #10) and (Block[ArgPosition] <> #13) do
          Inc(ArgPosition);
      end else
        Exit;
    end;
  end;

  function ReadStringArgument(const ArgumentName: String): String;
  var
    QuoteChar: Char;
    ParenthesisDepth: Integer;
    IsTopLevelString: Boolean;
    ClosedString: Boolean;
    SawTopLevelString: Boolean;
  begin
    SkipWhitespaceAndComments;
    if ArgPosition > Length(Block) then
      raise Exception.Create('Missing ' + ArgumentName + ' argument in gallery_link');
    Result := '';
    ParenthesisDepth := 0;
    SawTopLevelString := false;
    while ArgPosition <= Length(Block) do
    begin
      if (Block[ArgPosition] = '/') and (ArgPosition < Length(Block)) and
        (Block[ArgPosition + 1] = '/') then
      begin
        Inc(ArgPosition, 2);
        while (ArgPosition <= Length(Block)) and
          (Block[ArgPosition] <> #10) and (Block[ArgPosition] <> #13) do
          Inc(ArgPosition);
      end else
      if (Block[ArgPosition] = '/') and (ArgPosition < Length(Block)) and
        (Block[ArgPosition + 1] = '*') then
      begin
        Inc(ArgPosition, 2);
        while (ArgPosition < Length(Block)) and
          ((Block[ArgPosition] <> '*') or (Block[ArgPosition + 1] <> '/')) do
          Inc(ArgPosition);
        if ArgPosition < Length(Block) then
          Inc(ArgPosition, 2);
      end else
      if Block[ArgPosition] = '#' then
      begin
        while (ArgPosition <= Length(Block)) and
          (Block[ArgPosition] <> #10) and (Block[ArgPosition] <> #13) do
          Inc(ArgPosition);
      end else
      if (Block[ArgPosition] = '''') or (Block[ArgPosition] = '"') then
      begin
        QuoteChar := Block[ArgPosition];
        IsTopLevelString := ParenthesisDepth = 0;
        ClosedString := false;
        if IsTopLevelString then
          SawTopLevelString := true;
        Inc(ArgPosition);
        while ArgPosition <= Length(Block) do
        begin
          if Block[ArgPosition] = '\' then
          begin
            if ArgPosition < Length(Block) then
            begin
              Inc(ArgPosition);
              if (Block[ArgPosition] = QuoteChar) or
                (Block[ArgPosition] = '\') then
              begin
                if IsTopLevelString then
                  Result += Block[ArgPosition];
              end else
              begin
                if IsTopLevelString then
                begin
                  Result += '\';
                  Result += Block[ArgPosition];
                end;
              end;
              Inc(ArgPosition);
            end else
            begin
              if IsTopLevelString then
                Result += '\';
              Inc(ArgPosition);
            end;
          end else
          if Block[ArgPosition] = QuoteChar then
          begin
            Inc(ArgPosition);
            ClosedString := true;
            Break;
          end else
          begin
            if IsTopLevelString then
              Result += Block[ArgPosition];
            Inc(ArgPosition);
          end;
        end;
        if not ClosedString then
          raise Exception.Create('Unterminated ' + ArgumentName +
            ' string in gallery_link');
      end else
      if Block[ArgPosition] = '(' then
      begin
        Inc(ParenthesisDepth);
        Inc(ArgPosition);
      end else
      if Block[ArgPosition] = ')' then
      begin
        if ParenthesisDepth = 0 then
          Break;
        Dec(ParenthesisDepth);
        Inc(ArgPosition);
      end else
      if (Block[ArgPosition] = ',') and (ParenthesisDepth = 0) then
        Break
      else
        Inc(ArgPosition);
    end;
    if not SawTopLevelString then
      raise Exception.Create('No string value found for ' + ArgumentName +
        ' argument in gallery_link');
  end;

  procedure RequireComma;
  begin
    SkipWhitespaceAndComments;
    if (ArgPosition > Length(Block)) or (Block[ArgPosition] <> ',') then
      raise Exception.Create('Expected comma between gallery_link arguments');
    Inc(ArgPosition);
  end;

begin
  ArgPosition := 1;
  Result.Name := Trim(ReadStringArgument('title'));
  RequireComma;
  Result.Description := Trim(ReadStringArgument('description'));
  RequireComma;
  Result.ImageUrl := Trim(ReadStringArgument('image'));
  RequireComma;
  Result.RepoUrl := Trim(ReadStringArgument('page'));
end;

procedure TImportPackageForm.ParseJsonLibrary(const Content: String);
var
  JSONData: TJSONData;
  LibraryComponents: TJSONArray;
  Component: TJSONObject;
  Field: TJSONData;
  ComponentInfo: TPackageInfo;
  I: Integer;

  function RequiredString(const ObjectData: TJSONObject;
    const FieldName: String): String;
  var
    FieldData: TJSONData;
  begin
    FieldData := ObjectData.Find(FieldName);
    if (FieldData = nil) or (FieldData.JSONType <> jtString) then
      raise Exception.Create('Invalid component field "' + FieldName + '"');
    Result := FieldData.AsString;
  end;

begin
  JSONData := GetJSON(Content);
  try
    if JSONData.JSONType <> jtObject then
      raise Exception.Create('Library JSON root must be an object');

    Field := TJSONObject(JSONData).Find('components');
    if (Field = nil) or (Field.JSONType <> jtArray) then
      raise Exception.Create('Library JSON must contain a components array');
    LibraryComponents := TJSONArray(Field);

    ComponentList.Clear;
    for I := 0 to LibraryComponents.Count - 1 do
    begin
      if LibraryComponents[I].JSONType <> jtObject then
        raise Exception.Create('Library component must be an object');

      Component := TJSONObject(LibraryComponents[I]);
      ComponentInfo.Name := RequiredString(Component, 'title');
      ComponentInfo.Description := RequiredString(Component, 'description');
      ComponentInfo.ImageUrl := RequiredString(Component, 'image');
      ComponentInfo.RepoUrl := RequiredString(Component, 'page');
      ComponentList.Add(ComponentInfo);
      WritelnLog('ImportPackage', 'Found component: ' + ComponentInfo.Name);
    end;
  finally
    JSONData.Free;
  end;
end;

procedure TImportPackageForm.ParseFallbackLibrary(const Content: String);
var
  Regex: TRegExpr;
  BlockStart, BlockEnd: Integer;
  Block: String;
  ComponentInfo: TPackageInfo;
begin
  ComponentList.Clear;
  Regex := TRegExpr.Create;
  try
    Regex.ModifierM := true;
    Regex.ModifierG := true;
    Regex.Expression := '<\?php\s+gallery_link\s*\(';

    if Regex.Exec(Content) then
    begin
      repeat
        BlockStart := Regex.MatchPos[0] + Regex.MatchLen[0];

        { Find the end of the PHP block }
        BlockEnd := BlockStart;
        while (BlockEnd < Length(Content)) and (Copy(Content, BlockEnd, 2) <> '?>') do
          Inc(BlockEnd);

        if BlockEnd < Length(Content) then
        begin
          Block := Copy(Content, BlockStart, BlockEnd - BlockStart);
          ComponentInfo := ParseComponentBlock(Block);

          if (ComponentInfo.Name <> '') and (ComponentInfo.ImageUrl <> '') then
          begin
            ComponentList.Add(ComponentInfo);
            WritelnLog('ImportPackage', 'Found component: ' + ComponentInfo.Name);
          end;
        end;
      until not Regex.ExecNext;
    end;
    if ComponentList.Count = 0 then
      raise Exception.Create('No gallery_link components found in fallback library');
  finally
    Regex.Free;
  end;
end;

procedure TImportPackageForm.DownloadAndParseLibrary;
var
  Content: String;

  function DownloadContent(const Url: String): String;
  var
    DownloadStream: TStream;
  begin
    DownloadStream := Download(Url);
    try
      SetLength(Result, DownloadStream.Size);
      if DownloadStream.Size > 0 then
        DownloadStream.ReadBuffer(Result[1], DownloadStream.Size);
    finally
      DownloadStream.Free;
    end;
  end;

begin
  CancelImageDownloads;
  try
    { Enable blocking downloads only while fetching the library data. }
    EnableBlockingDownloads := true;
    try
      try
        WritelnLog('ImportPackage', 'Downloading library from: ' + LibraryUrl);
        Content := DownloadContent(LibraryUrl);
        WritelnLog('ImportPackage', 'Downloaded ' + IntToStr(Length(Content)) + ' bytes');
        ParseJsonLibrary(Content);
      except
        on E: Exception do
        begin
          WritelnWarning('ImportPackage', 'Failed to download/parse JSON library: ' +
            E.Message + '; trying fallback URL');
          try
            WritelnLog('ImportPackage', 'Downloading fallback library from: ' +
              LibraryFallbackUrl);
            Content := DownloadContent(LibraryFallbackUrl);
            WritelnLog('ImportPackage', 'Downloaded ' + IntToStr(Length(Content)) + ' bytes');
            ParseFallbackLibrary(Content);
          except
            on FallbackException: Exception do
            begin
              WritelnWarning('ImportPackage', 'Failed to download/parse fallback library: ' +
                FallbackException.Message);
              raise Exception.Create('JSON library failed: ' + E.Message +
                '; fallback library failed: ' + FallbackException.Message);
            end;
          end;
        end;
      end;
    finally
      EnableBlockingDownloads := false;
    end;

    WritelnLog('ImportPackage', 'Parsed ' + IntToStr(ComponentList.Count) + ' components');
    DisplayComponents;
  except
    on E: Exception do
    begin
      WritelnWarning('ImportPackage', 'Failed to download/parse library: ' + E.Message);
      ErrorBox('Failed to download library: ' + E.Message);
    end;
  end;
end;

procedure TImportPackageForm.DisplayComponents;
var
  I: Integer;
  ComponentPanel: TPackagePanel;
  TopPosition: Integer;
  PanelHeight: Integer;
  ImageUrl: String;
begin
  CancelImageDownloads;
  { Clear existing panels }
  while ScrollBoxPackages.ControlCount > 0 do
    ScrollBoxPackages.Controls[0].Free;
  
  TopPosition := 8;
  PanelHeight := 160;
  
  for I := 0 to ComponentList.Count - 1 do
  begin
    ComponentPanel := TPackagePanel.Create(ScrollBoxPackages);
    ComponentPanel.Parent := ScrollBoxPackages;
    ComponentPanel.Left := 8;
    ComponentPanel.Top := TopPosition;
    ComponentPanel.Width := ScrollBoxPackages.ClientWidth - 16;
    ComponentPanel.Height := PanelHeight;
    ComponentPanel.BorderStyle := bsSingle;
    ComponentPanel.BevelOuter := bvNone;
    
    { Create image on the left }
    ComponentPanel.Image := TImage.Create(ComponentPanel);
    ComponentPanel.Image.Parent := ComponentPanel;
    ComponentPanel.Image.Left := 8;
    ComponentPanel.Image.Top := 8;
    ComponentPanel.Image.Width := 100;
    ComponentPanel.Image.Height := 100;
    ComponentPanel.Image.Stretch := true;
    ComponentPanel.Image.Proportional := true;
    ComponentPanel.Image.Center := true;
    
    { Create name label (bigger text) }
    ComponentPanel.LabelName := TLabel.Create(ComponentPanel);
    ComponentPanel.LabelName.Parent := ComponentPanel;
    ComponentPanel.LabelName.Left := 120;
    ComponentPanel.LabelName.Top := 8;
    ComponentPanel.LabelName.Font.Size := 14;
    ComponentPanel.LabelName.Font.Style := [fsBold];
    ComponentPanel.LabelName.Caption := ComponentList[I].Name;
    ComponentPanel.LabelName.AutoSize := true;
    
    { Create description label (smaller text) }
    ComponentPanel.LabelDescription := TPackageDescription.Create(ComponentPanel);
    ComponentPanel.LabelDescription.Parent := ComponentPanel;
    ComponentPanel.LabelDescription.Left := 120;
    ComponentPanel.LabelDescription.Top := 30;
    ComponentPanel.LabelDescription.Width := ComponentPanel.Width - 200;
    ComponentPanel.LabelDescription.Height := 80;
    ComponentPanel.LabelDescription.SetHtml(ComponentList[I].Description);

    { Create a clickable repository link when the package has a repository URL }
    ComponentPanel.RepoUrl := ComponentList[I].RepoUrl;
    if ComponentPanel.RepoUrl <> '' then
    begin
      ComponentPanel.LabelRepo := TLabel.Create(ComponentPanel);
      ComponentPanel.LabelRepo.Parent := ComponentPanel;
      ComponentPanel.LabelRepo.Left := 120;
      ComponentPanel.LabelRepo.Top := 118;
      ComponentPanel.LabelRepo.Width := ComponentPanel.Width - 240;
      ComponentPanel.LabelRepo.Caption := ComponentPanel.RepoUrl;
      ComponentPanel.LabelRepo.AutoSize := false;
      ComponentPanel.LabelRepo.Font.Color := clBlue;
      ComponentPanel.LabelRepo.Font.Style := [fsUnderline];
      ComponentPanel.LabelRepo.Cursor := crHandPoint;
      ComponentPanel.LabelRepo.OnClick := @ComponentPanel.LabelRepoClick;
    end;
    
    { Create add button }
    ComponentPanel.ButtonAdd := TButton.Create(ComponentPanel);
    ComponentPanel.ButtonAdd.Parent := ComponentPanel;
    ComponentPanel.ButtonAdd.Left := ComponentPanel.Width - 100;
    ComponentPanel.ButtonAdd.Top := ComponentPanel.Height - 40;
    ComponentPanel.ButtonAdd.Width := 80;
    ComponentPanel.ButtonAdd.Height := 30;
    ComponentPanel.ButtonAdd.Caption := 'Add';
    ComponentPanel.ButtonAdd.OnClick := @ComponentPanel.ButtonAddClick;

    TopPosition := TopPosition + PanelHeight + 8;
  end;

  { Start downloads only after every target TImage has been created. }
  for I := 0 to ComponentList.Count - 1 do
  begin
    ComponentPanel := TPackagePanel(ScrollBoxPackages.Controls[I]);
        if LowerCase(ExtractFileExt(ComponentList[I].ImageUrl)) = '.gif' then
      ImageUrl := GalleryImageBaseUrl + ComponentList[I].ImageUrl
    else
      ImageUrl := ImageBaseUrl + ComponentList[I].ImageUrl;
    StartImageDownload(ComponentPanel.Image, ImageUrl);
  end;
  
  WritelnLog('ImportPackage', 'Displayed ' + IntToStr(ComponentList.Count) + ' components');
end;

end.