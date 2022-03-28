{
  Vampyre Imaging Library Demo
  Image Browser (class api, canvas, VCL/LCL interaction)

  This simple viewer application shows usage of high level class interface
  to Imaging library and also drawing images onto standard VCL/LCL TCanvas.
  TImagingCanvas class is also used here.

  In the left part of the window is shell tree view component. Here you can
  select files located in your computer. If the selected file is in one of the
  supported formats it is displayed in the viewer
  area and some information about the file is displayed in the info area.
  If image file contains subimages you can view them too. Select active subimage
  by clicking on buttons with arrows (Previous/Next).

  When supported file is selected in shell tree view it is loaded to
  TMultiImage and converted to ifA8R8G8B8
  data format. Active subimage is then drawn TPainBox component's
  client area using DisplayImage procedure (direct bit copy, no need to
  convert Imaging's data to TGraphic).

  You need ShellCtrls unit and its components installed in Delphi for this demo.
  In BDS 2006 you can find them in Demos\DelphiWin32\VCLWin32\ShellControls
  directory. In some other versions of Delphi it is installed by default during
  IDE installation.
}

unit Main;

{$I ImagingOptions.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ShellCtrls, ExtCtrls, StdCtrls, Buttons, ExtDlgs,
  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingComponents,
  ImagingCanvases,
  ImagingFormats,
  ImagingUtility;

type
  TMainForm = class(TForm)
    ImageList: TImageList;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    InfoPanel: TPanel;
    LabDataFormat: TLabel;
    LabFileFormat: TLabel;
    LabDim: TLabel;
    LabFileName: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Lab1: TLabel;
    ViewPanel: TPanel;
    PaintBox: TPaintBox;
    Tree: TShellTreeView;
    Splitter1: TSplitter;
    Label4: TLabel;
    LabActImage: TLabel;
    StatusBar: TStatusBar;
    BtnPrev: TSpeedButton;
    BtnNext: TSpeedButton;
    BtnFirst: TSpeedButton;
    BtnLast: TSpeedButton;
    BtnSave: TButton;
    SaveDialog: TSavePictureDialog;
    CheckFilter: TCheckBox;
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure BtnPrevClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnFirstClick(Sender: TObject);
    procedure BtnLastClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure ViewPanelResize(Sender: TObject);
    procedure CheckFilterClick(Sender: TObject);
  private
    // Class that holds multiple images (loaded from MNG or DDS files for instance)
    FImage: ImagingClasses.TMultiImage;
    // Canvas for drawing on loaded images
    FImageCanvas: ImagingCanvases.TImagingCanvas;
    // Image background
    FBack: ImagingClasses.TSingleImage;
    // Canvas for background image
    FBackCanvas: ImagingCanvases.TImagingCanvas;
    FFileName: string;
    FLastTime: LongInt;
    FOriginalFormats: array of TImageFormat;
    FOriginalSizes: array of Integer;
    FSupported: Boolean;
 {$IFDEF FPC}
    procedure TreeGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
 {$ENDIF}
  public
    procedure SetSupported;
    procedure SetUnsupported;
    procedure LoadFile;
    procedure FillDefault;
  end;

const
  FillColor = $FFE6F2FF;
  CheckersDensity = 32;
  SUnsupportedFormat = 'Selected item format not supported';

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
uses
  LCLType;
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Caption + ' version ' + Imaging.GetVersionStr;
  FImage := TMultiImage.Create;
  FImageCanvas := TImagingCanvas.Create;
  FBack := TSingleImage.CreateFromParams(128, 128, ifA8R8G8B8);
  FBackCanvas := FindBestCanvasForImage(FBack).CreateForImage(FBack);
  SetUnsupported;
{$IFDEF FPC}
  Tree.OnGetImageIndex := TreeGetImageIndex;
  Tree.OnGetSelectedIndex := TreeGetSelectedIndex;
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FImage.Free;
  FImageCanvas.Free;
  FBack.Free;
  FBackCanvas.Free;
end;

procedure TMainForm.LoadFile;
var
  I: LongInt;
  T: Int64;
begin
  try
    // DetermineFileFormat reads file header and returns image
    // file format identifier (like 'jpg', 'tga') if file is valid,
    // otherwise empty string is returned
    if Imaging.DetermineFileFormat(FFileName) <> '' then
    try
      // Load all subimages in file
      T := ImagingUtility.GetTimeMicroseconds;
      FImage.LoadMultiFromFile(FFileName);

      if not FImage.AllImagesValid then
      begin
        SetUnsupported;
        Exit;
      end;

      FLastTime := (ImagingUtility.GetTimeMicroseconds - T) div 1000;
      StatusBar.SimpleText := Format('Last image loaded in: %.0n ms', [FLastTime * 1.0]);

      // Store original data formats and sizes for later use
      SetLength(FOriginalFormats, FImage.ImageCount);
      SetLength(FOriginalSizes, FImage.ImageCount);

      for I := 0 to FImage.ImageCount - 1 do
      begin
        FImage.ActiveImage := I;
        FOriginalFormats[I] := FImage.Format;
        FOriginalSizes[I] := FImage.Size;
        // Convert image to 32bit ARGB format if current format is not supported
        // by canvas class
        if not (FImage.Format in TImagingCanvas.GetSupportedFormats) then
          FImage.Format := ifA8R8G8B8;
      end;
      // Activate first image and update UI
      FImage.ActiveImage := 0;
      SetSupported;
      PaintBox.Repaint;
    except
      SetUnsupported;
      raise;
    end
    else
      SetUnsupported;
  except
    SetUnsupported;
  end;
end;

procedure TMainForm.SetSupported;
var
  XRes, YRes: Double;
  ImgSize: Integer;
begin
  // Update image info and enable previous/next buttons
  ImgSize := FOriginalSizes[FImage.ActiveImage];
  if ImgSize > 8192 then
    ImgSize := ImgSize div 1024;
  LabDim.Caption := Format('%dx%d pixels', [FImage.Width, FImage.Height]);
  if GlobalMetadata.GetPhysicalPixelSize(ruDpi, XRes, YRes) then
    LabDim.Caption := LabDim.Caption + Format(' (DPI %.0nx%.0n)', [XRes, YRes]);
  LabFileFormat.Caption := Imaging.FindImageFileFormatByName(FFileName).Name;
  LabDataFormat.Caption := Imaging.GetFormatName(FOriginalFormats[FImage.ActiveImage]);
  LabDataFormat.Caption := LabDataFormat.Caption +
    Format('   (Size in memory: %s %s)', [IntToStrFmt(ImgSize), Iff(ImgSize = FOriginalSizes[FImage.ActiveImage], 'B', 'KiB')]);
  LabActImage.Caption := Format('%d/%d', [FImage.ActiveImage + 1, FImage.ImageCount]);
  BtnPrev.Enabled := True;
  BtnNext.Enabled := True;
  BtnFirst.Enabled := True;
  BtnLast.Enabled := True;
  BtnSave.Enabled := True;
  CheckFilter.Enabled := True;
  FSupported := True;
end;

procedure TMainForm.SetUnsupported;
var
  X, Y, Step: LongInt;
begin
  // Set info texts to 'unsupported' and create default image to show
  LabDim.Caption := SUnsupportedFormat;
  LabFileFormat.Caption := SUnsupportedFormat;
  LabDataFormat.Caption := SUnsupportedFormat;
  LabActImage.Caption := '0/0';
  StatusBar.SimpleText := 'No image loaded';
  BtnPrev.Enabled := False;
  BtnNext.Enabled := False;
  BtnFirst.Enabled := False;
  BtnLast.Enabled := False;
  BtnSave.Enabled := False;
  CheckFilter.Enabled := False;
  FSupported := False;

  if Assigned(FImage) then
  begin
    FImage.CreateFromParams(CheckersDensity, CheckersDensity, ifA8R8G8B8, 1);
    FImageCanvas.Free;
    FImageCanvas := FindBestCanvasForImage(FImage).CreateForImage(FImage);

    Step := FImage.Width div CheckersDensity;
    for Y := 0 to CheckersDensity - 1 do
      for X := 0 to CheckersDensity - 1 do
      begin
        FImageCanvas.FillColor32 := IffUnsigned((Odd(X) and not Odd(Y)) or (not Odd(X) and Odd(Y)),
          pcWhite, pcGray);
        FImageCanvas.FillRect(Rect(X * Step, Y * Step, (X + 1) * Step, (Y + 1) * Step));
      end;
  end;
  // Paint current image
  PaintBox.Repaint;
end;

procedure TMainForm.BtnPrevClick(Sender: TObject);
begin
  FImage.ActiveImage := FImage.ActiveImage - 1;
  SetSupported;
  PaintBox.Repaint;
end;

procedure TMainForm.BtnSaveClick(Sender: TObject);
var
  CopyPath: string;
begin
  SaveDialog.Filter := Imaging.GetImageFileFormatsFilter(False);
  SaveDialog.FileName := ChangeFileExt(ExtractFileName(FFileName), '');
  SaveDialog.FilterIndex := Imaging.GetFileNameFilterIndex(FFileName, False);
  if SaveDialog.Execute then
  begin
    CopyPath := ChangeFileExt(SaveDialog.FileName, '.' +
      Imaging.GetFilterIndexExtension(SaveDialog.FilterIndex, False));
    FImage.SaveMultiToFile(CopyPath);
  end;
end;

procedure TMainForm.CheckFilterClick(Sender: TObject);
begin
  PaintBox.Repaint;
end;

procedure TMainForm.BtnFirstClick(Sender: TObject);
begin
  FImage.ActiveImage := 0;
  SetSupported;
  PaintBox.Repaint;
end;

procedure TMainForm.BtnLastClick(Sender: TObject);
begin
  FImage.ActiveImage := FImage.ImageCount - 1;
  SetSupported;
  PaintBox.Repaint;
end;

procedure TMainForm.BtnNextClick(Sender: TObject);
begin
  FImage.ActiveImage := FImage.ActiveImage + 1;
  SetSupported;
  PaintBox.Repaint;
end;

procedure TMainForm.TreeChange(Sender: TObject; Node: TTreeNode);
begin
  // Selected item in the shell tree view has been changed
  // we check whether the selected item is valid file in one of the
  // supported formats
  FFileName := Tree.Path;
  LabFileName.Caption := ExtractFileName(FFileName);
  if FileExists(FFileName) and Assigned(Imaging.FindImageFileFormatByName(FFileName)) then
    LoadFile
  else
    SetUnsupported;
end;

{$IFDEF FPC}
procedure TMainForm.TreeGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if Node.HasChildren then
    Node.ImageIndex := 1
  else if IsFileFormatSupported(Node.Text) then
    Node.ImageIndex := 0;
end;

procedure TMainForm.TreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;
{$ENDIF}

procedure TMainForm.TreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FImage.ImageCount > 1 then
  begin
    if Key = VK_SPACE then
      BtnNextClick(Self);
  end;
end;

procedure TMainForm.ViewPanelResize(Sender: TObject);
begin
  // Resize background image to fit the paint box
  FBack.Resize(PaintBox.ClientWidth, PaintBox.ClientHeight, rfNearest);
  // Update back canvas state after resizing of associated image
  FBackCanvas.UpdateCanvasState;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  R: TRect;
  Filter: TResizeFilter;
begin
  // Fill background with default color
  FillDefault;

  // Determine which stretching filter to use
  if FSupported and CheckFilter.Checked then
    Filter := rfBicubic
  else
    Filter := rfNearest;
  // Scale image to fit the paint box
  R := ImagingUtility.ScaleRectToRect(FImage.BoundsRect, PaintBox.ClientRect);
  // Create canvas for current image frame
  FImageCanvas.Free;
  FImageCanvas := FindBestCanvasForImage(FImage).CreateForImage(FImage);
  // Stretch image over background canvas
  FImageCanvas.StretchDrawAlpha(FImage.BoundsRect, FBackCanvas, R, Filter);

  // Draw image to canvas (without conversion) using OS drawing functions.
  // Note that DisplayImage only supports images in ifA8R8G8B8 format so
  // if you have image in different format you must convert it or
  // create standard TBitmap by calling ImagingComponents.ConvertImageToBitmap
  ImagingComponents.DisplayImage(PaintBox.Canvas, PaintBox.BoundsRect, FBack);
end;

procedure TMainForm.FillDefault;
begin
  // Fill background canvas with default color
  FBackCanvas.FillColor32 := FillColor;
  FBackCanvas.FillRect(Rect(0, 0, FBack.Width, FBack.Height));
end;

{
  File Notes:

  -- 0.80 Changes/Bug Fixes ---------------------------------
    - Added Lazarus support so dropped "VCL" prefix.

  -- 0.77 Changes/Bug Fixes ---------------------------------
    - Displays size of image in memory.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - Displays image physical resolution if present.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Creates best canvas class for given image for faster
      blending and scaling.

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Added alpha blended drawing with optional filtered stretching.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Added Save Image Copy button and related stuff.
    - Added XP controls manifest (no TXPManifest since its not
      in older Delphis).
    - Wrong active image index was shown sometimes after several
      clicks on Prev/Next buttons.
    - Added First/Last subimage buttons.
    - Original data format of subimages at index >1 is displayed right now
      (was always A8R8G8B8)
    - Space key now shows next subimage if multi-images is loaded.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added canvas usage too
    - added support for viewing multiimages (like MNG)
    - change drawing to use stuff from ImagingComponents unit instead of
      converting to TBitmap
    - changed demo to use high level interface instead of low level

}

end.
