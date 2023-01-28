{
  Vampyre Imaging Library Demo
  LCL Imager (ObjectPascal, high level/component sets/canvas, Win32/Linux/macOS)
  tested in Lazarus 1.8.4 (Windows; Linux: Gtk2, Qt; macOS: Carbon, Cocoa)
  written by Marek Mauder

  Simple image manipulator program which shows usage of Imaging VCL/LCL
  classes (TImagingGraphic and its descendants) to display images on form.
  It also uses high level image classes and some low level functions.
  Demo uses TMultiImage class to store images (loaded from one file - MNG, DDS)
  which can be modified by user. After each modification image
  is assigned to TImagingBitmap class which provides visualization
  on the app form (using standard TImage component). Demo also uses new
  TImagingCanvas class to do some effects.
  
  In File menu you can open new image and save the current one. Items in
  View menu provide information about the current image and controls
  how it is displayed. You can also select next and previous subimage if loaded file
  contains more than one image. Format menu allows you to convert image
  to different image data formats supported by Imaging. Manipulate
  menu allows you to enlarge/shrink/flip/mirror/swap channels/other
  of the current image. Effects menu allows you to apply various effects to the
  image (provided by TImagingCanvas).
}

unit MainUnit;
      
{$I ImagingOptions.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Variants,
  Menus, ExtCtrls, ExtDlgs, DemoUtils, AboutUnit, ActnList, StdCtrls, ComCtrls,
  PairSplitter, FileUtil,

  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingComponents,
  ImagingCanvases,
  ImagingBinary,
  ImagingUtility;

type
  TManipulationType = (mtFlip, mtMirror, mtRotate90CW, mtRotate90CCW,
    mtFreeRotate, mtResize50, mtResize200, mtFreeResize,
    mtSwapRB, mtSwapRG, mtSwapGB, mtReduce1024,
    mtReduce256, mtReduce64, mtReduce16, mtReduce2);
  TPointTransform = (ptInvert, ptIncContrast, ptDecContrast, ptIncBrightness,
    ptDecBrightness, ptIncGamma, ptDecGamma, ptThreshold, ptLevelsLow,
    ptLevelsHigh, ptAlphaPreMult, ptAlphaUnPreMult);
  TNonLinearFilter = (nfMedian, nfMin, nfMax);
  TMorphology = (mpErode, mpDilate, mpOpen, mpClose);
  TAdditionalOp = (aoOtsuThreshold, aoDeskew);

  { TMainForm }
  TMainForm = class(TForm)
    ActViewInfo: TAction;
    ActViewFitToWindow: TAction;
    ActViewActualSize: TAction;
    ActionList: TActionList;
    Image: TImage;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    FormatItem: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MIMorphology: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    AlphaItem: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItemConvertAll: TMenuItem;
    MIAdditional: TMenuItem;
    PairSplitter: TPairSplitter;
    PairSplitterSideLeft: TPairSplitterSide;
    PairSplitterSideRight: TPairSplitterSide;
    RedItem: TMenuItem;
    GreenItem: TMenuItem;
    BlueItem: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItemActSubImage: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    StatusBar: TStatusBar;
    TreeImage: TTreeView;
    procedure ActViewFitToWindowExecute(Sender: TObject);
    procedure ActViewInfoExecute(Sender: TObject);
    procedure ActViewActualSizeExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure ImageClick(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem31Click(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure MenuItem35Click(Sender: TObject);
    procedure MenuItem37Click(Sender: TObject);
    procedure MenuItem38Click(Sender: TObject);
    procedure MenuItem39Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem40Click(Sender: TObject);
    procedure MenuItem41Click(Sender: TObject);
    procedure MenuItem42Click(Sender: TObject);
    procedure MenuItem43Click(Sender: TObject);
    procedure MenuItem44Click(Sender: TObject);
    procedure MenuItem45Click(Sender: TObject);
    procedure MenuItem46Click(Sender: TObject);
    procedure MenuItem47Click(Sender: TObject);
    procedure MenuItem48Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem50Click(Sender: TObject);
    procedure MenuItem51Click(Sender: TObject);
    procedure MenuItem53Click(Sender: TObject);
    procedure MenuItem54Click(Sender: TObject);
    procedure MenuItem56Click(Sender: TObject);
    procedure MenuItem57Click(Sender: TObject);
    procedure MenuItem58Click(Sender: TObject);
    procedure MenuItem59Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem60Click(Sender: TObject);
    procedure MenuItem61Click(Sender: TObject);
    procedure MenuItem62Click(Sender: TObject);
    procedure MenuItem64Click(Sender: TObject);
    procedure MenuItem65Click(Sender: TObject);
    procedure MenuItem66Click(Sender: TObject);
    procedure MenuItem67Click(Sender: TObject);
    procedure MenuItem68Click(Sender: TObject);
    procedure MenuItem69Click(Sender: TObject);
    procedure MenuItem70Click(Sender: TObject);
    procedure MenuItem71Click(Sender: TObject);
    procedure MenuItem72Click(Sender: TObject);
    procedure MenuItem73Click(Sender: TObject);
    procedure MenuItem74Click(Sender: TObject);
    procedure MenuItem75Click(Sender: TObject);
    procedure MenuItem76Click(Sender: TObject);
    procedure MenuItem78Click(Sender: TObject);
    procedure MenuItem79Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure FormatChangeClick(Sender: TObject);
    procedure ChannelSetClick(Sender: TObject);
    procedure MenuItem80Click(Sender: TObject);
    procedure MenuItem82Click(Sender: TObject);
    procedure MenuItem83Click(Sender: TObject);
    procedure MenuItem84Click(Sender: TObject);
    procedure MenuItem85Click(Sender: TObject);
    procedure MenuItem86Click(Sender: TObject);
    procedure MenuItem88Click(Sender: TObject);
    procedure MenuItem89Click(Sender: TObject);
    procedure MenuItem90Click(Sender: TObject);
    procedure MenuItem91Click(Sender: TObject);
    procedure MenuItem92Click(Sender: TObject);
    procedure TreeImageSelectionChanged(Sender: TObject);
  private
    FBitmap: TImagingBitmap;
    FImage: TMultiImage;
    FImageCanvas: TImagingCanvas;
    FFileName: string;
    FFileSize: Integer;
    FParam1, FParam2, FParam3: Integer;
    procedure OpenFile(const FileName: string);
    procedure SaveFile(const FileName: string);
    procedure SelectSubImage(Index: LongInt);
    procedure UpdateView(RebuildTree: Boolean);
    function CheckCanvasFormat: Boolean;
    procedure ApplyConvolution(Kernel: Pointer; Size: LongInt; NeedsBlur: Boolean);
    procedure ApplyPointTransform(Transform: TPointTransform);
    procedure ApplyManipulation(ManipType: TManipulationType);
    procedure ApplyNonLinear(FilterType: TNonLinearFilter; FilterSize: Integer);
    procedure ApplyMorphology(MorphOp: TMorphology);
    procedure ApplyAdditionalOp(Op: TAdditionalOp);
    procedure MeasureTime(const Msg: string; const OldTime: Int64);
    procedure FreeResizeInput;
    function InputInteger(const ACaption, APrompt: string; var Value: Integer): Boolean;
    procedure BuildImageTree;
  public

  end; 

const
  SWindowTitle = 'LCL Imager - Vampyre Imaging Library %s Demo';
  
var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Item: TMenuItem;
  Fmt: TImageFormat;
  Info: TImageFormatInfo;
  Platform: string;

  function Clone(AItem: TMenuItem): TMenuItem;
  begin
    Result := TMenuItem.Create(MainMenu);
    Result.Caption := AItem.Caption;
    Result.Tag := AItem.Tag;
    Result.OnClick := AItem.OnClick;;
  end;

  procedure AddSetChannelItem(const Caption: string; Value: Integer);
  begin
    Item := TMenuItem.Create(MainMenu);
    Item.Caption := Caption;
    Item.Tag := Value;
    Item.OnClick := ChannelSetClick;
    AlphaItem.Add(Item);
    RedItem.Add(Clone(Item));
    GreenItem.Add(Clone(Item));
    BlueItem.Add(Clone(Item));
  end;

begin
  Platform := '';
{$IF Defined(WIN64)}
  Platform := ' - WIN64';
{$ELSEIF Defined(WIN32)}
  Platform := ' - WIN32';
{$ELSEIF Defined(LINUX)}
  Platform := ' - Linux';
{$ELSEIF Defined(DARWIN)}
  Platform := ' - OSX';
{$ENDIF}

  Caption := Format(SWindowTitle, [Imaging.GetVersionStr]) + Platform;

  { Source image and Image's graphic are created and
    default image is opened.}
  FImage := TMultiImage.Create;
  FBitmap := TImagingBitmap.Create;
  Image.Picture.Graphic := FBitmap;
  FImageCanvas := TImagingCanvas.Create;

  { This builds Format submenu containing all possible
    image data formats (it dos not start at Low(TImageFormat)
    because there are some helper formats). Format for each item
    is stored in its Tag for later use in OnClick event.}
  for Fmt := ifIndex8 to High(TImageFormat) do
  begin
    GetImageFormatInfo(Fmt, Info);
    if Info.Name <> '' then
    begin
      Item := TMenuItem.Create(MainMenu);
      Item.Caption := Info.Name;
      Item.Tag := Ord(Fmt);
      Item.OnClick := FormatChangeClick;
      FormatItem.Add(Item);
    end;
  end;

  AddSetChannelItem('Set to 5%', 12);
  AddSetChannelItem('Set to 50%', 128);
  AddSetChannelItem('Set to 100%', 255);

  // Set 'Fit to window' mode
  ActViewFitToWindowExecute(Self);

  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    OpenFile(ParamStr(1))
  else
    OpenFile(GetDataDir + PathDelim + 'Tigers.jpg');
end;

procedure TMainForm.MenuItem10Click(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.MenuItem12Click(Sender: TObject);
begin
  ApplyManipulation(mtSwapRB);
end;

procedure TMainForm.MenuItem13Click(Sender: TObject);
begin
  ApplyManipulation(mtSwapRG);
end;

procedure TMainForm.MenuItem14Click(Sender: TObject);
begin
  ApplyManipulation(mtSwapGB);
end;

procedure TMainForm.MenuItem15Click(Sender: TObject);
begin
  ApplyManipulation(mtReduce1024);
end;

procedure TMainForm.MenuItem18Click(Sender: TObject);
begin
  ApplyManipulation(mtReduce256);
end;

procedure TMainForm.MenuItem19Click(Sender: TObject);
begin
  ApplyManipulation(mtReduce64);
end;

procedure TMainForm.MenuItem20Click(Sender: TObject);
begin
  ApplyManipulation(mtReduce16);
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
begin
  ApplyManipulation(mtMirror);
end;

procedure TMainForm.MenuItem23Click(Sender: TObject);
begin
  ApplyManipulation(mtRotate90CW);
end;

procedure TMainForm.MenuItem24Click(Sender: TObject);
begin
  ApplyManipulation(mtRotate90CCW);
end;

procedure TMainForm.MenuItem26Click(Sender: TObject);
begin
  FParam1 := Ord(rfNearest);
  ApplyManipulation(mtResize50);
end;

procedure TMainForm.MenuItem27Click(Sender: TObject);
begin
  FParam1 := Ord(rfBilinear);
  ApplyManipulation(mtResize50);
end;

procedure TMainForm.MenuItem28Click(Sender: TObject);
begin
  FParam1 := Ord(rfBicubic);
  ApplyManipulation(mtResize50);
end;

procedure TMainForm.MenuItem29Click(Sender: TObject);
begin
  FParam1 := Ord(rfNearest);
  ApplyManipulation(mtResize200);
end;

procedure TMainForm.MenuItem30Click(Sender: TObject);
begin
  FParam1 := Ord(rfBilinear);
  ApplyManipulation(mtResize200);
end;

procedure TMainForm.MenuItem31Click(Sender: TObject);
begin
  FParam1 := Ord(rfBicubic);
  ApplyManipulation(mtResize200);
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  ApplyManipulation(mtFlip);
end;

procedure TMainForm.MenuItem33Click(Sender: TObject);
begin
  ApplyManipulation(mtReduce2);
end;

procedure TMainForm.MenuItem37Click(Sender: TObject);
begin
  ApplyConvolution(@FilterGaussian3x3, 3, False);
end;

procedure TMainForm.MenuItem38Click(Sender: TObject);
begin
  ApplyConvolution(@FilterGaussian5x5, 5, False);
end;

procedure TMainForm.MenuItem39Click(Sender: TObject);
begin
  ApplyConvolution(@FilterSharpen3x3, 3, False);
end;

procedure TMainForm.MenuItem40Click(Sender: TObject);
begin
  ApplyConvolution(@FilterSharpen5x5, 5, False);
end;

procedure TMainForm.MenuItem41Click(Sender: TObject);
begin
  ApplyConvolution(@FilterLaplace5x5, 5, True);
end;

procedure TMainForm.MenuItem42Click(Sender: TObject);
begin
  ApplyConvolution(@FilterSobelHorz3x3, 3, True);
end;

procedure TMainForm.MenuItem43Click(Sender: TObject);
begin
  ApplyConvolution(@FilterSobelVert3x3, 3, True);
end;

procedure TMainForm.MenuItem44Click(Sender: TObject);
begin
  OpenFile(FFileName);
end;

procedure TMainForm.MenuItem45Click(Sender: TObject);
begin
  ApplyConvolution(@FilterGlow5x5, 5, False);
end;

procedure TMainForm.MenuItem46Click(Sender: TObject);
begin
  ApplyConvolution(@FilterEmboss3x3, 3, True);
end;

procedure TMainForm.MenuItem47Click(Sender: TObject);
begin
  ApplyPointTransform(ptIncContrast);
end;

procedure TMainForm.MenuItem48Click(Sender: TObject);
begin
  ApplyConvolution(@FilterEdgeEnhance3x3, 3, False);
end;

procedure TMainForm.MenuItem50Click(Sender: TObject);
begin
  ApplyConvolution(@FilterPrewittHorz3x3, 3, True);
end;

procedure TMainForm.MenuItem51Click(Sender: TObject);
begin
  ApplyConvolution(@FilterKirshHorz3x3, 3, True);
end;

procedure TMainForm.MenuItem53Click(Sender: TObject);
begin
  ApplyConvolution(@FilterPrewittVert3x3, 3, True);
end;

procedure TMainForm.MenuItem54Click(Sender: TObject);
begin
  ApplyConvolution(@FilterKirshVert3x3, 3, True);
end;

procedure TMainForm.MenuItem56Click(Sender: TObject);
begin
  ApplyPointTransform(ptInvert);
end;

procedure TMainForm.MenuItem57Click(Sender: TObject);
begin
  ApplyPointTransform(ptDecContrast);
end;

procedure TMainForm.MenuItem58Click(Sender: TObject);
begin
  ApplyPointTransform(ptIncBrightness);
end;

procedure TMainForm.MenuItem59Click(Sender: TObject);
begin
  ApplyPointTransform(ptDecBrightness);
end;

procedure TMainForm.MenuItem34Click(Sender: TObject);
begin
  SelectSubImage(FImage.ActiveImage + 1);
end;

procedure TMainForm.MenuItem35Click(Sender: TObject);
begin
  SelectSubImage(FImage.ActiveImage - 1);
end;

function TMainForm.CheckCanvasFormat: Boolean;
begin
  Result := FImage.Format in FImageCanvas.GetSupportedFormats;
  if not Result then
    MessageDlg('Image is in format that is not supported by TImagingCanvas.', mtError, [mbOK], 0);
end;

procedure TMainForm.ApplyConvolution(Kernel: Pointer; Size: LongInt; NeedsBlur: Boolean);
var
  T: Int64;
begin
  if CheckCanvasFormat then
  begin
    FImageCanvas.CreateForImage(FImage);
    T := GetTimeMicroseconds;
    
    if NeedsBlur then
      FImageCanvas.ApplyConvolution3x3(FilterGaussian3x3);
    if Size = 3 then
      FImageCanvas.ApplyConvolution3x3(TConvolutionFilter3x3(Kernel^))
    else
      FImageCanvas.ApplyConvolution5x5(TConvolutionFilter5x5(Kernel^));
      
    MeasureTime('Image convolved in:', T);
    UpdateView(False);
  end;
end;

procedure TMainForm.ApplyPointTransform(Transform: TPointTransform);
var
  T: Int64;
begin
  if CheckCanvasFormat then
  begin
    FImageCanvas.CreateForImage(FImage);
    T := GetTimeMicroseconds;

    case Transform of
      ptInvert:         FImageCanvas.InvertColors;
      ptIncContrast:    FImageCanvas.ModifyContrastBrightness(20, 0);
      ptDecContrast:    FImageCanvas.ModifyContrastBrightness(-20, 0);
      ptIncBrightness:  FImageCanvas.ModifyContrastBrightness(0, 20);
      ptDecBrightness:  FImageCanvas.ModifyContrastBrightness(0, -20);
      ptIncGamma:       FImageCanvas.GammaCorrection(1.2, 1.2, 1.2);
      ptDecGamma:       FImageCanvas.GammaCorrection(0.8, 0.8, 0.8);
      ptThreshold:      FImageCanvas.Threshold(0.5, 0.5, 0.5);
      ptLevelsLow:      FImageCanvas.AdjustColorLevels(0.0, 0.5, 1.0);
      ptLevelsHigh:     FImageCanvas.AdjustColorLevels(0.35, 1.0, 0.9);
      ptAlphaPreMult:   FImageCanvas.PremultiplyAlpha;
      ptAlphaUnPreMult: FImageCanvas.UnPremultiplyAlpha;
    end;

    MeasureTime('Point transform done in:', T);
    UpdateView(False);
  end;
end;

procedure TMainForm.ApplyNonLinear(FilterType: TNonLinearFilter; FilterSize: Integer);
var
  T: Int64;
begin
  if CheckCanvasFormat then
  begin
    FImageCanvas.CreateForImage(FImage);
    T := GetTimeMicroseconds;

    case FilterType of
      nfMedian: FImageCanvas.ApplyMedianFilter(FilterSize);
      nfMin:    FImageCanvas.ApplyMinFilter(FilterSize);
      nfMax:    FImageCanvas.ApplyMaxFilter(FilterSize);
    end;

    MeasureTime('Point transform done in:', T);
    UpdateView(False);
  end;
end;

procedure TMainForm.ApplyMorphology(MorphOp: TMorphology);
var
  T: Int64;
  Strel: TStructElement;
begin
  T := GetTimeMicroseconds;
  FImage.Format := ifGray8;
  OtsuThresholding(FImage.ImageDataPointer^);
  
  SetLength(Strel, 3, 3);
  Strel[0, 0] := 0;
  Strel[1, 0] := 1;
  Strel[2, 0] := 0;
  Strel[0, 1] := 1;
  Strel[1, 1] := 1;
  Strel[2, 1] := 1;
  Strel[0, 2] := 0;
  Strel[1, 2] := 1;
  Strel[2, 2] := 0;

  case MorphOp of
    mpErode:   Morphology(FImage.ImageDataPointer^, Strel, moErode);
    mpDilate:  Morphology(FImage.ImageDataPointer^, Strel, moDilate);
    mpOpen:
      begin
        Morphology(FImage.ImageDataPointer^, Strel, moErode);
        Morphology(FImage.ImageDataPointer^, Strel, moDilate);
      end;
    mpClose:
      begin
        Morphology(FImage.ImageDataPointer^, Strel, moDilate);
        Morphology(FImage.ImageDataPointer^, Strel, moErode);
      end;
  end;
  MeasureTime('Morphology operation applied in:', T);
  UpdateView(True);
end;

procedure TMainForm.ApplyAdditionalOp(Op: TAdditionalOp);
var
  T: Int64;
begin
  T := GetTimeMicroseconds;
  case Op of
    aoOtsuThreshold:
      begin
        FImage.Format := ifGray8;
        OtsuThresholding(FImage.ImageDataPointer^, True);
      end;
    aoDeskew: DeskewImage(FImage.ImageDataPointer^);
  end;
  MeasureTime('Operation completed in:', T);
  UpdateView(False);
end;

procedure TMainForm.ApplyManipulation(ManipType: TManipulationType);
var
  T: Int64;
  OldFmt: TImageFormat;
  OldSize: Integer;
  RebuildTree: Boolean;
begin
  OldFmt := FImage.Format;
  OldSize := FImage.Size;

  T := GetTimeMicroseconds;
  case ManipType of
    mtFlip:             FImage.Flip;
    mtMirror:           FImage.Mirror;
    mtRotate90CW:       FImage.Rotate(-90);
    mtRotate90CCW:      FImage.Rotate(90);
    mtFreeRotate:       FImage.Rotate(FParam1);
    mtResize50:         FImage.Resize(FImage.Width div 2, FImage.Height div 2, TResizeFilter(FParam1));
    mtResize200:        FImage.Resize(FImage.Width * 2, FImage.Height * 2, TResizeFilter(FParam1));
    mtFreeResize:       FImage.Resize(FParam2, FParam3, TResizeFilter(FParam1));
    mtSwapRB:           FImage.SwapChannels(ChannelRed, ChannelBlue);
    mtSwapRG:           FImage.SwapChannels(ChannelRed, ChannelGreen);
    mtSwapGB:           FImage.SwapChannels(ChannelGreen, ChannelBlue);
    mtReduce1024:       ReduceColors(FImage.ImageDataPointer^, 1024);
    mtReduce256:        ReduceColors(FImage.ImageDataPointer^, 256);
    mtReduce64:         ReduceColors(FImage.ImageDataPointer^, 64);
    mtReduce16:         ReduceColors(FImage.ImageDataPointer^, 16);
    mtReduce2:          ReduceColors(FImage.ImageDataPointer^, 2);
  end;
  MeasureTime('Image manipulated in:', T);

  RebuildTree := (FImage.Format <> OldFmt) or (FImage.Size <> OldSize);
  UpdateView(RebuildTree);
end;

procedure TMainForm.FormatChangeClick(Sender: TObject);
var
  T: Int64;
  Fmt: TImageFormat;
begin
  with Sender as TMenuItem do
  begin
    T := GetTimeMicroseconds;
    Fmt := TImageFormat(Tag);
    if MenuItemConvertAll.Checked then
      FImage.ConvertImages(Fmt)
    else
      FImage.Format := Fmt;
    MeasureTime('Image converted in:', T);
    UpdateView(True);
  end;
end;

procedure TMainForm.ChannelSetClick(Sender: TObject);
var
  T: Int64;
  Canvas: TImagingCanvas;
  ChanId: Integer;
begin
  if CheckCanvasFormat then
  with Sender as TMenuItem do
  begin
    case Parent.Caption[1] of
      'A': ChanId := ChannelAlpha;
      'R': ChanId := ChannelRed;
      'G': ChanId := ChannelGreen;
      'B': ChanId := ChannelBlue;
    else
      ChanId := ChannelRed;
    end;

    Canvas := TImagingCanvas.CreateForImage(FImage);

    T := GetTimeMicroseconds;
    Canvas.FillChannel(ChanId, Tag);
    MeasureTime('Channel filled in:', T);

    Canvas.Free;
    UpdateView(False);
  end;
end;

procedure TMainForm.MenuItem80Click(Sender: TObject);
begin
  if InputInteger('Free Rotate', 'Enter angle in degrees:', FParam1) then
    ApplyManipulation(mtFreeRotate);
end;

procedure TMainForm.FreeResizeInput;
begin
  if InputInteger('Free Resize', 'Enter width in pixels', FParam2) and
   InputInteger('Free Resize', 'Enter height in pixels', FParam3) then
  begin
    ApplyManipulation(mtFreeResize);
  end;
end;

function TMainForm.InputInteger(const ACaption, APrompt: string;
  var Value: Integer): Boolean;
var
  StrVal: string;
begin
  Result := False;
  StrVal := '';

  if Dialogs.InputQuery(ACaption, APrompt, StrVal) then
  begin
    if TryStrToInt(StrVal, Value) then
      Exit(True)
    else
      MessageDlg('Cannot convert input to number', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.MenuItem82Click(Sender: TObject);
var
  T: Int64;
  Canvas: TImagingCanvas;
  Red, Green, Blue, Alpha, Gray: THistogramArray;
  I, MaxPixels: Integer;
  Factor: Single;

  procedure VisualizeHistogram(const Histo: THistogramArray; Color: TColor32; Offset: Integer);
  var
    I: Integer;
  begin
    Canvas.PenColor32 := Color;
    for I := 0 to 255 do
      Canvas.VertLine(I + Offset, 256 - Round(Histo[I] * Factor), 255);
  end;

begin
  if CheckCanvasFormat then
  begin
    Canvas := TImagingCanvas.CreateForImage(FImage);

    T := GetTimeMicroseconds;
    Canvas.GetHistogram(Red, Green, Blue, Alpha, Gray);
    MeasureTime('Histograms computed in:', T);

    FImage.ActiveImage := FImage.AddImage(1024, 256, ifA8R8G8B8);
    Canvas.CreateForImage(FImage);
    Canvas.FillColor32 := pcBlack;
    Canvas.FillRect(FImage.BoundsRect);

    MaxPixels := 0;
    for I := 0 to 255 do
      if Red[I] > MaxPixels then MaxPixels := Red[I];
    for I := 0 to 255 do
      if Green[I] > MaxPixels then MaxPixels := Green[I];
    for I := 0 to 255 do
      if Blue[I] > MaxPixels then MaxPixels := Blue[I];
    for I := 0 to 255 do
      if Gray[I] > MaxPixels then MaxPixels := Gray[I];

    Factor := 256 / MaxPixels;

    VisualizeHistogram(Red, pcRed, 0);
    VisualizeHistogram(Green, pcGreen, 256);
    VisualizeHistogram(Blue, pcBlue, 512);
    VisualizeHistogram(Gray, pcGray, 768);

    Canvas.Free;
    UpdateView(True);
  end;
end;

procedure TMainForm.MenuItem83Click(Sender: TObject);
begin
  FParam1 := Ord(rfNearest);
  FreeResizeInput;
end;

procedure TMainForm.MenuItem84Click(Sender: TObject);
begin
  FParam1 := Ord(rfBilinear);
  FreeResizeInput;
end;

procedure TMainForm.MenuItem85Click(Sender: TObject);
begin
  FParam1 := Ord(rfBicubic);
  FreeResizeInput;
end;

procedure TMainForm.MenuItem86Click(Sender: TObject);
var
  Form: TForm;
  Memo: TMemo;
  I: Integer;
  Item: TMetadataItem;
  S: string;
begin
  Form := TForm.Create(Self);
  Form.BorderIcons := [biSystemMenu];
  Form.Caption := 'Detected Image Metadata';
  Form.Position := poOwnerFormCenter;
  Form.Width := 512;
  Form.Height := 512;
  Memo := TMemo.Create(Form);
  Memo.Parent := Form;
  Memo.Align := alClient;
  Memo.ReadOnly := True;
  Memo.ScrollBars := ssVertical;

  if GlobalMetadata.MetaItemCount > 0 then
  begin
    for I := 0 to GlobalMetadata.MetaItemCount - 1 do
    begin
      Item := GlobalMetadata.MetaItemsByIdx[I];
      S := Format('%s (idx: %d, type: %s): %s', [Item.Id, Item.ImageIndex,
        VarTypeAsText(VarType(Item.Value)), VarToStrDef(Item.Value, 'couldn''t convert Variant to string')]);
      Memo.Lines.Add(S);
    end;
  end
  else
    Memo.Lines.Add('No metadata loaded for this image');

  Form.ShowModal;
  Form.Free;
end;

procedure TMainForm.MenuItem88Click(Sender: TObject);
begin
  FParam1 := Ord(rfLanczos);
  ApplyManipulation(mtResize50);
end;

procedure TMainForm.MenuItem89Click(Sender: TObject);
begin
  FParam1 := Ord(rfLanczos);
  ApplyManipulation(mtResize200);
end;

procedure TMainForm.MenuItem90Click(Sender: TObject);
begin
  FParam1 := Ord(rfLanczos);
  FreeResizeInput;
end;

procedure TMainForm.MenuItem91Click(Sender: TObject);
begin
  ApplyAdditionalOp(aoDeskew);
end;

procedure TMainForm.MenuItem92Click(Sender: TObject);
var
  Images: TMultiImage;
begin
  OpenDialog.Filter := GetImageFileFormatsFilter(True);
  if OpenDialog.Execute then
  begin
    Images := TMultiImage.Create;
    try
      Images.LoadMultiFromFile(OpenDialog.FileName);
      FImage.AddImages(Images.DataArray);
      BuildImageTree;
      SelectSubImage(FImage.ActiveImage);
    finally
      Images.Free;
    end;
  end;
end;

procedure TMainForm.TreeImageSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeImage.Selected;
  if Node <> nil then
    SelectSubImage(PtrInt(Node.Data));
end;

procedure TMainForm.ActViewActualSizeExecute(Sender: TObject);
begin
  ActViewActualSize.Checked := True;
  ActViewFitToWindow.Checked := False;
  Image.Proportional := False;
  Image.Stretch := False;
end;

procedure TMainForm.ActViewFitToWindowExecute(Sender: TObject);
begin
  ActViewFitToWindow.Checked := True;
  ActViewActualSize.Checked := False;
  Image.Proportional := True;
  Image.Stretch := True;
end;

procedure TMainForm.ActViewInfoExecute(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  // For some strange reason ordinary MessageDlg sometimes shows empty message for
  // A8R8G8B8 images. Using Win32 msg box instead now.
  MessageBox(Handle, PChar(ImageToStr(FImage.ImageDataPointer^)), 'Image information', MB_OK or MB_ICONINFORMATION);
{$ELSE}
  MessageDlg(ImageToStr(FImage.ImageDataPointer^), mtInformation, [mbOK], 0);
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FImageCanvas.Free;
  FBitmap.Free;
  FImage.Free;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  if Length(FileNames) > 0 then
    OpenFile(FileNames[0]);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if ClientWidth > 600 then
    PairSplitterSideLeft.Width := 280;
  WindowState := wsMaximized;
end;

procedure TMainForm.ImageClick(Sender: TObject);
begin
  ActViewInfo.Execute;
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
begin
  OpenDialog.Filter := GetImageFileFormatsFilter(True);
  if OpenDialog.Execute then
    OpenFile(OpenDialog.FileName);
end;

procedure TMainForm.MenuItem5Click(Sender: TObject);
begin
  SaveDialog.Filter := GetImageFileFormatsFilter(False);
  SaveDialog.FileName := ChangeFileExt(ExtractFileName(FFileName), '');
  SaveDialog.FilterIndex := GetFileNameFilterIndex(FFileName, False);
  if SaveDialog.Execute then
  begin
    FFileName := ChangeFileExt(SaveDialog.FileName, '.' + GetFilterIndexExtension(SaveDialog.FilterIndex, False));
    SaveFile(FFileName);
  end;
end;

procedure TMainForm.MenuItem60Click(Sender: TObject);
begin
  ApplyPointTransform(ptIncGamma);
end;

procedure TMainForm.MenuItem61Click(Sender: TObject);
begin
  ApplyPointTransform(ptDecGamma);
end;

procedure TMainForm.MenuItem62Click(Sender: TObject);
begin
  ApplyPointTransform(ptThreshold);
end;

procedure TMainForm.MenuItem64Click(Sender: TObject);
begin
  ApplyNonLinear(nfMedian, 3);
end;

procedure TMainForm.MenuItem65Click(Sender: TObject);
begin
  ApplyNonLinear(nfMedian, 5);
end;

procedure TMainForm.MenuItem66Click(Sender: TObject);
begin
  ApplyNonLinear(nfMin, 3);
end;

procedure TMainForm.MenuItem67Click(Sender: TObject);
begin
  ApplyNonLinear(nfMin, 5);
end;

procedure TMainForm.MenuItem68Click(Sender: TObject);
begin
  ApplyNonLinear(nfMax, 3);
end;

procedure TMainForm.MenuItem69Click(Sender: TObject);
begin
  ApplyNonLinear(nfMax, 5);
end;

procedure TMainForm.MenuItem70Click(Sender: TObject);
begin
  ApplyAdditionalOp(aoOtsuThreshold);
end;

procedure TMainForm.MenuItem71Click(Sender: TObject);
begin
  ApplyMorphology(mpErode);
end;

procedure TMainForm.MenuItem72Click(Sender: TObject);
begin
  ApplyMorphology(mpDilate);
end;

procedure TMainForm.MenuItem73Click(Sender: TObject);
begin
  ApplyMorphology(mpOpen);
end;

procedure TMainForm.MenuItem74Click(Sender: TObject);
begin
  ApplyMorphology(mpClose);
end;

procedure TMainForm.MenuItem75Click(Sender: TObject);
begin
  ApplyPointTransform(ptLevelsLow);
end;

procedure TMainForm.MenuItem76Click(Sender: TObject);
begin
  ApplyPointTransform(ptLevelsHigh);
end;

procedure TMainForm.MenuItem78Click(Sender: TObject);
begin
  ApplyPointTransform(ptAlphaPreMult);
end;

procedure TMainForm.MenuItem79Click(Sender: TObject);
begin
  ApplyPointTransform(ptAlphaUnPreMult);
end;

procedure TMainForm.MenuItem7Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.OpenFile(const FileName: string);
var
  T: Int64;
begin
  FFileName := FileName;
  try
    T := GetTimeMicroseconds;
    GlobalMetadata.ClearMetaItems;
    FImage.LoadMultiFromFile(FileName);
    FFileSize := FileSize(FileName);
    BuildImageTree;
    GlobalMetadata.CopyLoadedMetaItemsForSaving;
    MeasureTime(Format('File %s opened in:', [ExtractFileName(FileName)]), T);
  except
    MessageDlg(GetExceptObject.Message, mtError, [mbOK], 0);
    FImage.CreateFromParams(32, 32, ifA8R8G8B8, 1);
    TreeImage.Items.Clear;
  end;
  SelectSubImage(0);
end;

procedure TMainForm.BuildImageTree;
var
  Root, Node: TTreeNode;
  I: PtrInt;
  Lab: string;
  Data: TImageData;
begin
  TreeImage.Items.Clear;

  Lab := Format('%s (%d images)', [ExtractFileName(FFileName), FImage.ImageCount]);
  Root := TreeImage.Items.Add(nil, Lab);

  for I := 0 to FImage.ImageCount - 1 do
  begin
    Data := FImage.Images[I];
    Lab := Format('Img%.2d %dx%d %s', [I, Data.Width, Data.Height, GetFormatName(Data.Format)]);
    Node := TreeImage.Items.AddChild(Root, Lab);
    Node.Data := Pointer(I);
  end;
end;

procedure TMainForm.SaveFile(const FileName: string);
var
  T: Int64;
begin
  try
    T := GetTimeMicroseconds;
    FImage.SaveMultiToFile(FileName);
    MeasureTime(Format('File %s saved in:', [ExtractFileName(FileName)]), T);
  except
    MessageDlg(GetExceptObject.Message, mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.SelectSubImage(Index: LongInt);
begin
  FImage.ActiveImage := Index;
  MenuItemActSubImage.Caption := Format('Active Subimage: %d/%d', [FImage.ActiveImage + 1, FImage.ImageCount]);
  UpdateView(False);
end;

procedure TMainForm.UpdateView(RebuildTree: Boolean);
begin
  Image.Picture.Graphic.Assign(FImage);
  if RebuildTree then
    BuildImageTree;
end;

procedure TMainForm.MeasureTime(const Msg: string; const OldTime: Int64);
begin
  StatusBar.SimpleText := Format('  %s %.0n ms', [Msg, (GetTimeMicroseconds - OldTime) / 1000.0]);
end;

{
  File Notes:

  -- 0.80 Changes/Bug Fixes -----------------------------------
    - Added "Add images from file" menu item

  -- 0.77.1 Changes/Bug Fixes ---------------------------------
    - Writing metadata from loaded file when resaving.
    - Added Otsu Thresholding and Deskwing, reorganized some menus.
    - Added Lanczos filtering option to resize image functions.
    - Added option to convert data format of all subimages by default.
    - UI enhancements: added TreeView with image/subimage list,
      added StatusBar instead of simple Panel.

  -- 0.26.5 Changes/Bug Fixes ---------------------------------
    - You can drop file on the form to open it.
    - Added "Show Metadata" item to View menu + related functionality.

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Added Free Resize and Free Rotate functions to Manipulate menu.
    - Added premult/unpremult alpha point transforms.

  -- 0.26.1 Changes/Bug Fixes ---------------------------------
    - Added "show histogram" menu item and functionality.
    - Added new Colors submenu with "set channel set value" commands.
    - Added Canvas.AdjustColorLevels example.

  -- 0.25.0 Changes/Bug Fixes ---------------------------------
    - Added binary morphology operations.
    - Added point transforms and non-linear filters.

  -- 0.24.1 Changes/Bug Fixes ---------------------------------
    - Added status bar which shows times taken by some oprations.
    - Reworked manipulation commands to get rid of UpdateView calls
      everywhere.
    - With Lazarus 0.9.24 images are now displayed with
      proper transparency (those with alpha). Also it doesn't
      screw up some images with 'Fit to window' so that is now
      default.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Catches exceptions during file load/save.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Save As... now saves all images levels instead of just current one.
    - Added XP controls manifest to resource file.
    - Added new filters to Effects menu.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - you can now open image in Imager from shell by passing
      path to image as parameter: 'LCLImager /home/myimage.jpg'
    - added Reload from File menu to reload image from disk
      (poor man's Undo)
    - added Effects menu with some convolution filters
    - added support for displaying of multi images

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - added Nearest, Bilinear, and Bicubic filter options to
      Resize To 50/200% menu items
    - better handling of file exts when using save dialog
    - added rotations to Manipulate menu
    - now works well in Linux too

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - created
}

end.


