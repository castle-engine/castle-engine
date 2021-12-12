{

  Clipping Demo
  Vampyre Imaging Library
  http://imaginglib.sourceforge.net

  I used this demo during fixing of clipping for CopyRect/StretchRect functions.
  You have a source and destination images on the form and few movable and
  resizable bevels that represent source, destnation, and clipping rectangle.
  Fiddle with them as you want and then click CopyRect Test or StretchRect Test
  button. New form will be shown with results. One image created by
  Imaging's Copy/Stretch rect functions (wrapped in TBaseImage here)
  and the second created by WinAPI's BitBlt and StretchBlt functions.
  Copied images should look exactly the same and stretched ones should
  have the same clipping and very similar looks (Imaging's stretch is filtered,
  WinAPI's not).

  Demo shows usage of high level Imaging classes (TBaseImage->TSingleImage)
  and VCL component support (TImagingBitmap). Needs JVCL library to compile.

}
unit ClipForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExExtCtrls, JvMovableBevel, StdCtrls, Buttons,
  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingComponents,
  ImagingCanvases,
  ImagingFormats,
  ImagingUtility;

type
  TMainForm = class(TForm)
    PanelConf: TPanel;
    ImageSrc: TImage;
    ImageDst: TImage;
    SelDst: TJvMovableBevel;
    SelSrc: TJvMovableBevel;
    PanelCmd: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ClipDst: TJvMovableBevel;
    Button5: TButton;
    Button6: TButton;
    CheckGenCanvas: TCheckBox;
    procedure BtnLoadImagesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  public
    SrcImage, DstImage: TSingleImage;
    SrcBitmap, DstBitmap: TImagingBitmap;
    procedure DoTest(Stretch, CanvasDraw: Boolean);
  end;

const
  DefaultSrc = 'Vezyr.png';
  DefaultDst = 'Tigers.jpg';
  ForceFormat = ifA8R8G8B8;

var
  MainForm: TMainForm;

implementation

uses
  ResultsForm;

{$R *.dfm}

function GetTestImage(const FileName: string): string;
begin
  Result := ExtractFileDir(ExtractFileDir(ExtractFileDir(GetAppDir))) +
    PathDelim + 'Demos' + PathDelim + 'Data' + PathDelim + FileName;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create working images
  SrcImage := TSingleImage.Create;
  DstImage := TSingleImage.Create;
  // Create our bitmaps which will be assigned to TImage components.
  // Standard TBitmap could be used but our bitmaps can be assigned directly
  // from TSingleImage.
  SrcBitmap := TImagingBitmap.Create;
  DstBitmap := TImagingBitmap.Create;

  ImageSrc.Picture.Graphic := SrcBitmap;
  ImageDst.Picture.Graphic := DstBitmap;

  BtnLoadImagesClick(Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Free used images
  SrcImage.Free;
  DstImage.Free;
  // Free bitmaps asigned to TImage too - it wont free them automatically
  SrcBitmap.Free;
  DstBitmap.Free;
end;

procedure TMainForm.BtnLoadImagesClick(Sender: TObject);
begin
  // Load test images
  SrcImage.LoadFromFile(GetTestImage(DefaultSrc));
  DstImage.LoadFromFile(GetTestImage(DefaultDst));
  // Change their format to A8R8G8B8 (for faster drawing later)
  SrcImage.Format := ForceFormat;
  DstImage.Format := ForceFormat;
  // Resize them to fit in TImages on form
  SrcImage.Resize(ImageSrc.Width, ImageSrc.Height, rfNearest);
  DstImage.Resize(ImageDst.Width, ImageDst.Height, rfNearest);
  // Finally assign them to those TImages
  ImageSrc.Picture.Graphic.Assign(SrcImage);
  ImageDst.Picture.Graphic.Assign(DstImage);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  SelSrc.SetBounds(ImageSrc.Left, ImageSrc.Top, ImageSrc.Width, ImageSrc.Height);
  SelDst.SetBounds(ImageDst.Left, ImageDst.Top, ImageSrc.Width, ImageSrc.Height);
  ClipDst.SetBounds(ImageDst.Left - 20, ImageDst.Top - 20, ImageDst.Width + 40, ImageDst.Height + 40);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  SelSrc.SetBounds(ImageSrc.Left, ImageSrc.Top, ImageSrc.Width, ImageSrc.Height);
  SelDst.SetBounds(ImageDst.Left, ImageDst.Top, ImageDst.Width, ImageDst.Height);
  ClipDst.SetBounds(ImageDst.Left - 20, ImageDst.Top - 20, ImageDst.Width + 40, ImageDst.Height + 40);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  DoTest(False, False);
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  DoTest(True, False);
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  DoTest(False, True);
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  DoTest(True, True);
end;

procedure TMainForm.DoTest(Stretch, CanvasDraw: Boolean);
var
  Result: TSingleImage;
  SrcBounds, DstBounds, DstClip: TRect;
  SrcBmp, DstBmp: TImagingBitmap;
  Rgn: HRGN;
  SrcCanvas, DestCanvas: TImagingCanvas;
begin
  // First use Imaging to copy/stretch images ----------------

  // Create result image and get rects from movable bevels on the form
  Result := TSingleImage.CreateFromImage(DstImage);
  SrcBounds := Rect(SelSrc.Left - ImageSrc.Left, SelSrc.Top - ImageSrc.Top,
    SelSrc.Width, SelSrc.Height);
  DstBounds := Rect(SelDst.Left - ImageDst.Left, SelDst.Top - ImageDst.Top,
    SelDst.Width, SelDst.Height);
  DstClip := Rect(ClipDst.Left - ImageDst.Left, ClipDst.Top - ImageDst.Top,
    ClipDst.Left - ImageDst.Left + ClipDst.Width, ClipDst.Top - ImageDst.Top + ClipDst.Height);

  if not CanvasDraw then
  begin
    if Stretch then
    begin
      // Clips rects for stretching
      ImagingUtility.ClipStretchBounds(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
        DstBounds.Left, DstBounds.Top, DstBounds.Right, DstBounds.Bottom, SrcImage.Width, SrcImage.Height, DstClip);
      // Call image's stretch method
      SrcImage.StretchTo(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
        Result, DstBounds.Left, DstBounds.Top, DstBounds.Right, DstBounds.Bottom, rfBilinear);
    end
    else
    begin
      // Clips rects for copying
      ImagingUtility.ClipCopyBounds(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
        DstBounds.Left, DstBounds.Top, SrcImage.Width, SrcImage.Height, DstClip);
      // Call image's copy method
      SrcImage.CopyTo(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
        Result, DstBounds.Left, DstBounds.Top);
    end;
  end
  else
  begin
    if CheckGenCanvas.Checked then
    begin
      SrcCanvas := TImagingCanvas.CreateForImage(SrcImage);
      DestCanvas := TImagingCanvas.CreateForImage(Result);
    end
    else
    begin
      SrcCanvas := FindBestCanvasForImage(SrcImage).CreateForImage(SrcImage);
      DestCanvas := FindBestCanvasForImage(Result).CreateForImage(Result);
    end;

    if Stretch then
    begin
      // Clips rects for stretching
      ImagingUtility.ClipStretchBounds(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
        DstBounds.Left, DstBounds.Top, DstBounds.Right, DstBounds.Bottom, SrcImage.Width, SrcImage.Height, DstClip);
      // Call stretch method
      SrcCanvas.StretchDrawAlpha(Rect(SrcBounds.Left, SrcBounds.Top, SrcBounds.Left + SrcBounds.Right, SrcBounds.Top + SrcBounds.Bottom),
        DestCanvas, Rect(DstBounds.Left, DstBounds.Top, DstBounds.Left + DstBounds.Right, DstBounds.Top + DstBounds.Bottom),
        rfBilinear);
    end
    else
    begin
      // Clips rects for copying
      ImagingUtility.ClipCopyBounds(SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
        DstBounds.Left, DstBounds.Top, SrcImage.Width, SrcImage.Height, DstClip);
      // Call draw method
      SrcCanvas.DrawAlpha(Rect(SrcBounds.Left, SrcBounds.Top, SrcBounds.Left + SrcBounds.Right, SrcBounds.Top + SrcBounds.Bottom),
        DestCanvas, DstBounds.Left, DstBounds.Top);
    end;

    SrcCanvas.Free;
    DestCanvas.Free;
  end;

  // Assign Imaging result to TImage on Result form
  ResultForm.ImageMy.Picture.Graphic.Assign(Result);

  // Now use WinAPI to copy/stretch images ----------------------

  // Create bitmaps and assign source and dest images to them
  SrcBmp := TImagingBitmap.Create;
  SrcBmp.Assign(SrcImage);
  DstBmp := TImagingBitmap.Create;
  DstBmp.Assign(DstImage);

  // Get fresh bounds
  SrcBounds := Rect(SelSrc.Left - ImageSrc.Left, SelSrc.Top - ImageSrc.Top,
    SelSrc.Width, SelSrc.Height);
  DstBounds := Rect(SelDst.Left - ImageDst.Left, SelDst.Top - ImageDst.Top,
    SelDst.Width, SelDst.Height);

  // Now create and set clipping region
  Rgn := CreateRectRgn(DstClip.Left, DstClip.Top, DstClip.Right, DstClip.Bottom);
  SelectClipRgn(DstBmp.Canvas.Handle, Rgn);

  // Now stretch or copy
  if Stretch then
  begin
    StretchBlt(DstBmp.Canvas.Handle, DstBounds.Left, DstBounds.Top, DstBounds.Right, DstBounds.Bottom,
      SrcBmp.Canvas.Handle, SrcBounds.Left, SrcBounds.Top, SrcBounds.Right, SrcBounds.Bottom, SRCCOPY);
  end
  else
  begin
    BitBlt(DstBmp.Canvas.Handle, DstBounds.Left, DstBounds.Top, SrcBounds.Right, SrcBounds.Bottom,
      SrcBmp.Canvas.Handle, SrcBounds.Left, SrcBounds.Top, SRCCOPY);
  end;

  // Assign Imaging result to TImage on Result form
  ResultForm.ImageWin.Picture.Graphic.Assign(DstBmp);

  Result.Free;
  SrcBmp.Free;
  DstBmp.Free;
  //SelectClipRgn(DstBmp.Canvas.Handle, 0);
  //DeleteObject(Rgn);

  // Show results
  ResultForm.ShowModal;
end;

end.
