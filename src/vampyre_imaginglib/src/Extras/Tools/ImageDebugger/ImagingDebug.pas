{
  Vampyre Imaging Library
  by Marek Mauder 
  http://imaginglib.sourceforge.net

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

{ This unit is a wrapper to "The Image Debugger" library/utility. Get the
  debugger binaries at http://www.billbaxter.com/projects/imdebug/ .}
unit ImagingDebug;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, ImagingTypes, Imaging, ImagingUtility, PasImDebug;

{ Wrapper to PasImDebug.ImDebug function which automaticaly creates proper
  format string for given image. ImageDebugger's DLL and viewer app must
  be in system's search path.}  
procedure DebugImage(const Img: TImageData);

implementation

procedure DebugImage(const Img: TImageData);
var
  FmtInfo: TImageFormatInfo;
  PF: PPixelFormatInfo;
  FmtString: string;
  DebugImg: TImageData;
begin
  if TestImage(Img) then
  begin
    GetImageFormatInfo(Img.Format, FmtInfo);
    if FmtInfo.IsSpecial or FmtInfo.IsIndexed then
    begin
      CloneImage(Img, DebugImg);
      ConvertImage(DebugImg, ifDefault);
      GetImageFormatInfo(DebugImg.Format, FmtInfo);
    end
    else
      DebugImg := Img;

    // first determine proper channel format
    if FmtInfo.HasGrayChannel then
    begin
      FmtString := 'lum';
      if FmtInfo.HasAlphaChannel then
        FmtString := FmtString + 'a';
    end
    else
    begin
      if FmtInfo.IsRBSwapped then
        FmtString := 'rgb'
      else
        FmtString := 'bgr';

      if FmtInfo.HasAlphaChannel then
        FmtString := FmtString + 'a';
    end;

    FmtString := FmtString + ' b=';

    // Now determine proper channel bit counts
    if FmtInfo.UsePixelFormat then
    begin
      PF := FmtInfo.PixelFormat;
      FmtString := FmtString + Iff(PF.BBitCount > 0, IntToStr(PF.BBitCount), '');
      FmtString := FmtString + Iff(PF.GBitCount > 0, ',' + IntToStr(PF.GBitCount), '');
      FmtString := FmtString + Iff(PF.RBitCount > 0, ',' + IntToStr(PF.RBitCount), '');
      FmtString := FmtString + Iff(PF.ABitCount > 0, ',' + IntToStr(PF.ABitCount), '');
    end
    else
    begin
      if FmtInfo.HasGrayChannel then
      begin
        case FmtInfo.Format of
          ifGray8, ifA8Gray8: FmtString := FmtString + '8';
          ifGray16, ifA16Gray16: FmtString := FmtString + '16';
          ifGray32: FmtString := FmtString + '32';
          ifGray64: FmtString := FmtString + '64';
        end;
      end
      else if FmtInfo.IsFloatingPoint then
      begin
        case FmtInfo.BytesPerPixel of
          4: FmtString := 'r b=32f';
          6, 8: FmtString := FmtString + '16f';
          12, 16: FmtString := FmtString + '32f';
        end;
      end
      else
      begin
        case FmtInfo.BytesPerPixel of
          3..4: FmtString := FmtString + '8';
          6..8: FmtString := FmtString + '16';
          12..16: FmtString := FmtString + '32';
        end;
      end;
    end;
    imdebug(PAnsiChar(AnsiString(FmtString + ' w=%d h=%d %p')), DebugImg.Width, DebugImg.Height, DebugImg.Bits);

    if DebugImg.Bits <> Img.Bits then
      FreeImage(DebugImg);
  end
  else
    DebugMsg('DebugImage: Invalid input image %s', [ImageToStr(Img)]);
end;

end.
