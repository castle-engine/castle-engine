{
  Copyright 2001-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface_uses}
CastleXF86VMode,
{$endif}

{ This implements TCastleApplication video resizing with XF86VMode }

{$ifdef read_application_interface}
private
  VideoModeChanged: boolean;
  { *ModeInfos are valid only if VideoModeChanged }
  CurrentModeInfo, OriginalModeInfo: TXF86VidModeModeInfo;

  FUseVirtualXScreen: boolean;
public
  { jezeli UseVirtualXScreen to pod X-ami zmiana rozmiarow Video przez
    VideoChange nie spowoduje zmiany ScreenWidth/H i bedziemy miec
    okno np. 800x600 na ekranie 640x480. To jest wlasnie virtual screen
    X-ow. W sumie moze to byc fajna rzecz dla jakichs programow uzytkowych ktore
    pozwalaja userowi na operowanie na ekranie jakimis kontrolkami
    ale nie dla typowych pelnoekranowych gier/dem : dla nas zmiana
    rozmiaru ekranu ma na celu zmniejszenie/zwiekszenie dokladnosci
    renderowanego obrazu. Wiec domyslnie UseVirtualXScreen = false. }
  property UseVirtualXScreen: boolean
    read FUseVirtualXScreen write FUseVirtualXScreen default false;
{$endif read_application_interface}

{$ifdef read_implementation}

function XF86VidModeModeLineToModeInfo(const ModeLine: TXF86VidModeModeLine;
  DotClock: Longint): TXF86VidModeModeInfo;
begin
 result.dotclock := DotClock;
 result.hdisplay := ModeLine.hdisplay;
 result.hsyncstart := ModeLine.hsyncstart;
 result.hsyncend := ModeLine.hsyncend;
 result.htotal := ModeLine.htotal;
 result.hskew := ModeLine.hskew;
 result.vdisplay := ModeLine.vdisplay;
 result.vsyncstart := ModeLine.vsyncstart;
 result.vsyncend := ModeLine.vsyncend;
 result.vtotal := ModeLine.vtotal;
 result.flags := ModeLine.flags;
 result.privsize := 0;
 { TODO: should I try to copy privsize here somehow ?
   TODO: should we in TryVideoChange and VideoReset and here do something
   with c_private fields ? Manual is unclear to me... }
end;

function TCastleApplication.TryVideoChange: boolean;
var
  {vidModeMajorVersion, vidModeMinorVersion: integer;}
  i, bestmode: integer;

  modesCount: integer;
  modes : PArray_PXF86VidModeModeInfo;

  OriginalMode: TXF86VidModeModeLine;
  OriginalModeDotClock: Longint;

  { I really don't need this, but XF86VidModeQueryExtension on my Debian
    segfaults when passed nil as 2nd or 3rd param. So I pass it some valid
    pointers... }
  DummyEventBase: LongInt;
  DummyErrorBase: LongInt;
begin
 {w tej chwili nie zmieniamy BitsPerPixel ekranu wiec
  robimy faktyczne VideoChange tylko jezeli VideoResize. }
 if not VideoResize then exit(true);

 InitializeXDisplay;

 {TESTOWE:}
{ XF86VidModeQueryVersion(XDisplay, @vidModeMajorVersion,
   @vidModeMinorVersion);
 Writeln(Format('XF86VidModeExtension-Version %d.%d', [vidModeMajorVersion,
   vidModeMinorVersion]));}

 if not XF86VidModeQueryExtension(XDisplay, @DummyEventBase,
   @DummyErrorBase) then
   { X server doesn't implement this extension (at least on this display) }
   Exit(false);

 { if this is first VideoChange then save desktop resolution before
   switching modes }
 if not VideoModeChanged then
 begin
  XF86VidModeGetModeLine(XDisplay, XScreen, @OriginalModeDotClock,
    @OriginalMode);
  OriginalModeInfo := XF86VidModeModeLineToModeInfo(OriginalMode, OriginalModeDotClock);
 end;

 XF86VidModeGetAllModeLines(XDisplay, XScreen, @modesCount, @modes);
 try
  { look for mode with requested resolution }
  bestmode := -1;
  for i := 0 to modesCount-1 do
   if (not VideoResize) or
      ((modes^[i]^.hdisplay = VideoResizeWidth) and
       (modes^[i]^.vdisplay = VideoResizeHeight)) then
       { TODO: jak tu dopasowac BitsPerPixel }
    bestMode := i;

  if bestmode < 0 then exit(false);

  CurrentModeInfo := modes^[bestMode]^;
  XF86VidModeSwitchToMode(XDisplay, XScreen, @CurrentModeInfo);

  {SetViewport jest przydatne gdy zmienimy sie z duzego ekranu na mniejszy i
   nie chcemy uzywac VirtualXScreen. Wtedy czasem nasz maly ekranik jest
   nieco z boku i user musi nakierowac myszka aby byl on w srodku.
   Ponizsze SetViewport zrobi to za niego. }
  XF86VidModeSetViewPort(XDisplay, XScreen, 0, 0);

  VideoModeChanged := true;
  result := true;
 finally XFree(modes) end;
end;

procedure TCastleApplication.VideoReset;
begin
 if VideoModeChanged then
 begin
  { switch back to original desktop resolution }
  XF86VidModeSwitchToMode(XDisplay, XScreen, @OriginalModeInfo);
  VideoModeChanged := false;
 end;
end;

function TCastleApplication.ScreenWidth: integer;
begin
 InitializeXDisplay;

 { jezeli VideoModeChanged to nie chcemy uzywac rozmiarow screenu (bo wtedy
   po utworzeniu okienka fullscreen mielibysmy okno o wymiarach np. 800x600
   na ekranie 640x480. Po tym oknie musielibysmy sie przewijac myszka.
   Zdecydowanie, to nie jest nasz cel : zazwyczaj my zmieniajac rozdzielczosc
   chcemy miec po prostu wieksza/mniejsza dokladnosc obrazu zmieniajac ilosc pixeli
   na ktorych ten obraz ma sie miescic. }
 if VideoModeChanged and (not UseVirtualXScreen) then
  result := CurrentModeInfo.hdisplay else
  result := XDisplayWidth(XDisplay, XScreen);
end;

function TCastleApplication.ScreenHeight: integer;
begin
 InitializeXDisplay;

 if VideoModeChanged and (not UseVirtualXScreen) then
  result := CurrentModeInfo.vdisplay else
  result := XDisplayHeight(XDisplay, XScreen);
end;

{$endif read_implementation}
