{
  Copyright 2006-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Abstract precalculated animation (TCastlePrecalculatedAnimationCore). }
unit CastlePrecalculatedAnimationCore;

{$I castleconf.inc}

interface

uses CastleUtils, DOM, Castle3D, Classes;

type
  { An abstract (cannot be rendered) precalculated animation.
    You usually want to use a descendant of this class that can be rendered
    in OpenGL, see TCastlePrecalculatedAnimation. }
  TCastlePrecalculatedAnimationCore = class(T3D)
  public
    { Load animation data from a given URL to a set of variables.

      See [http://castle-engine.sourceforge.net/kanim_format.php]
      for specification of the file format.

      This is a @italic(class procedure) --- it doesn't load the animation
      data to the given TCastlePrecalculatedAnimationCore instance. Instead it loads
      the data to your variables (passed as "out" params). In case
      of RootNodes and Times, you should pass here references to
      @italic(already created and currently empty) lists.

      ModelURLs returned will always contain only absolute paths.
      We will expand every path (like URL parameter) if necessary for this.

      If you seek for most comfortable way to load TCastlePrecalculatedAnimation from a file,
      you probably want to use TCastlePrecalculatedAnimation.LoadFromFile.
      This procedure is more flexible --- it allows
      you to e.g. modify parameters before creating TCastlePrecalculatedAnimation
      instance, and it's usefull to implement a class like
      TCastlePrecalculatedAnimationInfo that also wants to read animation data,
      but doesn't have an TCastlePrecalculatedAnimation instance available. }
    class procedure LoadFromFileToVars(const URL: string;
      ModelURLs: TStringList;
      Times: TSingleList;
      out ScenesPerTime: Cardinal;
      out EqualityEpsilon: Single;
      out ATimeLoop, ATimeBackwards: boolean);

    { Load animation data from a given XML element to a set of variables.

      This is just like LoadFromFileToVars, but it works using
      an Element. This way you can use it to load <animation> element
      that is a part of some larger XML file.

      @param(BaseUrl The URL from which relative
        URLs inside Element will be resolved. It doesn't
        have to be absolute, we will expand it to make it absolute
        if necessary.) }
    class procedure LoadFromDOMElementToVars(Element: TDOMElement;
      const BaseUrl: string;
      ModelURLs: TStringList;
      Times: TSingleList;
      out ScenesPerTime: Cardinal;
      out EqualityEpsilon: Single;
      out ATimeLoop, ATimeBackwards: boolean);
  end;

implementation

uses SysUtils, XMLRead, CastleXMLUtils, CastleFilesUtils, CastleDownload,
  CastleURIUtils;

class procedure TCastlePrecalculatedAnimationCore.LoadFromFileToVars(const URL: string;
  ModelURLs: TStringList;
  Times: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out ATimeLoop, ATimeBackwards: boolean);
var
  Document: TXMLDocument;
  Stream: TStream;
begin
  Stream := Download(URL);
  try
    ReadXMLFile(Document, Stream);
  finally FreeAndNil(Stream) end;

  try
    LoadFromDOMElementToVars(Document.DocumentElement, URL,
      ModelURLs, Times, ScenesPerTime,
      EqualityEpsilon, ATimeLoop, ATimeBackwards);
  finally FreeAndNil(Document); end;
end;

const
  DefaultKAnimScenesPerTime = 30;
  DefaultKAnimEqualityEpsilon = 0.001;
  DefaultKAnimLoop = false;
  DefaultKAnimBackwards = false;

class procedure TCastlePrecalculatedAnimationCore.LoadFromDOMElementToVars(
  Element: TDOMElement;
  const BaseUrl: string;
  ModelURLs: TStringList;
  Times: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out ATimeLoop, ATimeBackwards: boolean);
var
  AbsoluteBaseUrl: string;
  FrameElement: TDOMElement;
  Children: TDOMNodeList;
  I: Integer;
  FrameTime: Single;
  FrameURL: string;
  Attr: TDOMAttr;
begin
  Assert(Times.Count = 0);
  Assert(ModelURLs.Count = 0);

  AbsoluteBaseUrl := AbsoluteURI(BaseUrl);

  Check(Element.TagName = 'animation',
    'Root node of an animation XML file must be <animation>');

  { Assign default values for optional attributes }
  ScenesPerTime := DefaultKAnimScenesPerTime;
  EqualityEpsilon := DefaultKAnimEqualityEpsilon;
  ATimeLoop := DefaultKAnimLoop;
  ATimeBackwards := DefaultKAnimBackwards;

  for I := 0 to Integer(Element.Attributes.Length) - 1 do
  begin
    Attr := Element.Attributes.Item[I] as TDOMAttr;
    if Attr.Name = 'scenes_per_time' then
      ScenesPerTime := StrToInt(Attr.Value) else
    if Attr.Name = 'optimization' then
      { ignore } else
    if Attr.Name = 'equality_epsilon' then
      EqualityEpsilon := StrToFloat(Attr.Value) else
    if Attr.Name = 'loop' then
      ATimeLoop := StrToBool(Attr.Value) else
    if Attr.Name = 'backwards' then
      ATimeBackwards := StrToBool(Attr.Value) else
      raise Exception.CreateFmt('Unknown attribute of <animation> element: "%s"',
        [Attr.Name]);
  end;

  Children := Element.ChildNodes;
  try
    for I := 0 to Integer(Children.Count) - 1 do
      if Children.Item[I].NodeType = ELEMENT_NODE then
      begin
        FrameElement := Children.Item[I] as TDOMElement;
        Check(FrameElement.TagName = 'frame',
          'Each child of <animation> element must be a <frame> element');

        if not DOMGetSingleAttribute(FrameElement, 'time', FrameTime) then
          raise Exception.Create('<frame> element must have a "time" attribute');

        if not DOMGetAttribute(FrameElement, 'file_name', FrameURL) then
          raise Exception.Create('<frame> element must have a "file_name" attribute');

        { Make FrameURL absolute, treating it as relative vs
          AbsoluteBaseUrl }
        FrameURL := CombineURI(AbsoluteBaseUrl, FrameURL);

        if (Times.Count > 0) and (FrameTime <= Times.Last) then
          raise Exception.Create(
            'Frames within <animation> element must be specified in ' +
            'increasing time order');

        ModelURLs.Add(FrameURL);
        Times.Add(FrameTime);
      end;

    if ModelURLs.Count = 0 then
      raise Exception.Create(
        'At least one <frame> is required within <animation> element');
  finally FreeChildNodes(Children) end;
end;

end.