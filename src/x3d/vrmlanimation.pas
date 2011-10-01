{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Abstract precalculated animation (T3DPrecalculatedAnimationCore). }
unit VRMLAnimation;

{$I castleconf.inc}

interface

uses CastleUtils, DOM, Base3D, Classes;

type
  { An abstract (cannot be rendered) precalculated animation.
    You usually want to use a descendant of this class that can be rendered
    in OpenGL, see T3DPrecalculatedAnimation. }
  T3DPrecalculatedAnimationCore = class(T3D)
  public
    { Load animation data from a given FileName to a set of variables.

      See [http://castle-engine.sourceforge.net/kanim_format.php]
      for specification of the file format.

      This is a @italic(class procedure) --- it doesn't load the animation
      data to the given T3DPrecalculatedAnimationCore instance. Instead it loads
      the data to your variables (passed as "out" params). In case
      of RootNodes and Times, you should pass here references to
      @italic(already created and currently empty) lists.

      ModelFileNames returned will always contain only absolute paths.
      We will expand every path (like FileName parameter) if necessary for this.

      If you seek for most comfortable way to load T3DPrecalculatedAnimation from a file,
      you probably want to use T3DPrecalculatedAnimation.LoadFromFile.
      This procedure is more flexible --- it allows
      you to e.g. modify parameters before creating T3DPrecalculatedAnimation
      instance, and it's usefull to implement a class like
      T3DPrecalculatedAnimationInfo that also wants to read animation data,
      but doesn't have an T3DPrecalculatedAnimation instance available. }
    class procedure LoadFromFileToVars(const FileName: string;
      ModelFileNames: TStringList;
      Times: TSingleList;
      out ScenesPerTime: Cardinal;
      out EqualityEpsilon: Single;
      out ATimeLoop, ATimeBackwards: boolean);

    { Load animation data from a given XML element to a set of variables.

      This is just like LoadFromFileToVars, but it works using
      an Element. This way you can use it to load <animation> element
      that is a part of some larger XML file.

      @param(BasePath The path from which relative
        filenames inside Element will be resolved. It doesn't
        have to be an absolute path, we will expand it to make it absolute
        if necessary.) }
    class procedure LoadFromDOMElementToVars(Element: TDOMElement;
      const BasePath: string;
      ModelFileNames: TStringList;
      Times: TSingleList;
      out ScenesPerTime: Cardinal;
      out EqualityEpsilon: Single;
      out ATimeLoop, ATimeBackwards: boolean);
  end;

implementation

uses SysUtils, XMLRead, CastleXMLUtils, CastleFilesUtils;

class procedure T3DPrecalculatedAnimationCore.LoadFromFileToVars(const FileName: string;
  ModelFileNames: TStringList;
  Times: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out ATimeLoop, ATimeBackwards: boolean);
var
  Document: TXMLDocument;
begin
  try
    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }
    ReadXMLFile(Document, FileName);

    LoadFromDOMElementToVars(Document.DocumentElement,
      ExtractFilePath(FileName),
      ModelFileNames, Times, ScenesPerTime,
      EqualityEpsilon, ATimeLoop, ATimeBackwards);
  finally FreeAndNil(Document); end;
end;

const
  DefaultKAnimScenesPerTime = 30;
  DefaultKAnimEqualityEpsilon = 0.001;
  DefaultKAnimLoop = false;
  DefaultKAnimBackwards = false;

class procedure T3DPrecalculatedAnimationCore.LoadFromDOMElementToVars(
  Element: TDOMElement;
  const BasePath: string;
  ModelFileNames: TStringList;
  Times: TSingleList;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out ATimeLoop, ATimeBackwards: boolean);
var
  AbsoluteBasePath: string;
  FrameElement: TDOMElement;
  Children: TDOMNodeList;
  I: Integer;
  FrameTime: Single;
  FrameFileName: string;
  Attr: TDOMAttr;
begin
  Assert(Times.Count = 0);
  Assert(ModelFileNames.Count = 0);

  AbsoluteBasePath := ExpandFileName(BasePath);

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

        if not DOMGetAttribute(FrameElement, 'file_name', FrameFileName) then
          raise Exception.Create('<frame> element must have a "file_name" attribute');

        { Make FrameFileName absolute, treating it as relative vs
          AbsoluteBasePath }
        FrameFileName := CombinePaths(AbsoluteBasePath, FrameFileName);

        if (Times.Count > 0) and (FrameTime <= Times.Last) then
          raise Exception.Create(
            'Frames within <animation> element must be specified in ' +
            'increasing time order');

        ModelFileNames.Add(FrameFileName);
        Times.Add(FrameTime);
      end;

    if ModelFileNames.Count = 0 then
      raise Exception.Create(
        'At least one <frame> is required within <animation> element');
  finally FreeChildNodes(Children) end;
end;

end.