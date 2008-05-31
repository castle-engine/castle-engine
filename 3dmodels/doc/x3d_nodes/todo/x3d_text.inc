{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}
  TNodeX3DFontStyleNode = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeFontStyle = class(TNodeX3DFontStyleNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdfamily: TMFString;
    public property Fdfamily: TMFString read FFdfamily;

    private FFdhorizontal: TSFBool;
    public property Fdhorizontal: TSFBool read FFdhorizontal;

    private FFdjustify: TMFString;
    public property Fdjustify: TMFString read FFdjustify;

    private FFdlanguage: TSFString;
    public property Fdlanguage: TSFString read FFdlanguage;

    private FFdleftToRight: TSFBool;
    public property FdleftToRight: TSFBool read FFdleftToRight;

    private FFdsize: TSFFloat;
    public property Fdsize: TSFFloat read FFdsize;

    private FFdspacing: TSFFloat;
    public property Fdspacing: TSFFloat read FFdspacing;

    private FFdstyle: TSFString;
    public property Fdstyle: TSFString read FFdstyle;

    private FFdtopToBottom: TSFBool;
    public property FdtopToBottom: TSFBool read FFdtopToBottom;
  end;

  TNodeText = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdfontStyle: TSFNode;
    public property FdfontStyle: TSFNode read FFdfontStyle;

    private FFdlength: TMFFloat;
    public property Fdlength: TMFFloat read FFdlength;

    private FFdmaxExtent: TSFFloat;
    public property FdmaxExtent: TSFFloat read FFdmaxExtent;

    private FFdstring: TMFString;
    public property Fdstring: TMFString read FFdstring;

    { Event: MFVec2f, out } { }
    private FEventlineBounds: TVRMLEvent;
    public property EventlineBounds: TVRMLEvent read FEventlineBounds;

    { Event: SFVec3f, out } { }
    private FEventorigin: TVRMLEvent;
    public property Eventorigin: TVRMLEvent read FEventorigin;

    { Event: SFVec2f, out } { }
    private FEventtextBounds: TVRMLEvent;
    public property EventtextBounds: TVRMLEvent read FEventtextBounds;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DFontStyleNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeFontStyle.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfamily := TMFString.Create('family', 'SERIF');
  FFdfamily.Exposed := false;
  Fields.Add(FFdfamily);

  FFdhorizontal := TSFBool.Create('horizontal', true);
  FFdhorizontal.Exposed := false;
  Fields.Add(FFdhorizontal);

  FFdjustify := TMFString.Create('justify', 'BEGIN');
  FFdjustify.Exposed := false;
  Fields.Add(FFdjustify);
  { X3D specification comment: ["BEGIN","END","FIRST","MIDDLE",""] }

  FFdlanguage := TSFString.Create('language', '');
  FFdlanguage.Exposed := false;
  Fields.Add(FFdlanguage);

  FFdleftToRight := TSFBool.Create('leftToRight', true);
  FFdleftToRight.Exposed := false;
  Fields.Add(FFdleftToRight);

  FFdsize := TSFFloat.Create('size', 1.0);
  FFdsize.Exposed := false;
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdspacing := TSFFloat.Create('spacing', 1.0);
  FFdspacing.Exposed := false;
  Fields.Add(FFdspacing);
  { X3D specification comment: [0,Inf) }

  FFdstyle := TSFString.Create('style', 'PLAIN');
  FFdstyle.Exposed := false;
  Fields.Add(FFdstyle);
  { X3D specification comment: ["PLAIN"|"BOLD"|"ITALIC"|"BOLDITALIC"|""] }

  FFdtopToBottom := TSFBool.Create('topToBottom', true);
  FFdtopToBottom.Exposed := false;
  Fields.Add(FFdtopToBottom);

  DefaultContainerField := 'children';
end;

class function TNodeFontStyle.ClassNodeTypeName: string;
begin
  Result := 'FontStyle';
end;

function TNodeFontStyle.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeFontStyle.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeText.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfontStyle := TSFNode.Create(Self, 'fontStyle', [X3FontStyleNode]);
  Fields.Add(FFdfontStyle);

  FFdlength := TMFFloat.Create('length', []);
  Fields.Add(FFdlength);
  { X3D specification comment: [0,Inf) }

  FFdmaxExtent := TSFFloat.Create('maxExtent', 0.0);
  Fields.Add(FFdmaxExtent);
  { X3D specification comment: [0,Inf) }

  FFdstring := TMFString.Create('string', []);
  Fields.Add(FFdstring);

  FEventlineBounds := TVRMLEvent.Create('lineBounds', TMFVec2f, false);
  Events.Add(FEventlineBounds);

  FEventorigin := TVRMLEvent.Create('origin', TSFVec3f, false);
  Events.Add(FEventorigin);

  FEventtextBounds := TVRMLEvent.Create('textBounds', TSFVec2f, false);
  Events.Add(FEventtextBounds);

  FFdsolid := TSFBool.Create('solid', false);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  DefaultContainerField := 'children';
end;

class function TNodeText.ClassNodeTypeName: string;
begin
  Result := 'Text';
end;

function TNodeText.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeText.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterTextNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeFontStyle,
    TNodeText
  ]);
end;

{$endif read_implementation}
