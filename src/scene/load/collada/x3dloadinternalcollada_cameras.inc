{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_implementation_nested}

var
  Cameras: TX3DChildrenList;

{ Read <library_cameras> (Collada 1.4.x). Fills Cameras list. }
procedure ReadLibraryCameras(LibraryE: TDOMElement);
var
  Id: string;
  CameraGroup: TGroupNode;

  procedure InitializeNavigationInfo(E: TDOMElement);
  var
    Navigation: TNavigationInfoNode;
    ZNear, ZFar: Float;
  begin
    Navigation := TNavigationInfoNode.Create(Id + '_navigation_info', BaseUrl);
    CameraGroup.AddChildren(Navigation);

    if ReadChildFloat(E, 'znear', ZNear) then
      Navigation.FdAvatarSize.Items[0] := ZNear * 2;

    if ReadChildFloat(E, 'zfar', ZFar) then
      Navigation.FdVisibilityLimit.Value := ZFar;
  end;

var
  I: TXMLElementIterator;
  Viewpoint: TViewpointNode;
  OrthoViewpoint: TOrthoViewpointNode;
  OpticsE, TechniqueE, PerspectiveE, OrthographicE: TDOMElement;
  XFov, YFov, XMag, YMag, AspectRatio: Float;
begin
  I := LibraryE.ChildrenIterator('camera');
  try
    while I.GetNext do
      if I.Current.AttributeString('id', Id) then
      begin
        CameraGroup := TGroupNode.Create(Id, BaseUrl);
        Cameras.Add(CameraGroup);

        OpticsE := I.Current.ChildElement('optics', false);
        if OpticsE <> nil then
        begin
          TechniqueE := OpticsE.ChildElement('technique_common', false);
          if TechniqueE <> nil then
          begin
            PerspectiveE := TechniqueE.ChildElement('perspective', false);
            if PerspectiveE <> nil then
            begin
              Viewpoint := TViewpointNode.Create(Id + '_viewpoint', BaseUrl);
              if CastleX3dExtensions then
                Viewpoint.AutoCenterOfRotation := true;
              Viewpoint.FdPosition.Value := TVector3.Zero;
              CameraGroup.AddChildren(Viewpoint);

              { Try to get YFov, and use it as X3D fieldOfView.
                It's not a perfect translation, the idea of fieldOfView
                is just different (X3D doesn't force aspect ratio). }
              if ReadChildFloat(PerspectiveE, 'yfov', YFov) then
                Viewpoint.FdFieldOfView.Value := DegToRad(YFov) else
              if ReadChildFloat(PerspectiveE, 'xfov', XFov) and
                 ReadChildFloat(PerspectiveE, 'aspect_ratio', AspectRatio) and
                 (AspectRatio > SingleEpsilon) then
                { aspect_ratio = xfov / yfov, so we can calculate yfov }
                Viewpoint.FdFieldOfView.Value := DegToRad(XFov) / AspectRatio;

              InitializeNavigationInfo(PerspectiveE);
            end else
            begin
              OrthographicE := TechniqueE.ChildElement('orthographic', false);
              if OrthographicE <> nil then
              begin
                OrthoViewpoint := TOrthoViewpointNode.Create(Id + '_viewpoint', BaseUrl);
                if CastleX3dExtensions then
                  OrthoViewpoint.AutoCenterOfRotation := true;
                OrthoViewpoint.FdPosition.Value := TVector3.Zero;
                CameraGroup.AddChildren(OrthoViewpoint);

                { Translation to X3D cannot be perfect, as fieldOfView
                  just works differently, and X3D automatically preserves
                  aspect ratio. We concentrate on setting vertical angle
                  right (as this one is usually smaller, and so determines
                  horizontal angle). }
                if ReadChildFloat(OrthographicE, 'ymag', YMag) then
                begin
                  OrthoViewpoint.FdFieldOfView.Items[1] := -YMag;
                  OrthoViewpoint.FdFieldOfView.Items[3] :=  YMag;
                end else
                if ReadChildFloat(OrthographicE, 'xmag', XMag) then
                begin
                  if ReadChildFloat(OrthographicE, 'aspect_ratio', AspectRatio) and
                     (AspectRatio > SingleEpsilon) then
                  begin
                    { aspect_ratio = xmag / ymag, so we can calculate ymag }
                    YMag := XMag / AspectRatio;
                    OrthoViewpoint.FdFieldOfView.Items[1] := -YMag;
                    OrthoViewpoint.FdFieldOfView.Items[3] :=  YMag;
                  end else
                  begin
                    OrthoViewpoint.FdFieldOfView.Items[0] := -XMag;
                    OrthoViewpoint.FdFieldOfView.Items[2] :=  XMag;
                  end;
                end;

                InitializeNavigationInfo(OrthographicE);
              end else
                WritelnWarning('Collada', 'No supported camera inside <technique_common>');
            end;
          end else
            WritelnWarning('Collada', 'No supported camera technique inside <optics>');
        end else
          WritelnWarning('Collada', 'No <optics> inside camera');
      end;
  finally FreeAndNil(I) end;
end;

{$endif read_implementation_nested}
