{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface_nested}

var
  { List of Collada images (TAbstractTextureNode). X3DName of every instance
    comes from Collada "id" of <image> element (these are referred to
    by <init_from> contents from <surface>). }
  Images: TX3DNodeList;

{$endif read_interface_nested}

{$ifdef read_implementation_nested}

{ Read <library_images> (Collada 1.4.x). Fills Images list. }
procedure ReadLibraryImages(LibraryElement: TDOMElement);
var
  I: TXMLElementIterator;
  Image: TImageTextureNode;
  ImageId, ImageUrl: string;
begin
  I := LibraryElement.ChildrenIterator('image');
  try
    while I.GetNext do
      if I.Current.AttributeString('id', ImageId) then
      begin
        Image := TImageTextureNode.Create(ImageId, BaseUrl);
        Images.Add(Image);
        ImageUrl := ReadChildText(I.Current, 'init_from');
        if ImageUrl <> '' then
          Image.FdUrl.Items.Add(ImageUrl);
      end;
  finally FreeAndNil(I) end;
end;

{$endif read_implementation_nested}
