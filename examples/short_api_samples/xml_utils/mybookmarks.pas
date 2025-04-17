{ Manage list of bookmarks, in particular load and save them to a simple XML file. }
unit MyBookmarks;

interface

uses Classes, SysUtils, Generics.Collections;

type
  { Bookmark definition. Contains URL and additional data. }
  TBookmark = class
    Caption, Url: String;
    function CaptionFinal: String;
  end;

  { List of TBookmark. }
  TBookmarkList = class({$ifdef FPC}specialize{$endif} TObjectList<TBookmark>)
    procedure Load;
    procedure Save;
  end;

const
  BookmarksDatabase = 'my-bookmarks.xml';

implementation

uses DOM,
  CastleXmlUtils, CastleUriUtils;

{ TBookmark ------------------------------------------------------------------ }

function TBookmark.CaptionFinal: String;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := Url;
end;

{ TBookmarkList -------------------------------------------------------------- }

procedure TBookmarkList.Load;
var
  B: TBookmark;
  Document: TXMLDocument;
  I: TXMLElementIterator;
begin
  Clear;

  if URIFileExists(BookmarksDatabase) then
  begin
    Document := URLReadXML(BookmarksDatabase);
    try
      if Document.DocumentElement.TagName8 <> 'bookmark-list' then
        raise Exception.CreateFmt('Bookmarks XML file has invalid top-level element: %s', [
          Document.DocumentElement.TagName8
        ]);
      I := Document.DocumentElement.ChildrenIterator('bookmark');
      try
        while I.GetNext do
        begin
          B := TBookmark.Create;
          B.Url := I.Current.AttributeString('url');
          B.Caption := I.Current.AttributeString('caption');
          Add(B);
        end;
      finally
        FreeAndNil(I);
      end;
    finally
      FreeAndNil(Document);
    end;
  end;
end;

procedure TBookmarkList.Save;
var
  Document: TXMLDocument;
  RootElement, Child: TDOMElement;
  B: TBookmark;
begin
  Document := TXMLDocument.Create;
  try
    RootElement := Document.CreateElement('bookmark-list');
    Document.AppendChild(RootElement);

    for B in Self do
    begin
      Child := RootElement.CreateChild('bookmark');
      Child.AttributeSet('caption', B.Caption);
      Child.AttributeSet('url', B.Url);
    end;

    URLWriteXML(Document, BookmarksDatabase);
  finally
    FreeAndNil(Document);
  end;
end;

end.
