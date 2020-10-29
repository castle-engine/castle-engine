{ Use MyBookmarks unit to load/save bookmarks. }
uses SysUtils, MyBookmarks;
var
  Bookmarks: TBookmarkList;
  NewBookmark: TBookmark;
begin
  Bookmarks := TBookmarkList.Create(true);
  try
    Bookmarks.Load;

    NewBookmark := TBookmark.Create;
    NewBookmark.Url := 'https://castle-engine.io/';
    NewBookmark.Caption := 'Castle Game Engine';
    Bookmarks.Add(NewBookmark);
    Bookmarks.Save;
  finally
    FreeAndNil(Bookmarks);
  end;
end.
