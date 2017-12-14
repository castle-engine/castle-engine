Tempates for Windows resources (including version info, icon, manifest):

- Windows version info crafted looking at docs, what others are doing, and what Lazarus produces:
  http://www.osronline.com/article.cfm?article=588
  http://msdn.microsoft.com/en-us/library/windows/desktop/aa381058%28v=vs.85%29.aspx
  http://stackoverflow.com/questions/12821369/vc-2012-how-to-include-version-info-from-version-inc-maintained-separately
  http://forum.lazarus.freepascal.org/index.php?topic=8979.0

- Windows manifest contents also done looking at what Lazarus produces. See also:
  http://stackoverflow.com/questions/1402137/how-to-use-a-manifest-embedded-as-a-resource-windows-xp-vista-style-controls
  http://qt-project.org/forums/viewthread/17440
  http://msdn.microsoft.com/en-us/library/wwtazz9d.aspx

- Note that the main icon has MainIcon identifier, not AppIcon.
  Although [http://freepascal.org/docs-html/prog/progse61.html] suggests using AppIcon,
  some programs use MAINICON, and our CastleWindow in WinAPI backend also loads icon
  from MAINICON name. Ultimately, it doesn't seem to matter to Windows (it grabs
  the first ico available in file?).
