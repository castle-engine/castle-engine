# Not maintained (known to be outdated) Delphinus metadata

In 2023 we tried to integrate _Castle Game Engine_ with Delphinus, an open-source package manager for Delphi. Initial results were promising, we could use Delphinus to "install" CGE into Delphi at one point, at least locally.

However, we hit then limitations of Delphinus -- some of them critical, and it doesn't seem like maintainer has capacity to address them. We had to stop supporting Delphinus, the metadata in this directory is not maintained and doesn't contain latest info (directories, platforms) anymore.

Details of our issues:

- Not possible to have one design-time package depending on another
  https://github.com/Memnarch/Delphinus/issues/93

    This is most critical, it means Delphinus just cannot work with latest CGE.

    We needed a certain package organization, see https://castle-engine.io/delphi_packages and https://github.com/castle-engine/castle-engine/blob/master/packages/delphi/README.md . In particular, we need more than one design-time package -> since the package with Delphi integration code has to be only Win32, while base package has to declare all platforms (Win32, Win64, Linux now) otherwise the TCastleControl is not visible as available in Delphi component palette for non-Win32 systems.

    But installing 2 design-time packages always fails with Delphinus.

- Delphinus fails to download CGE
  https://github.com/castle-engine/castle-engine/issues/84

    Downloading CGE from Delphinus was never possible, Delphinus hangs at downloading. Our engine is quite large, still it is 2 minutes to download with a reasonable connection speed (see https://github.com/castle-engine/castle-engine/issues/84#issuecomment-1535336574 ). However, with the same Internet connection, Delphinus takes 20 minutes and still cannot finish -- it eaither hangs at some point, or uses network connection in some very slow way.

    (Maybe related to `TNetHTTPClient` slowness on Delphi+Windows? We also fought with this in CGE, and eventually switched to use `TIdHttp` on Delphi+Windows.)

- After installing a package it won't uninstall again
  https://github.com/Memnarch/Delphinus/issues/90

- Delphinus Websetup cannot install to default system-wide location (due to not having admin rights)
  https://github.com/Memnarch/Delphinus/issues/91

- Show clearly on the Install / Uninstall dialogs that installation / uninstallation failed
  https://github.com/Memnarch/Delphinus/issues/92