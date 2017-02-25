# Checks

* .itch.toml
* Running on 32/64 bit versions of Mint, Debian, Ubuntu, openSUSE, Manjaro
* OS tags on itch, and sanity check: exe must be on Windows, ELF on Linux, machO on macOS, html on web
* Linux: .desktop file exists
* macOS: app is present, it's signed and correct
* macOS: executable is fat file
* Antivirus check with ClamAV and possibly VirusTotal public API
* Signature check on Windows
* Warn against Unity-plugin games
* Executable permissions on Linux and macOS - for binaries, scripts and .desktop
* Filenames with Unicode symbols
* Suggest setting display name for uploads with missing display name
* Suggest using butler for uploads (i.e. detect wharf-non-enabled uploads)
* Suggest using user-version when using butler

## glibc versions by ubuntu version

* Ubuntu 12.04 precise: 2.15
* Ubuntu 14.04 trusty: 2.19
* Ubuntu 16.04 xenial: 2.23
* Ubuntu 16.10 yakkety: 2.24.3
* Ubuntu 17.04 zesty: 2.24.7

## Per-binary checks

* Compare binary's dependencies with bundled libraries, suggest bundling missing libraries or adding prereqs into manifest
* Linux: compare glibc version with distros' versions

## Per-upload checks

* if there's binaries(s), they must cover all tags' platforms, and cannot be for other platform
* if there's no binaries, it's ok with whatever flags - maybe it's soundtrack or asset pack
* .itch.toml actions must point to existing files

## Per-upload group checks

Upload groups:

* Release
* Pre-order
* Demo

Across upload group:

* If there's binaries for Windows, check that there's 32-bit version.
* If there's binaries for Linux, check that there's both x64 and x86 binaries.
* If there's binaries for macOS, check that there's both x64 and x86 binaries. Suggest making a fat file if it's not.
