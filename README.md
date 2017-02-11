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
* Executable permissions on Linux and macOS
* Filenames with Unicode symbols


## glibc versions by ubuntu version

* Ubuntu 12.04 precise: 2.15
* Ubuntu 14.04 trusty: 2.19
* Ubuntu 16.04 xenial: 2.23
* Ubuntu 16.10 yakkety: 2.24.3
* Ubuntu 17.04 zesty: 2.24.7
