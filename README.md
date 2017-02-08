# Checks

* .itch.toml
* Running on 32/64 bit versions of Mint, Debian, Ubuntu, openSUSE, Manjaro
* OS tags on itch, and sanity check: exe must be on Windows, ELF on Linux, machO on macOS, html on web
* Linux: .desktop file exists
* macOS: app is present, it's signed and correct
* macOS: executable is fat file
* Antivirus check with VirusTotal public API
* Signature check on Windows
* Warn against Unity-plugin games
* Executable permissions on Linux and macOS
