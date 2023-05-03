# copy-paste-sync

An easy way to share clipboard (not encrypted) on a trusted LAN.

For two machines, 192.168.8.1 (macOS) and 192.168.8.2 (Linux), run the
following:

Run on macOS:

```
$ stack run -- macos 1234 http://192.168.8.1:1234/
```

Run on Linux:

```
$ stack run -- linux 1234 http://192.168.8.2:1234/
```

You can make it secure via an SSH tunnel if you're not on a trusted
LAN.
