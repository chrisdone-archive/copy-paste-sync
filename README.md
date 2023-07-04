# copy-paste-sync

An easy way to share clipboard (encrypted by Blowfish) on a LAN.

Make a `$HOME/.copy-paste-sync.pass` with a large randomly generated
password in it on both machines.

For two machines, 192.168.8.1 (macOS) and 192.168.8.2 (Linux), run the
following:

Run on macOS:

```
$ stack run -- macos 3232 http://192.168.8.1:3232 $HOME/copy-paste-sync.pass
```

Run on Linux:

```
$ stack run -- linux 3232 http://192.168.8.2:3232 $HOME/copy-paste-sync.pass
```
