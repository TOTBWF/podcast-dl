# podcast-dl
A simple CLI application for downloading podcasts.

## Configuration
`config.yaml` should be located at `$XDG_CONFIG_HOME`.
The current options are:
- `output-dir`: Where the podcasts will be downloaded to. Supports `gvfs` filesystems.
- `urls`: A list of rss feed urls.

## Usage
Press `<Enter>` to expand a section, `d` to download, and `-` to minimize an
entire feed.

## Building
Note that you need to have `gobject` and `gobject-introspection` installed.
This is because uploading to mtpfs or other funky filesystems is often required.
``` sh
stack build
```

