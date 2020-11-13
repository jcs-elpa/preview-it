[![Build Status](https://travis-ci.com/jcs-elpa/preview-it.svg?branch=master)](https://travis-ci.com/jcs-elpa/preview-it)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# preview-it
> Preview anything at point.

Preview `file`, `image`, and `website` by moving cursor on top of target.

## Features

This package current supports viewing these items.

* File (absolute/relative)
* Image
* Url/Link

| Preview File                                         | Preview Image                                         |
|:-----------------------------------------------------|:------------------------------------------------------|
| <img src="./etc/file.png" width="450" height="200"/> | <img src="./etc/image.png" width="450" height="200"/> |

| Preview Url                                         |
|:----------------------------------------------------|
| <img src="./etc/url.png" width="450" height="200"/> |

## Usage

You can enable this feature by the following command. So it will work every time
your cursor is hovering anything that can be preview.

```
M-x preview-it-mode
```

To enabled it globally, use the following command instead.

```
M-x global-preview-it-mode
```

You can also called `preview-it` function directly to preview thing at the current
cursor's position.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
