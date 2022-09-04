[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/preview-it.svg)](https://jcs-emacs.github.io/jcs-elpa/#/preview-it)

# preview-it
> Preview anything at point.

[![CI](https://github.com/jcs-elpa/preview-it/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/preview-it/actions/workflows/test.yml)

Preview `file`, `image`, and `website` by moving cursor on top of target.

## 🏆 Features

This package current supports viewing these items.

* File (absolute/relative)
* Image
* Url/Link

| Preview File       | Preview Image        |
|:-------------------|:---------------------|
| ![](./etc/url.png) | ![](./etc/image.png) |

| Preview Url        | Color                |
|:-------------------|:---------------------|
| ![](./etc/url.png) | ![](./etc/color.png) |

## 🔨 Usage

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

## 🔧 Customization

#### 🧪 Variables

* `preview-it-delay` - Seconds delay to show preview.
* `preview-it-render-md` - Set to non-nil, render markdown file.
* `preview-it-color-text` - String to display color.

#### 🧪 Face

* `preview-it-background` - Background color of the preview buffer.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
