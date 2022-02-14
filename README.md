[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# preview-it
> Preview anything at point.

[![CI](https://github.com/jcs-elpa/preview-it/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/preview-it/actions/workflows/test.yml)

Preview `file`, `image`, and `website` by moving cursor on top of target.

## 🏆 Features

This package current supports viewing these items.

* File (absolute/relative)
* Image
* Url/Link

| Preview File                 | Preview Image                 |
|:-----------------------------|:------------------------------|
| <img src="./etc/file.png" /> | <img src="./etc/image.png" /> |

| Preview Url                 |
|:----------------------------|
| <img src="./etc/url.png" /> |

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

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
