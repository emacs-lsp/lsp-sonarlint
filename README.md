# SonarLint for Emacs
![License: GPLv3](https://img.shields.io/badge/License-GPLv3-blue.svg)
[![MELPA](https://melpa.org/packages/lsp-sonarlint-badge.svg)](https://melpa.org/#/lsp-sonarlint)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-sonarlint-badge.svg)](https://stable.melpa.org/#/lsp-sonarlint)
<a href="https://www.sonarlint.org/"> <img align="right" width="120" src="./images/SonarLint_icon.svg"></a>

SonarLint™ is a free IDE extension that lets you fix coding issues before they exist!

Like a spell checker,it highlights Bugs and Security Vulnerabilities as you write code, with clear remediation guidance so you can fix them before the code is even committed.

In Emacs supports analysis of JavaScript, TypeScript, Python, Java, HTML, PHP, C/C++,
Go, and XML out of the box!

:warning: This is not an official [SonarSource](https://www.sonarsource.com/) extension

![Flycheck gif](https://gitlab.com/sasanidas/lsp-sonarlint/-/raw/master/examples/sonarlint-example.gif "Flycheck gif")

## How it works

lsp-sonarlint relies on the [official vscode extension](https://github.com/SonarSource/sonarlint-vscode/), which is downloaded and unzipped.

The official VSCode extension typically contains :
- A language server (written in java)
- A Java runtime
- Analysers for 10+ languages
- A VSCode front-end

lsp-sonarlint is emacs's equivalent of the VSCode front-end, but only
implements the base feature set, i.e linting and rules viewing.

## Requirements

- emacs >= 25
- [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
- [ht](https://github.com/Wilfred/ht.el)
- [dash](https://github.com/magnars/dash.el)

To analyze JavaScript and TypeScript, SonarLint will also need Node.js.

To analyse C/C++ projects, SonarLint's CFamily analyzer will need both a working compiler and a
compile_commands.json which is typically generated by
[cmake](https://cmake.org/cmake/help/latest/variable/CMAKE_EXPORT_COMPILE_COMMANDS.html),
[bear](https://github.com/rizsotto/Bear), or
[Sonar Build Wrapper](https://docs.sonarsource.com/sonarcloud/advanced-setup/languages/c-family/prerequisites/#using-build-wrapper).

## Installation

## Source
Download and include the main file lsp-sonarlint.el.

### Melpa
You can then run the following commands to install lsp-sonarlint:

```
M-x package-refresh-contents RET      (to refresh your package database)
M-x package-install RET lsp-sonarlint RET   (to install and compile `lsp-sonarlint` and its dependencies)
```

## Usage
The language server relies on java plugins to properly analyze the selected language source code.

By default all are enabled, but you can specify the ones you want.

``` lisp
(use-package lsp-sonarlint
  :custom
  ;; Allow sonarlint to download and unzip the official VSCode extension
  ;; If nil, you'll have to do that yourself. See also `lsp-sonarlint-download'
  ;; `lsp-sonarlint-download-url' and `lsp-sonarlint-download-dir'
  (lsp-sonarlint-auto-download t)

  ;; Choose which analyzers you want enabled. By default all are enabled
  ;; See command `lsp-sonarlint-available-analyzers' for the full list.
  (lsp-sonarlint-enabled-analyzers '("java" "cfamily" "python" "text")))
```

> [!WARNING]
> On windows, lsp-sonarlint may encounter [this issue](https://github.com/emacs-lsp/lsp-mode/issues/3022) while unzipping the VSCode extension. If you do, try the following :
```elisp
(setq lsp-unzip-script lsp-ext-pwsh-script)
```

## Static Analysis Rules

Out of the box, SonarLint automatically checks your code against the following rules:

- [JavaScript rules](https://rules.sonarsource.com/javascript)
- [TypeScript rules](https://rules.sonarsource.com/typescript)
- [Python rules](https://rules.sonarsource.com/python)
- [Java rules](https://rules.sonarsource.com/java)
- [HTML rules](https://rules.sonarsource.com/html)
- [PHP rules](https://rules.sonarsource.com/php)
- [XML rules](https://rules.sonarsource.com/xml)
- [Go rules](https://rules.sonarsource.com/go)
- [C rules](https://rules.sonarsource.com/c)
- [C++ rules](https://rules.sonarsource.com/cpp)
- [Bidi (bidirectional unicode characters)](https://rules.sonarsource.com/text/) + [Secrets](https://rules.sonarsource.com/secrets/)

## Supported settings

* `lsp-sonarlint-auto-download` - Set to t to enable auto-downloading of VSCode's extension on startup.
* `lsp-sonarlint-download-url` - Specify another URL for the VSCode extension.
* `lsp-sonarlint-download-dir` - Specify where VSCode's extension will be downloaded and unzipped.
* `lsp-sonarlint-use-system-jre` - If t, use the system Java runtime instead of the bundled one.
* `lsp-sonarlint-enabled-analyzers` - List of analyzers to enable. Defaults to 'all for all analyzers.
* `lsp-sonarlint-modes-enabled` - List of major modes where the lsp server will activate.
* `lsp-sonarlint-disable-telemetry` - Disable telemetry option (disabled by default).
* `lsp-sonarlint-test-file-pattern` - Regex to find test file, most rules are not evaluated on test files.
* `lsp-sonarlint-show-analyzer-logs` - Show analyzer logs.
* `lsp-sonarlint-verbose-logs` - Make SonarLint logs verbose.
* `lsp-sonarlint-cfamily-compilation-commands-path` - Path to compile_commands.json for C/C++ analysis.

### Available commands
* `lsp-sonarlint-download` - Download the VSCode extension and unzip it. Called automatically
if `lsp-sonarlint-auto-download`is set to t
* `lsp-sonarlint-available-analyzers` - List all available analyzers provided by the downloaded VSCode extension.

### Plugins additional info

For most analyzers, lsp-sonarlint provides variables describing additional
info.

* `lsp-sonarlint-LANGUAGE-doc-url` - Sonarsource official plugin documentation
* `lsp-sonarlint-LANGUAGE-repository-url` - Plugin source code

### Plugins not tested yet
Currently, sonarlint's vscode extension also provides omnisharp and Infrastructure As Code (IAC)
analyzers. They have not been tested yet, you may expect some additional configuration
to make them work.
You'll at least need to add the major-modes to `lsp-sonarlint-modes-enabled'.

Feel free to try them out and provide feedback.

## Data and telemetry

This extension collects anonymous usage data and sends it to SonarSource.

Collection of telemetry is controlled via the setting: `lsp-sonarlint-disable-telemetry`, it is disable by default.

Click [here](https://github.com/SonarSource/sonarlint-vscode/blob/master/telemetry-sample.md) to see a sample of the data that are collected.

## Additional packages

* [lsp-ui](https://github.com/emacs-lsp/lsp-ui) : Flycheck, documentation and code actions support.
* [company-capf](https://github.com/company-mode/company-mode) : Completion backend support.
* [treemacs](https://github.com/Alexander-Miller/treemacs) : Project viewer.
* [lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs) : `lsp-mode` GUI controls implemented using treemacs.

## Contributions

Contributions are very much welcome.

#### Copyright

SONARLINT and SONARSOURCE are trademarks of SonarSource SA.
All other trademarks and copyrights are the property of their respective owners.
