# SonarLint for Emacs
![License: GPLv3](https://img.shields.io/badge/License-GPLv3-blue.svg)
[![MELPA](https://melpa.org/packages/lsp-sonarlint-badge.svg)](https://melpa.org/#/lsp-sonarlint)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-sonarlint-badge.svg)](https://stable.melpa.org/#/lsp-sonarlint)
<a href="https://www.sonarlint.org/"> <img align="right" width="120" src="./images/SonarLint_icon.svg"></a>

SonarLint™ is a free IDE extension that lets you fix coding issues before they exist!

Like a spell checker,it highlights Bugs and Security Vulnerabilities as you write code, with clear remediation guidance so you can fix them before the code is even committed. 

In Emacs supports analysis of JavaScript, TypeScript, Python, Java, HTML, PHP, Go, and XML out of the box!

:warning: This is not an official [SonarSource](https://www.sonarsource.com/) extension

![Flycheck gif](https://gitlab.com/sasanidas/lsp-sonarlint/-/raw/master/examples/sonarlint-example.gif "Flycheck gif")
## Requirements

- emacs >= 25
- [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
- [ht](https://github.com/Wilfred/ht.el)
- [dash](https://github.com/magnars/dash.el)

The language server needs a Java Runtime (JRE) 8 or 11. If one is already installed on your computer, SonarLint should automatically find and use it.

To analyze JavaScript and TypeScript, SonarLint will also need Node.js.


## Installation

## Source
Download and include the main file lsp-sonarlint.el and the folders languages and server.

### Melpa
You can then run the following commands to install lsp-sonarlint:

```
M-x package-refresh-contents RET      (to refresh your package database)
M-x package-install RET lsp-sonarlint RET   (to install and compile `lsp-sonarlint` and its dependencies)
```

## Usage
The language server relies on java plugins to properly analyze the selected language source code.
The basic workflow to activate a plugin for a language is:

 - Enable language specific extension, alongside lsp-sonarlint:

``` lisp
(require 'lsp-sonarlint)
(require 'lsp-sonarlint-LANGUAGENAME)
``` 

- It is important to enable the desired languages **before** execute the lsp client:

``` lisp
(setq lsp-sonarlint-LANGUAGENAME-enabled t)
``` 

- If the extension is not installed in the path **lsp-sonarlint-LANGUAGENAME-analyzer-path**, it will ask if you want to download it from
the URL defined in **lsp-sonarlint-LANGUAGENAME-download-url**.

- If everything went well, you can now activate the *lsp* emacs command.


#### Complete config example
In this example, we have a multiple language project, with Javascript Typescript, HTML and PHP:

``` lisp
(require 'lsp-sonarlint)

(require 'lsp-sonarlint-php)
(setq lsp-sonarlint-php-enabled t)

(require 'lsp-sonarlint-html)
(setq lsp-sonarlint-html-enabled t)

(require 'lsp-sonarlint-javascript)
(setq lsp-sonarlint-javascript-enabled t)

(require 'lsp-sonarlint-typescript)
(setq lsp-sonarlint-typescript-enabled t)
``` 

Now we can activate the lsp extension.

The extension will check every plugin path and ask if it is not find to download it,
the default path is defined in **lsp-sonarlint-LANGUAGENAME-analyzer-path**.


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
- [Bidi (bidirectional unicode characters)](https://rules.sonarsource.com/text/) + [Secrets](https://rules.sonarsource.com/secrets/)

## Supported settings

* `lsp-sonarlint-server-path` - Path of the sonarlint jar executable file.
* `lsp-sonarlint-modes-enabled` - List of major modes where the lsp server will activate.
* `lsp-sonarlint-disable-telemetry` - Disable telemetry option (disabled by default).
* `lsp-sonarlint-test-file-pattern` - Regex to find test file, most rules are not evaluated on test files.
* `lsp-sonarlint-show-analyzer-logs` - Show analyzer logs.
* `lsp-sonarlint-verbose-logs` - Make SonarLint logs verbose.
* `lsp-sonarlint-vscode-plugin-url` - SonarLint language server download URL.
* `lsp-sonarlint-plugin-autodownload` - Not ask for confirmation and download analyzers if they are missing.

### Plugins supported settings
This settigns are common for all the language plugins.

* `lsp-sonarlint-LANGUAGE-enabled` - Enable LANGUAGE lsp-sonarlint plugin (disable by default)
* `lsp-sonarlint-LANGUAGE-download-url` - URL to download the LANGUAGE sonarlint plugin
* `lsp-sonarlint-LANGUAGE-analyzer-path` - Location where the plugin/anlyzer is located.
* `lsp-sonarlint-LANGUAGE-doc-url` - Sonarsource official plugin documentation
* `lsp-sonarlint-LANGUAGE-repository-url` - Plugin source code

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
