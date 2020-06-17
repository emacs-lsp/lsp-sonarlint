# SonarLint for Emacs

SonarLint is a free IDE extension that lets you fix coding issues before they exist! Like a spell checker, SonarLint highlights Bugs and Security Vulnerabilities as you write code, with clear remediation guidance so you can fix them before the code is even committed. 
SonarLint in Emacs supports analysis of JavaScript, TypeScript, Python, Java, HTML , PHP, Ruby, Scala and XML out of the box!
## Requirements

- emacs >= 25
- [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
- [curl](https://curl.haxx.se/)
- [ht](https://github.com/Wilfred/ht.el)
- [dash](https://github.com/magnars/dash.el)


## Installation

## Source
Download and include the main file lsp-sonarlint.el and the folders languages and server.

### Melpa
WIP

## Usage
Sonarlint language server relies on java plugins to properly analyze the selected language source code.
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
In this example, we have a multiple language project, with javascript,HTML and PHP:

``` lisp
(require 'lsp-sonarlint)

(require 'lsp-sonarlint-php)
(setq lsp-sonarlint-php-enabled t)

(require 'lsp-sonarlint-html)
(setq lsp-sonarlint-html-enabled t)


(require 'lsp-sonarlint-javascript)
(setq lsp-sonarlint-javascript-enabled t)

``` 
Now we can active the lsp extension.

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
- [Scala rules](https://rules.sonarsource.com/scala)
- [Ruby rules](https://rules.sonarsource.com/ruby)
- [XML rules](https://rules.sonarsource.com/xml)

## Requirements

The SonarLint language server needs a Java Runtime (JRE) 8 or 11. If one is already installed on your computer, SonarLint should automatically find and use it.

If a suitable JRE cannot be found at the usual places, SonarLint will ask for your permission to download and manage its own version.

To analyze JavaScript and TypeScript, SonarLint will also need Node.js.

## Data and telemetry

This extension collects anonymous usage data and sends it to SonarSource. Collection of telemetry is controlled via the setting: `lsp-sonarlint-disable-telemetry`. Click [here](https://github.com/SonarSource/sonarlint-vscode/blob/master/telemetry-sample.md) to see a sample of the data that are collected.

## License
lsp-sonarlint Copyright © 2020 by Fermin Munoz <fmfs@posteo.net>

lsp-sonarlint is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

lsp-sonarlint is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with lsp-sonarlint.  If not, see <http://www.gnu.org/licenses/>.
