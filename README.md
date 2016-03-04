- [Exwm-X](#exwm-x)
  - [What is Exwm-X](#what-is-exwm-x)
  - [Feature](#feature)
  - [Snapshot](#snapshot)
    - [Tilling windows](#tilling-windows)
    - [Floating windows](#floating-windows)
  - [Install](#install)
  - [Configure](#configure)

# Exwm-X<a id="orgheadline8"></a>

## What is Exwm-X<a id="orgheadline1"></a>

Exwm-X is an extension of exwm (emacs x window manager), which can
make exwm easier for Mouse-Control-People to use.

## Feature<a id="orgheadline2"></a>

1.  Shortcuts, toolbar and other window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.

## Snapshot<a id="orgheadline5"></a>

### Tilling windows<a id="orgheadline3"></a>

![img](./snapshots/tilling-window.png)

### Floating windows<a id="orgheadline4"></a>

![img](./snapshots/floating-window.png)

## Install<a id="orgheadline6"></a>

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET

## Configure<a id="orgheadline7"></a>

    (require 'exwm-x)
    (require 'exwm-x-example) ;; Adjust this line.

Note: exwm-x-example is Exwm-X buildin example, usr can use it to test Exwm-X's
features. If it doesn't suit for your need, just copy and paste its useful pieces
to your own exwm config :-)
