- [Exwm-X](#orga2fb85c)
  - [What is Exwm-X](#org50086c5)
  - [Feature](#orga48b67f)
  - [Pictures and videos](#org6720290)
  - [Install](#org4326b19)
  - [Configure](#orgb6d4cdd)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org2c5224f)
    - [Edit emacs configure](#orgb069aef)


<a id="orga2fb85c"></a>

# Exwm-X


<a id="org50086c5"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="orga48b67f"></a>

## Feature

1.  Shortcuts, toolbar and other window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.


<a id="org6720290"></a>

## Pictures and videos

1.  Tilling windows

![img](./snapshots/tilling-window.png)

1.  Floating windows

![img](./snapshots/floating-window.png)

1.  Exwm-X videos

<https://github.com/tumashu/exwm-x-videos>


<a id="org4326b19"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="orgb6d4cdd"></a>

## Configure


<a id="org2c5224f"></a>

### Edit "~/.initrc" file or "~/.xsession" file

You should edit "~/.initrc" file or "~/.xsession" file like below example:

    # The below line make sure "exwm-x-example" package correct loaded,
    # don't delete!
    export exwm_x_enable="yes"

    # Emacs X input method (exim) setting
    # export XMODIFIERS=@im=exim
    # export GTK_IM_MODULE=xim
    # export QT_IM_MODULE=xim
    # export CLUTTER_IM_MODULE=xim

    # xhost +

    exec dbus-launch --exit-with-session emacs


<a id="orgb069aef"></a>

### Edit emacs configure

Add the below two lines to your emacs configure file:

    (require 'exwm-x)
    (require 'exwm-x-example) ;; Adjust this line.

Note: Package "exwm-x-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)


Converted from exwm-x.el by [el2org](https://github.com/tumashu/el2org) .