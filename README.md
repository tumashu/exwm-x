- [Exwm-X](#orgebd3335)
  - [What is Exwm-X](#org1e57380)
  - [Feature](#orge59e883)
  - [Pictures and videos](#org92fd3d5)
  - [Install](#org2eaa15d)
  - [Configure](#orgafde6d1)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org0f39afc)
    - [Make "~/.initrc" or "~/.xsession" excutable](#orgc587973)
    - [Edit "~/.exwm.el"](#orgb608db8)


<a id="orgebd3335"></a>

# Exwm-X


<a id="org1e57380"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="orge59e883"></a>

## Feature

1.  Shortcuts, toolbar and other window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.


<a id="org92fd3d5"></a>

## Pictures and videos

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)

3.  Exwmx videos

    <https://github.com/tumashu/exwmx-videos>


<a id="org2eaa15d"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwmx RET


<a id="orgafde6d1"></a>

## Configure


<a id="org0f39afc"></a>

### Edit "~/.initrc" file or "~/.xsession" file

You should edit "~/.initrc" file or "~/.xsession" file like below example:

    # Emacs X input method (exim) setting
    # export XMODIFIERS=@im=exim
    # export GTK_IM_MODULE=xim
    # export QT_IM_MODULE=xim
    # export CLUTTER_IM_MODULE=xim

    xhost +SI:localuser:$USER

    exec dbus-launch --exit-with-session emacs --eval "(if (file-exists-p \"~/.exwm.el\")(load \"~/.exwm.el\")(require 'exwm)(require 'exwm-config)(exwm-config-default)(message \"EXWM: ~/.exwm.el is not exist. use exwm fallback configure.\"))"


<a id="orgc587973"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="orgb608db8"></a>

### Edit "~/.exwm.el"

Add the below two lines to your emacs configure file:

    (add-to-list 'load-path "/path/to/exwmx")
    (require 'exwm-x)
    (require 'exwmx-example) ;; Adjust this line.

Note: Package "exwmx-example" is Exwm-X buildin example, user can use it to test Exwmx's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)