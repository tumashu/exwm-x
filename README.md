- [Exwm-X](#org9eca8bc)
  - [What is Exwm-X](#orgeb8cf74)
  - [Feature](#orgee2c93e)
  - [Pictures and videos](#org5a5c9da)
  - [Install](#org1145e5c)
  - [Configure](#org2459463)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org7da755a)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org0c406e4)
    - [Edit "~/.exwm.el"](#org4fb75e6)


<a id="org9eca8bc"></a>

# Exwm-X


<a id="orgeb8cf74"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="orgee2c93e"></a>

## Feature

1.  Shortcuts, toolbar and other window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.


<a id="org5a5c9da"></a>

## Pictures and videos

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)

3.  Exwm-X videos

    <https://github.com/tumashu/exwm-x-videos>


<a id="org1145e5c"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="org2459463"></a>

## Configure


<a id="org7da755a"></a>

### Edit "~/.initrc" file or "~/.xsession" file

You should edit "~/.initrc" file or "~/.xsession" file like below example:

    # Emacs X input method (exim) setting
    # export XMODIFIERS=@im=exim
    # export GTK_IM_MODULE=xim
    # export QT_IM_MODULE=xim
    # export CLUTTER_IM_MODULE=xim

    xhost +SI:localuser:$USER

    exec dbus-launch --exit-with-session emacs --eval "(if (file-exists-p \"~/.exwm.el\")(load \"~/.exwm.el\")(require 'exwm)(require 'exwm-config)(exwm-config-default)(message \"EXWM: ~/.exwm.el is not exist. use exwm fallback configure.\"))"


<a id="org0c406e4"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.initrc
    chmod a+x ~/.xsession


<a id="org4fb75e6"></a>

### Edit "~/.exwm.el"

Add the below two lines to your emacs configure file:

    (require 'exwm-x)
    (require 'exwm-x-example) ;; Adjust this line.

Note: Package "exwm-x-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)