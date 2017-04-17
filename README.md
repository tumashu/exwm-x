- [Exwm-X](#org43e0e12)
  - [What is Exwm-X](#orgd42997a)
  - [Feature](#org2f2d081)
  - [Pictures and videos](#orgd251876)
  - [Install](#org9210041)
  - [Configure](#org928d822)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org9323351)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org79b9074)
    - [Edit "~/.exwm.el"](#orgdb0cb50)


<a id="org43e0e12"></a>

# Exwm-X


<a id="orgd42997a"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="org2f2d081"></a>

## Feature

1.  Shortcuts, toolbar and other window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.


<a id="orgd251876"></a>

## Pictures and videos

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)

3.  Exwm-X videos

    <https://github.com/tumashu/exwm-x-videos>


<a id="org9210041"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="org928d822"></a>

## Configure


<a id="org9323351"></a>

### Edit "~/.initrc" file or "~/.xsession" file

You should edit "~/.initrc" file or "~/.xsession" file like below example:

    # Emacs X input method (exim) setting
    # export XMODIFIERS=@im=exim
    # export GTK_IM_MODULE=xim
    # export QT_IM_MODULE=xim
    # export CLUTTER_IM_MODULE=xim

    xhost +SI:localuser:$USER

    exec dbus-launch --exit-with-session emacs --eval "(if (file-exists-p \"~/.exwm.el\")(load \"~/.exwm.el\")(require 'exwm)(require 'exwm-config)(exwm-config-default)(message \"EXWM: ~/.exwm.el is not exist. use exwm fallback configure.\"))"


<a id="org79b9074"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="orgdb0cb50"></a>

### Edit "~/.exwm.el"

Add the below two lines to your emacs configure file:

    (add-to-list 'load-path "/path/to/exwm-x")
    (require 'exwm-x)
    (require 'exwm-x-example) ;; Adjust this line.

Note: Package "exwm-x-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)