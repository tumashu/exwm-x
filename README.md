- [Exwm-X](#orgeca88f1)
  - [What is Exwm-X](#org2f10374)
  - [Feature](#orgdb668a0)
  - [Pictures](#org89eeb66)
  - [Install](#orgb5f459d)
  - [Configure](#orge78e63a)
    - [Edit "~/.initrc" file or "~/.xsession" file](#orgf3511e1)
    - [Make "~/.initrc" or "~/.xsession" excutable](#orga013d5c)
    - [Edit "~/.exwm.el"](#org4a02db4)


<a id="orgeca88f1"></a>

# Exwm-X


<a id="org2f10374"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="orgdb668a0"></a>

## Feature

1.  Window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.


<a id="org89eeb66"></a>

## Pictures

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)


<a id="orgb5f459d"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="orge78e63a"></a>

## Configure


<a id="orgf3511e1"></a>

### Edit "~/.initrc" file or "~/.xsession" file

You should edit "~/.initrc" file or "~/.xsession" file like below example:

    # Emacs X input method (exim) setting
    # export XMODIFIERS=@im=exim
    # export GTK_IM_MODULE=xim
    # export QT_IM_MODULE=xim
    # export CLUTTER_IM_MODULE=xim

    xhost +SI:localuser:$USER

    # Fallback cursor
    # xsetroot -cursor_name left_ptr

    # Keyboard repeat rate
    # xset r rate 200 60

    exec dbus-launch --exit-with-session emacs --eval '(cond ((file-exists-p "~/.exwm") (load-file "~/.exwm")) ((not (featurep (quote exwm))) (require (quote exwm)) (require (quote exwm-config)) (exwm-config-default) (message "exwm configuration not found. Falling back to default configuration...")))'


<a id="orga013d5c"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="org4a02db4"></a>

### Edit "~/.exwm.el"

Add the below two lines to your emacs configure file:

    (add-to-list 'load-path "/path/to/exwm-x")
    (require 'exwm-x)
    (require 'exwmx-xfce)
    (require 'exwmx-example) ;; Adjust this line.

Note: Package "exwmx-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)