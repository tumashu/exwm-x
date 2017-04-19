- [Exwm-X](#orgf3549d0)
  - [What is Exwm-X](#orgf991c6d)
  - [Feature](#org02fd42d)
  - [Pictures and videos](#org97882ac)
  - [Install](#org0ab460b)
  - [Configure](#orge8ace6f)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org7d5c148)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org81ba65d)
    - [Edit "~/.exwm.el"](#orga5af899)


<a id="orgf3549d0"></a>

# Exwm-X


<a id="orgf991c6d"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="org02fd42d"></a>

## Feature

1.  Window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.


<a id="org97882ac"></a>

## Pictures and videos

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)


<a id="org0ab460b"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="orge8ace6f"></a>

## Configure


<a id="org7d5c148"></a>

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


<a id="org81ba65d"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="orga5af899"></a>

### Edit "~/.exwm.el"

Add the below two lines to your emacs configure file:

    (add-to-list 'load-path "/path/to/exwm-x")
    (require 'exwm-x)
    (require 'exwmx-example) ;; Adjust this line.

Note: Package "exwmx-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)