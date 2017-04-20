- [Exwm-X](#orge95301e)
  - [What is Exwm-X](#org9d4d695)
  - [Feature](#orge37d6f2)
  - [Pictures](#orge2f80e2)
  - [Install](#org3a4f908)
  - [Configure](#org1a3d661)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org0a688d8)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org97307e2)
    - [Edit "~/.exwm"](#org7e534d0)


<a id="orge95301e"></a>

# Exwm-X


<a id="org9d4d695"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="orge37d6f2"></a>

## Feature

1.  Window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.
4.  Dmenu, just dynamic menu


<a id="orge2f80e2"></a>

## Pictures

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)


<a id="org3a4f908"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="org1a3d661"></a>

## Configure


<a id="org0a688d8"></a>

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


<a id="org97307e2"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="org7e534d0"></a>

### Edit "~/.exwm"

Add the below lines to your emacs configure file:

    (add-to-list 'load-path "/path/to/exwm-x")
    (require 'exwm)
    (require 'exwm-x)
    (require 'exwmx-xfce)
    (require 'exwmx-example) ;; Adjust this line.

Note: Package "exwmx-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)