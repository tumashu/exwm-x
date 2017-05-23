- [Exwm-X](#orgfe5ddb0)
  - [What is Exwm-X](#orgb25cba9)
  - [Feature](#org40a12dc)
  - [Pictures](#org7959073)
  - [Install](#org6cf867a)
  - [Configure](#orga5029c7)
    - [Add exwm-x directory to emacs's load-path](#org2dd604d)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org8449ee8)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org5b46c76)
    - [Edit "~/.exwm-x"](#orgd427192)


<a id="orgfe5ddb0"></a>

# Exwm-X


<a id="orgb25cba9"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="org40a12dc"></a>

## Feature

1.  Window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.
4.  Dmenu, just dynamic menu
5.  Emacs's kill-ring integration


<a id="org7959073"></a>

## Pictures

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)


<a id="org6cf867a"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="orga5029c7"></a>

## Configure


<a id="org2dd604d"></a>

### Add exwm-x directory to emacs's load-path

Pasting the below line to "~/.emacs" is a simple way.

    (add-to-list 'load-path "/path/to/exwm-x")


<a id="org8449ee8"></a>

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

    exec dbus-launch --exit-with-session emacs --eval '(require (quote exwmx-loader))'


<a id="org5b46c76"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="orgd427192"></a>

### Edit "~/.exwm-x"

Add your exwm config to this file, for example:

    (require 'exwm)
    (require 'exwm-x)
    (require 'exwmx-xfce)
    (require 'exwmx-example)
    (exwm-input-set-key (kbd "C-t v") 'exwmx:thunar)
    (exwm-input-set-key (kbd "C-t f") 'exwmx:icecat)
    (exwm-input-set-key (kbd "C-t c") 'exwmx:xfce4-terminal)
    (exwm-input-set-key (kbd "C-t C-c") 'exwmx:xfce4-new-terminal)

Note: Package "exwmx-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)


Converted from exwm-x.el by [el2org](https://github.com/tumashu/el2org) .