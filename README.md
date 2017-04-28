- [Exwm-X](#org34c4a86)
  - [What is Exwm-X](#org1b9f042)
  - [Feature](#org853483d)
  - [Pictures](#orgd443ea7)
  - [Install](#orgdb61476)
  - [Configure](#org7e95316)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org1e7443b)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org14b20aa)
    - [Edit "~/.exwm"](#org6db4de6)


<a id="org34c4a86"></a>

# Exwm-X


<a id="org1b9f042"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="org853483d"></a>

## Feature

1.  Window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.
4.  Dmenu, just dynamic menu


<a id="orgd443ea7"></a>

## Pictures

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)


<a id="orgdb61476"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="org7e95316"></a>

## Configure


<a id="org1e7443b"></a>

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


<a id="org14b20aa"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="org6db4de6"></a>

### Edit "~/.exwm"

Add your exwm config to this file, for example:

    (add-to-list 'load-path "/path/to/exwm-x")
    (require 'exwm)
    (require 'exwm-x)
    (require 'exwmx-xfce)
    (require 'exwmx-example)
    (exwm-input-set-key (kbd "C-t v") 'exwmx:thunar)
    (exwm-input-set-key (kbd "C-t c") 'exwmx:xfce4-terminal)
    (exwm-input-set-key (kbd "C-t f") 'exwmx:icecat)
    (exwm-input-set-key (kbd "C-t C-x") 'exwmx:xfce4-new-terminal)
    (exwm-input-set-key (kbd "C-t C-t") 'exwmx:xfce4-terminal)

Note: Package "exwmx-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)


Converted from exwm-x.el by [el2org](https://github.com/tumashu/el2org) .