- [Exwm-X](#orgedc64bc)
  - [What is Exwm-X](#org24321e4)
  - [Feature](#org9eaf3c7)
  - [Pictures](#org1ea2780)
  - [Install](#orgfda856c)
  - [Configure](#org28d67d7)
    - [Add exwm-x directory to emacs's load-path](#orgab33c89)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org5d7c548)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org62a4fe4)
    - [Edit "~/.exwm-x"](#org1791e55)


<a id="orgedc64bc"></a>

# Exwm-X


<a id="org24321e4"></a>

## What is Exwm-X

Exwm-X is an extension of exwm (emacs x window manager), which can make exwm easier for Mouse-Control-People to use.


<a id="org9eaf3c7"></a>

## Feature

1.  Window operate buttons in mode-line.
2.  Move or resize a floating-window without press WIN key.
3.  Jump-or-exec, which will switch to an exist app instead of launch it again.
4.  Dmenu, just dynamic menu
5.  Emacs's kill-ring integration


<a id="org1ea2780"></a>

## Pictures

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)


<a id="orgfda856c"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="org28d67d7"></a>

## Configure


<a id="orgab33c89"></a>

### Add exwm-x directory to emacs's load-path

Pasting the below line to "~/.emacs" is a simple way.

    (add-to-list 'load-path "/path/to/exwm-x")


<a id="org5d7c548"></a>

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


<a id="org62a4fe4"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="org1791e55"></a>

### Edit "~/.exwm-x"

Add your exwm config to this file, for example:

    (require 'exwm)
    (require 'exwm-x)
    (require 'exwmx-xfce)
    (require 'exwmx-example)
    (exwm-input-set-key (kbd "C-t v") 'exwmx:file-browser)
    (exwm-input-set-key (kbd "C-t f") 'exwmx:web-browser)
    (exwm-input-set-key (kbd "C-t c") 'exwmx-xfce-terminal)
    (exwm-input-set-key (kbd "C-t C-c") 'exwmx-xfce-new-terminal)

Note: Package "exwmx-example" is Exwm-X buildin example, user can use it to test Exwm-X's features. If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)


Converted from exwm-x.el by [el2org](https://github.com/tumashu/el2org) .