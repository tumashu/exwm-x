- [Exwm-X](#org5d1f74a)
  - [What is Exwm-X](#orgda9fee3)
  - [Showcase](#org72101c5)
  - [Feature](#org3d44ee2)
    - [Appconfig](#orgfc6682d)
    - [Buttons](#org8d66bfd)
    - [Move or resize a floating-window without press WIN key.](#orgeac25ef)
    - [Jump-or-exec](#org8782572)
    - [Dynamic menu](#orgab4035e)
    - [Send a string to application](#orgb8ee4e2)
  - [Install](#orgd05e578)
  - [Configure](#org06e18a3)
    - [Add exwm-x directory to emacs's load-path](#orgc8ee873)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org45d2ea3)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org52edfe1)
    - [Edit "~/.exwm-x"](#orgc51339e)


<a id="org5d1f74a"></a>

# Exwm-X


<a id="orgda9fee3"></a>

## What is Exwm-X

Exwm-X is a derivative window manager based on EXWM (emacs x window manager), which focus on Mouse-Control-People.


<a id="org72101c5"></a>

## Showcase

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)


<a id="org3d44ee2"></a>

## Feature


<a id="orgfc6682d"></a>

### Appconfig

\`exwmx-appconfig' is a database manager, which is used to record and manage appconfigs (an appconfig is a plist of application's information), when run command \`exwmx-appconfig', a buffer with appconfig-template will be poped up, user can edit the template and run \`exwmx-appconfig-finish' to save the change or run \`exwmx-appconfig-ignore' to ignore the change.

All appconfigs will be saved into file: \`exwmx-appconfig-file'.

By default, every appconfig have the following keys:

1.  :command

    Record the shell command of application.

2.  :alias

    Define alias of an application, this key is used by \`exwmx-jump-or-exec'.

3.  :pretty-name

    In EXWM and Exwm-X, an application is assocated with an emacs buffer, user can set the buffer's name with :pretty-name.

4.  :paste-key

    Record the paste keybinding of an application, this key is used by \`exwmx-sendstring'.

5.  :class

    Record the application's class, this key is used by \`exwmx-jump-or-exec'.

6.  :instance

    Record the application's instance, this key is used by \`exwmx-jump-or-exec'.

7.  :title

    Record the application's title, this key is used by \`exwmx-jump-or-exec'.


<a id="org8d66bfd"></a>

### Buttons

Exwm-X add the following **buttons** to mode-line, user can click them to operate application's window:

1.  [X]: Delete the current application.
2.  [D]: Delete the current emacs window.
3.  [R]: Run \`exwm-reset'.
4.  [F]: Toggle floating/tilling window.
5.  [<]: Move window border to left.
6.  [+]: Maximize the current window.
7.  [>]: Move window border to right.
8.  [-]: Split window horizontal.
9.  [|]: Split window vertical.
10. [\_]: minumize floating application
11. [Z+]: Zoom+ floating application's window
12. [Z-]: Zoom- floating application's window
13. [Line 'XXXX']: toggle EXWM char-mode/line-mode


<a id="orgeac25ef"></a>

### Move or resize a floating-window without press WIN key.

By default, EXWM use "s-'down-mouse-1'" to move a floating-window and "s-'down-mouse-3'" to resize a floating-window.

When Exwm-X is enabled, user can drag **title showed in mode-line** to move a floating-window. and click '[Z+]' and '[Z-]' in mode-line to resize a floating-window.


<a id="org8782572"></a>

### Jump-or-exec

If the application's window is found, jump to this window, otherwise, launch the application with command, this feature need appconfigs stored in \`exwmx-appconfig-file'.

1.  The simplest usage

        (exwmx-jump-or-exec "firefox")

2.  Define an alias

    Search an appconfig which :alias is "web-browser", and run this appconfig's :command.

        (exwmx-jump-or-exec "web-browser" nil t)


<a id="orgab4035e"></a>

### Dynamic menu

\`exwmx-dmenu' let user input a dmenu command in minibuffer, and execute it, ivy is used to complete.

\`exwmx-dmenu' support some command prefixes:

1.  "," -> run a dmenu-command in terminal emulator, for example, dmenu-command ",top" will execute a terminal emulator, then run shell command: "top" .

    Note: user can change terminal emulator with the help of variable \`exwmx-terminal-emulator'.
2.  ";" -> run an emacs command which name is exwm:<input>, for example, dmenu-command ";firefox" will run emacs command exwm:firefox.
3.  "-" -> split window top-to-bottom, for example, the result of dmenu-command "-32" is that 3 windows on top and 2 windows in buttom.
4.  "|" -> split window left-to-right, for example, the result of dmenu-command "|32" is that 3 windows at left and 2 window at right

User can customize the prefixes of \`exwmx-dmenu' with the help of \`exwmx-dmenu-prefix-setting'.


<a id="orgb8ee4e2"></a>

### Send a string to application

When run \`exwmx-sendstring', a buffer will be poped up to let user edit. after run command \`exwmx-sendstring-finish', the content of the buffer will be sent to the input field of current application.

\`exwmx-sendstring-from-minibuffer' is a simple version of \`exwmx-sendstring', it use minibuffer to get input.

\`exwmx-sendstring-from-kill-ring' can select a string in kill-ring then send this string to application.

NOTE: if \`exwmx-sendstring' can not work well with an application, user should set :paste-key of this application with the help of \`exwmx-appconfig'.


<a id="orgd05e578"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="org06e18a3"></a>

## Configure


<a id="orgc8ee873"></a>

### Add exwm-x directory to emacs's load-path

Pasting the below line to "~/.emacs" is a simple way.

    (add-to-list 'load-path "/path/to/exwm-x")


<a id="org45d2ea3"></a>

### Edit "~/.initrc" file or "~/.xsession" file

You should edit "~/.initrc" file or "~/.xsession" file like below example:

    # Fallback cursor
    # xsetroot -cursor_name left_ptr

    # Keyboard repeat rate
    # xset r rate 200 60

    xhost +SI:localuser:$USER

    exec dbus-launch --exit-with-session emacs --eval '(require (quote exwmx-loader))'


<a id="org52edfe1"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="orgc51339e"></a>

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

Note: Package "exwmx-example" is Exwm-X buildin example, user can use it to test Exwm-X's features:

| Key       | command                         |
|--------- |------------------------------- |
| "C-t ;"   | exwmx-dmenu                     |
| "C-t C-e" | exwmx-sendstring                |
| "C-t C-r" | exwmx-appconfig                 |
| "C-t 1"   | exwmx-switch-to-1-workspace     |
| "C-t 2"   | exwmx-switch-to-2-workspace     |
| "C-t 3"   | exwmx-switch-to-3-workspace     |
| "C-t 4"   | exwmx-switch-to-4-workspace     |
| "C-x o"   | switch-window                   |
| "C-c y"   | exwmx-sendstring-from-kill-ring |

If it doesn't suit for your need, just copy and paste its useful pieces to your own exwm config :-)


Converted from exwm-x.el by [el2org](https://github.com/tumashu/el2org) .