- [Exwm-X](#orgd037ff5)
  - [What is Exwm-X](#orgf19aa98)
  - [Showcase](#org65fdc52)
  - [Feature](#org9cb0738)
    - [Appconfig](#org2063f30)
    - [Buttons](#orgd2f2d51)
    - [Move or resize a floating-window without press WIN key.](#org19598fb)
    - [Jump-or-exec](#orgc3a50b9)
    - [Dynamic menu](#org2c8deed)
    - [Send a string to application](#org72e4250)
  - [Install](#org7190f3c)
  - [Configure](#org8a0ccf5)
    - [Add exwm-x directory to emacs's load-path](#org809ebb4)
    - [Edit "~/.initrc" file or "~/.xsession" file](#org8161ec3)
    - [Make "~/.initrc" or "~/.xsession" excutable](#org99be776)
    - [Edit "~/.exwm-x"](#org53edf37)
  - [Usage](#org1209b1c)
    - [Build appconfig database](#org4670b34)
    - [The usage of "exwmx-example"](#org81c5c2a)


<a id="orgd037ff5"></a>

# Exwm-X


<a id="orgf19aa98"></a>

## What is Exwm-X

Exwm-X is a derivative window manager based on EXWM (emacs x window manager), which focus on Mouse-Control-People.


<a id="org65fdc52"></a>

## Showcase

1.  Tilling windows

    ![img](./snapshots/tilling-window.png)

2.  Floating windows

    ![img](./snapshots/floating-window.png)


<a id="org9cb0738"></a>

## Feature


<a id="org2063f30"></a>

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


<a id="orgd2f2d51"></a>

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


<a id="org19598fb"></a>

### Move or resize a floating-window without press WIN key.

By default, EXWM use "s-'down-mouse-1'" to move a floating-window and "s-'down-mouse-3'" to resize a floating-window.

When Exwm-X is enabled, user can drag **title showed in mode-line** to move a floating-window. and click '[Z+]' and '[Z-]' in mode-line to resize a floating-window.


<a id="orgc3a50b9"></a>

### Jump-or-exec

If the application's window is found, jump to this window, otherwise, launch the application with command, this feature need appconfigs stored in \`exwmx-appconfig-file'.

1.  The simplest usage

        (exwmx-jump-or-exec "firefox")

2.  Define an alias

    Search an appconfig which :alias is "web-browser", and run this appconfig's :command.

        (exwmx-jump-or-exec "web-browser" nil t)


<a id="org2c8deed"></a>

### Dynamic menu

\`exwmx-dmenu' let user input a dmenu command in minibuffer, and execute it, ivy is used to complete.

\`exwmx-dmenu' support some command prefixes:

1.  "," -> run a dmenu-command in terminal emulator, for example, dmenu-command ",top" will execute a terminal emulator, then run shell command: "top" .

    Note: user can change terminal emulator with the help of variable \`exwmx-terminal-emulator'.
2.  ";" -> run an emacs command which name is exwm:<input>, for example, dmenu-command ";firefox" will run emacs command exwm:firefox.
3.  "-" -> split window top-to-bottom, for example, the result of dmenu-command "-32" is that 3 windows on top and 2 windows in buttom.
4.  "|" -> split window left-to-right, for example, the result of dmenu-command "|32" is that 3 windows at left and 2 window at right

User can customize the prefixes of \`exwmx-dmenu' with the help of \`exwmx-dmenu-prefix-setting'.


<a id="org72e4250"></a>

### Send a string to application

When run \`exwmx-sendstring', a buffer will be poped up to let user edit. after run command \`exwmx-sendstring-finish', the content of the buffer will be sent to the input field of current application.

\`exwmx-sendstring-from-minibuffer' is a simple version of \`exwmx-sendstring', it use minibuffer to get input.

\`exwmx-sendstring-from-kill-ring' can select a string in kill-ring then send this string to application.

NOTE: if \`exwmx-sendstring' can not work well with an application, user should set :paste-key of this application with the help of \`exwmx-appconfig'.


<a id="org7190f3c"></a>

## Install

1.  Config melpa repository, please see：<http://melpa.org/#/getting-started>
2.  M-x package-install RET exwm-x RET


<a id="org8a0ccf5"></a>

## Configure


<a id="org809ebb4"></a>

### Add exwm-x directory to emacs's load-path

Pasting the below line to "~/.emacs" is a simple way.

    (add-to-list 'load-path "/path/to/exwm-x")


<a id="org8161ec3"></a>

### Edit "~/.initrc" file or "~/.xsession" file

You should edit "~/.initrc" file or "~/.xsession" file like below example:


    # Fallback cursor
    # xsetroot -cursor_name left_ptr

    # Keyboard repeat rate
    # xset r rate 200 60

    xhost +SI:localuser:$USER

    exec dbus-launch --exit-with-session emacs --eval '(require (quote exwmx-loader))'


<a id="org99be776"></a>

### Make "~/.initrc" or "~/.xsession" excutable

    chmod a+x ~/.xsession

or

    chmod a+x ~/.initrc


<a id="org53edf37"></a>

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


<a id="org1209b1c"></a>

## Usage


<a id="org4670b34"></a>

### Build appconfig database

When user **first** login in Exwm-X desktop environment, appconfigs of frequently used applications should be added to appconfig database file: \`exwmx-appconfig-file', it is simple but **very very** important, for many useful commands of Exwm-X need this database file, for example: \`exwmx-jump-or-exec', \`exwmx-sendstring' and so on.

user should do like the below:

1.  Launch an application with \`exwmx-dmenu'.
2.  Run command \`exwmx-appconfig'.
3.  Edit appconfig template
4.  Save
5.  Launch another application with \`exwmx-dmenu'.
6.  &#x2026;&#x2026;.


<a id="org81c5c2a"></a>

### The usage of "exwmx-example"

"exwmx-example" is Exwm-X buildin example, user can use it to test Exwm-X's features, the following is its keybindings. by the way, Exwm-X is a Exwm derivative, most Exwm commands can be used too :-)

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

If exwmx-example doesn't suit for your need, just copy and paste its useful pieces to your "~/.exwm-x" file.


Converted from exwm-x.el by [el2org](https://github.com/tumashu/el2org) .