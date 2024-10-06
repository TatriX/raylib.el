# raylib.el

`raylib.is` is a [dynamic module](https://www.gnu.org/software/emacs/manual/html_node/elisp/Writing-Dynamic-Modules.html) for [raylib](https://www.raylib.com/)

It is in a very early stage, so don't expect anything to work reliably
or at all just yet.

## Installation
Ensure you have `raylib` installed, for example in Arch you can do `pacman -S raylib`.
Clone the repo and run make, which should build `raylib.el.so` dynamic module.
```
cd ~/.emacs.d
git clone https://github.com/TatriX/raylib.el raylib
cd raylib
make

```

## Usage
```elisp
(use-package raylib
    :load-path ~/.emacs.d/raylib)
```

Now you can try running the [examples](./examples):

```elisp
(find-file "~/.emacs.d/raylib/examples/core/core-basic-window.el")
(eval-buffer)
```
