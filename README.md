# raylib.el

`raylib.el` is a [dynamic module](https://www.gnu.org/software/emacs/manual/html_node/elisp/Writing-Dynamic-Modules.html) for [raylib](https://www.raylib.com/)

> [!WARNING]
> The project is in a very early stage!
> Please don't expect anything to work reliably or at all just yet.

> [!CAUTION]
> <h4>It can crash your Emacs!</h4>

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

## Debugging
When an error occurs in the main loop managed by `rl-run-mainloop` it
is caught and reported as `raylib caught error: ...`. Main loop will
continue running and generating the same error, because we don't want
to interrupt your flow in case you just made a typo.

You may also want to stop the main loop and run your function
manually, potentially using `edebug` to step it through:

```elisp
(rl-stop-mainloop)
(my-mainloop rl-dt)
```
