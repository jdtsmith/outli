# outli
Simple and stylish comment-based outlines for Emacs.
![image](https://user-images.githubusercontent.com/93749/190755666-69ca250c-476b-41c9-a26a-f9c12d167c99.png)

`outli` is a simple Emacs outliner for code, documents, and more which styles your headings, and emulates org-mode navigation and structure editing.  It is simple by design, and provides just a few key features:

- Configurable heading syntax based on the concept of a `stem` (fixed first characters) and `repeat-char` (the number of which determines a heading's depth).  Example level two headers include classics such as `;;;;` or `# **`, but anything's possible.
- Header style options including color-matched overline and blended background color for the initial heading info.
- `Tab` and `Shift-Tab` work just like you'd expect from org-mode to toggle headings or document visibility. 
- _Speed keys_ mirroring thos of org-mode for easy navigation, visibility, and structure editing on the beginning of a headline.  Hit `?` for the list of available keys.   Additions include `h` to hide sublevels below this current, and `1`-`5` to specify such a level directly. 

# Configuration

Not yet in a package database; simply clone and point `use-package` at the correct path (or use [straight](https://github.com/radian-software/straight.el), etc.).

```elisp
(use-package outli
  :load-path "~/code/emacs/outli"
  :after lispy ; only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer
```

You should probably not enable `outli` in org-mode, or with outher packages which operate on headings like `outshine`. 

# Related Packages

- `outline-mode`: The built in mode for outlining documents on which `outli` builds. 
- [`orgmode`](https://orgmode.org): The do-everything outliner mode.
- [`outshine`](https://github.com/alphapapa/outshine): A featureful `outline-mode` enhancement from which `outli` took its inspiration.  Has many legacy features. 
- [`outorg`](https://github.com/alphapapa/outorg): Required by `outshine`, this mode enabled editing comment blocks in temporary org-mode buffers (the inverse of code-blocks in org).
- See [more related packages for org-like behavior outside of org](https://orgmode.org/worg/org-tutorials/org-outside-org.html). 
