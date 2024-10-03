# outli
Simple and stylish comment-based outlines with speed keys for Emacs.
<img width="1052" alt="image" src="https://github.com/user-attachments/assets/a6e35266-f283-4438-9fcb-c4b9a3d0a6f8">


`outli` is a simple Emacs outliner for code, documents, and more which provides hierarchical hide/show, styles your headings, and emulates org-mode navigation and structure editing.  It builds on the built-in `outline-minor-mode` and is simple by design, providing just a few key additional features beyond what outline already offers:

- Configurable heading syntax based on the concept of a `stem` (fixed first characters) and `repeat-char` (the number of which determines a heading's depth).  Example level-two headers include classics such as `;;;;` and `# **`, but anything's possible.
- Header style options including color-matched overline and blended background color for the initial heading info.
- `Tab` and `Shift-Tab` work just like you'd expect from org-mode to toggle headings or document visibility. 
- _Speed keys_ mirroring org-mode for easy navigation, visibility, and structure editing at headlines.  Hit `?` for the list of available keys.   Additions include `h` to hide sub-levels below the current level, and `1`-`5` to specify such a level directly. 
- Exposes headings to imenu.  A fast imenu browser like [consult-imenu](https://github.com/minad/consult) is recommended. 

# Configuration
Not yet in a package database; simply clone and point `use-package` at the correct path (or use [straight](https://github.com/radian-software/straight.el), etc.).

```elisp
(use-package outli
  :load-path "~/code/emacs/outli"
  ;:after lispy ; uncomment only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer
```

You should probably not enable `outli` in `org-mode` (which by default is prevented), or with other packages which operate on headings or use similar short bindings, like outshine or pdf-tools.  If you want to disable individual modes within some larger mode hierarchy (like `text-mode`), you can either opt-in by adding them one-by-one, or include those modes in `outli-heading-config` with a disable entry; see below.

# Customization
## Headline style
The main variable to customize is `outli-heading-config`, where you can set the _stem_ and _repeat char_, and influence the styling, including whether to style the stem and repeat chars the same, whether to include the overline, or whether to omit styling altogether.  Note that the `t` member of this alist is the default used for all modes whih are not explicitly specified.

Configured defaults are:

- **emacs-lisp-mode**: stem `;;`, repeat-char `;`
- **tex-mode**: `%%`, `%`
- **markdown-mode**: (empty stem), `#` 
- **org-mode**: disabled
- **all others**: `comment-start` + a space, `*`

The custom variable `outli-blend` controls whether a blended background is used to start the headline.   After configuring `outli-heading-config`, you should restart `outli-mode` in any open buffers using it.

## Style Defaults

You can set defaults for the `STYLE` and `NOBAR` properties for all modes, which take effect if these parameters are omitted or `nil` in the `outli-heading-config`; see `outli-default-style` and `outli-default-nobar`. 

## Speed keys
Customize `outli-speed-commands` to alter or disable speed keys, which work at the beginning of heading lines only (similar to org speed keys).

## Folding/unfolding automatically when hopping around buffers

Try `reveal-mode`; see below.

# FAQ
- **How does this relate to outline-minor-mode?**
 
  `outli` is mostly a convenient wrapper around functionality that is already built-in to outline, adding a few things like `narrow-to-subtree` and `insert-heading-respect-content` (ala org). And of course the speed-key bindings, automatic comments-as-header patterns, and styling.
- **How does this relate to outshine?**

  Mostly just conceptually.  Outshine also provides (different) speed keys, for example.  And a lot more, much of which isn't as relevant to modern emacs.  Since it builds more directly on the built-in capabilities of outline-minor-mode, `outli` is a _much_ smaller and simpler package. 

- **I wish `outli` styling would update when I change themes**: This should happen automatically in Emacs 29.1 and later.  For earlier version, add the following to your `use-package` stanza:

   ```elisp
      :config (advice-add 'load-theme :after #'outli-reset-all-faces) ```
   ```

- **What is the syntax of `outli-heading-config`?** 

  It's an _alist_, each entry of which looks like:

    `(MAJOR-MODE STEM REPEAT-CHAR STYLE NO-BAR)` to configure a mode.
  or
  
    `(MAJOR-MODE . nil)` to explicitly prevent `outli` from running in this mode.
	
   I recommend using the customize interface to configure `outli`: `M-x customize-group outli`.  But it may help to know:
    - `MAJOR-MODE`: A symbol for a major mode, or parent mode from which the current mode inherits, like `'text-mode` (note: omit the single apostrophe in the customize interface: it knows it's a symbol).  A value of `t` is used to specify the default.
    - `STEM`: A string like `"# "`.  The fixed "stem" of the headline pattern (omit quotes in customize interface).  Can also be an elisp expression which evaluates to a string.
    - `REPEAT-CHAR`: A _character_ like `?*`.  The repeating character which specifies the level of a headline (again: no `?` needed in customize, just type the character).  Can also be an elisp expression which evaluates to a character. 
    - `STYLE`: A style flag.  `nil` for default (maximum) styling, the symbol `none` for no special styling of headlines, and `t` for matched styling between stem and repeat char.  Can be omitted (defaults to `nil`).  See also `outli-default-style`. 
    - `NO-BAR`: A flag for the overline bar.  If non-`nil`, omit the overline.  Can be omitted (defaults to `nil`).  See also `outli-default-nobar`. 

# Tips
- You can use arbitrary expressions for the stem and repeat chars; they'll get evaluated at run-time.
- It's useful to target high-level modes like `prog-mode` or `text-mode`, from which many modes inherit (see [mode-minder](https://github.com/jdtsmith/mode-minder) to get a list of your major mode hierarchy).
- Try out the `h` key at headline start: it folds everything up to be no deeper than the current header's level. 
- To prevent `outli` from being enabled in a given mode (or family of derived modes), just include `(MODE . nil)` in `outli-heading-config`.  By default, `org-mode` is excluded in this way.
- Some emacs tools like `isearch` are smart about folding/unfolding text as you navigate through a buffer with them.  But not all.  To fix this, you can consider enabling `reveal-mode` in buffers where you use `outli`, then tools like `xref`, etc. will reveal folded targets, re-hiding them when you navigate away.
- I recommend `consult-heading` for quickly browsing outli headings.  If you use `consult-imenu`, you might like to separate headings, by adding this to the `consult-imenu` config:

   ```elisp
     (push '(?h "Headings")
        (plist-get (cdr (assoc 'emacs-lisp-mode consult-imenu-config)) :types))
   ```


# Related Packages
- `outline-minor-mode`: The built-in minor mode for outlining documents on which `outli` builds. 
- [orgmode](https://orgmode.org): The do-everything outliner mode.
- [outshine](https://github.com/alphapapa/outshine): A feature-full `outline-minor-mode` enhancement from which `outli` took its inspiration.  Has many legacy features. 
- [outorg](https://github.com/alphapapa/outorg): Required by `outshine`, this mode enables editing comment blocks in temporary `org-mode` buffers (the inverse of code-blocks in org).
- See [more related packages for org-like behavior outside of org](https://orgmode.org/worg/org-tutorials/org-outside-org.html). 
