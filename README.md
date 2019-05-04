# org-msr

Minimal Spaced Repetition setup for Org mode.

## Usage
Run `org-msr-setup` in a Org mode file to
- add file TODO keywords according to `org-msr-keyword-frequency-alist`
- start `org-msr-mode` automatically when visiting it.

Now for each heading you want to remember, run `org-todo` and select how often you want to see it. A repeater will automatically be added and updated, and you can see all items in your org-agenda.

I recommend using [`org-super-agenda`](https://github.com/alphapapa/org-super-agenda) and a filetag so that the `org-msr` entries don’t bury everything else.

In your init:
```emacs-lisp
(setq org-super-agenda-groups '((:name "Vocabulary"
                                       :tag "org-msr"
                                       :order 100)))
```

In the Org file:
```org
#+FILETAGS: :org-msr:
```
