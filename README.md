# org-msr

Minimal Spaced Repetition setup for Org mode.

There’s a chance I might abandon this if I move to `org-drill`. For now, this is my setup.

## Usage

In an Org mode file, let each todo entry represent what you want to remember (like a vocabulary word). If it has a repeating schedule, you can review it in your org-agenda. Here, a repeating schedule is manually added.

```org
#+TODO: UNFAMILIAR/1w | DONE
* UNFAMILIAR/1w 宗教勧誘
SCHEDULED: <2019-05-06 Mon .+1w>
```

The idea is to use file-local custom TODO keywords to represent different lengths for the repeater. As we need to track when we have reviewed the idea, a single [“todo keyword set”](https://orgmode.org/manual/Multiple-sets-in-one-file.html#Multiple-sets-in-one-file) isn’t enough, so we put other keywords in different keyword sets.

```org
#+TODO: UNFAMILIAR/1w | DONE
#+TODO: MEMORIZED/3y | DONE
* UNFAMILIAR/1w 宗教勧誘
しゅうきょうかんゆう
cult (or religion) soliciting
* MEMORIZED/3y モニター
monitor
```

Because Org itself does not associate TODO keywords with frequencies, we do that with our own function, `org-msr-update-repeater`, which adds or updates the repeating schedule according to `org-msr-keyword-frequency-alist`.

Before running `org-msr-update-repeater`:
```org
#+TODO: UNFAMILIAR/1w | DONE
#+TODO: MEMORIZED/3y | DONE
* UNFAMILIAR/1w 宗教勧誘
しゅうきょうかんゆう
cult (or religion) soliciting
* MEMORIZED/3y モニター
monitor
```

After running `org-msr-update-repeater`:
```org
#+TODO: UNFAMILIAR/1w | DONE
#+TODO: MEMORIZED/3y | DONE
* UNFAMILIAR/1w 宗教勧誘
SCHEDULED: <2019-05-04 .+1w>
しゅうきょうかんゆう
cult (or religion) soliciting
* MEMORIZED/3y モニター
SCHEDULED: <2019-05-04 .+3y>
monitor
```

Now you can review an entry by running `org-todo` and selecting `DONE`. Although Org does not recommend sharing keywords between todo sets, sharing `DONE` seems to work fine. To change an entry’s review frequency, run `org-todo` and select another keyword. If `org-msr-mode` is enabled, that’s it; otherwise run `org-msr-update-repeater` again.

### org-msr-mode

`org-msr` is a minor mode that runs `org-msr-update-repeater` automatically after running `org-todo`. It also binds <kbd>C-c _</kbd> to `org-msr-update-repeater`.

### org-msr-setup

`org-msr-setup` does the above setup, as well as adding a file local variable to start `org-msr-mode` automatically. Essentially, it inserts this into the end of your file:

```org
* Org-msr Setup
#+TODO: NOMEMORY/5h | DONE(d)
#+TODO: DAILY/1d | DONE(d)
#+TODO: HARD/3d | DONE(d)
#+TODO: UNFAMILIAR/1w | DONE(d)
#+TODO: SOMEWHAT/2w | DONE(d)
#+TODO: FAMILIAR/1m | DONE(d)
#+TODO: EASY/6m | DONE(d)
#+TODO: CONFIDENT/1y | DONE(d)
#+TODO: MEMORIZED/3y | DONE(d)

# Local Variables:
# eval: (org-msr-mode 1)
# End:
```

The TODO lines come from `org-msr-keyword-frequency-alist`; the “Org-msr Setup” string is from `org-msr-setup-heading-name`.

### Combining with [`org-super-agenda`](https://github.com/alphapapa/org-super-agenda)

`org-msr` entries will probably bury everything else in your org-agenda. You can create a different agenda; personally I use [`org-super-agenda`](https://github.com/alphapapa/org-super-agenda) because that’s what I learned first.

To set it up:

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

## Install

### [`straight.el`](https://github.com/raxod502/straight.el)

```elisp
(straight-use-package '(org-msr :type git :host gitlab :repo "kisaragi-hiu/org-msr"))
```

### [Quelpa](https://framagit.org/steckerhalter/quelpa)

```elisp
(quelpa '(org-msr :fetcher gitlab :repo "kisaragi-hiu/org-msr"))
```
