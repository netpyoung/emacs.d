;; ** magit-blame coomand
;;    | RET | magit-show-commit                      |
;;    | SPC | magit-diff-show-or-scroll-up           |
;;    | DEL | magit-diff-show-or-scroll-down         |
;;    | b   | magit-blame-popup                      |
;;    | n   | magit-blame-next-chunk                 |
;;    | N   | magit-blame-next-chunk-same-commit     |
;;    | p   | magit-blame-previous-chunk             |
;;    | P   | magit-blame-previous-chunk-same-commit |
;;    | q   | magit-blame-quit                       |

;; ** git-timemachine command
;;    | p | Move to the previous revision                      |
;;    | n | Move to next revision                              |
;;    | w | Copy the hash of the current revision              |
;;    | W | Copy the full hash of the current revision         |
;;    | g | Move to specified revision                         |
;;    | t | Move to revision with selected commit message      |
;;    | q | Quit git-timemachine                               |
;;    | b | Run magit-blame on the currently visited revision. |

(use-package magit
  :pin melpa-stable
  :ensure t
  :bind
  ("C-c g b" . magit-blame-addition)
  ("C-c g m" . magit))

(use-package git-gutter
  ;;:diminish
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (custom-set-variables
   '(git-gutter:update-interval 2)
   '(git-gutter:modified-sign "*")
   '(git-gutter:added-sign "+")
   '(git-gutter:deleted-sign "-")
   '(git-gutter:hide-gutter nil)))

(when (display-graphic-p)
  (use-package git-gutter-fringe
    :ensure t))
