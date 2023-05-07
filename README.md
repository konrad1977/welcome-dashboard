# welcome-dashboard
A minimalistic dashboard for Emacs.

## How it looks:
!["Dashboard"](https://github.com/konrad1977/welcome-dashboard/blob/main/screenshots/screenshot_1.png)

## Configuration

```elisp
(use-package welcome
  :ensure nil
  :config
  (setq welcome-latitude 56.7365
        welcome-longitude 16.2981
        welcome-path-max-length 75
        welcome-min-left-padding 10
        welcome-image-file "~/.emacs.d/themes/true.png"
        welcome-image-width 200
        welcome-image-height 169
        welcome-title "Welcome Emacser. Have a great day!")
  (welcome-create-welcome-hook))
  ```
