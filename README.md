# welcome-dashboard
A minimalistic dashboard for Emacs.

## How it looks:
!["Dashboard"](https://github.com/konrad1977/welcome-dashboard/blob/main/screenshots/screenshot_1.png)

## Configuration

```elisp
(use-package welcome-dashboard
  :ensure nil ;; when using local file and not straight nor use-package
  :config
  (setq welcome-dashboard-latitude 56.7365
        welcome-dashboard-longitude 16.2981 ;; latitude and longitude must be set to show weather information
        welcome-dashboard-path-max-length 75
        welcome-dashboard-use-fahrenheit nil ;; show in celcius or fahrenheit.
        welcome-dashboard-min-left-padding 10
        welcome-dashboard-image-file "~/.emacs.d/themes/true.png"
        welcome-dashboard-image-width 200
        welcome-dashboard-image-height 169
        welcome-dashboard-title "Welcome Mikael. Have a great day!")
  (welcome-dashboard-create-welcome-hook))
  
  ```
