# welcome-dashboard
An alternative dashboard for Emacs. Includes recent projects, recent files and an todos in your most recent project.

 ❤️ [Please sponsor me if you like this package](https://github.com/sponsors/konrad1977)

## How it looks:
!["Dashboard"](https://github.com/konrad1977/welcome-dashboard/blob/main/screenshots/screenshot_1.png)

## Configuration and installation

```elisp
(use-package welcome-dashboard
  :ensure nil ;; when using local file and not straight nor use-package
  :config
  (setq welcome-dashboard-latitude 56.7365
        welcome-dashboard-longitude 16.2981 ;; latitude and longitude must be set to show weather information
        welcome-dashboard-use-nerd-icons t ;; Use nerd icons instead of all-the-icons
        welcome-dashboard-path-max-length 75
        welcome-dashboard-use-fahrenheit nil ;; show in celcius or fahrenheit.
        welcome-dashboard-min-left-padding 10
        welcome-dashboard-image-file "~/path/yourimage.png"
        welcome-dashboard-image-width 200
        welcome-dashboard-max-number-of-todos 5
        welcome-dashboard-image-height 169
        welcome-dashboard-title (concat "Welcome " user-full-name))
  (welcome-dashboard-create-welcome-hook))
  ```
  
### Straight users
```elisp
  (use-package welcome-dashboard
    :straight (
    :host github
    :repo "konrad1977/welcome-dashboard"
    :files ("welcome-dashboard.el")
    :ensure t
    :after nerd-icons
    )
    :demand
    :init
    (setq welcome-dashboard-use-nerd-icons t
          welcome-dashboard-longitude 6.0440
          welcome-dashboard-latitude 53.0825
          welcome-dashboard-path-max-length 75
          welcome-dashboard-use-fahrenheit nil
          welcome-dashboard-min-left-padding 10
          welcome-dashboard-image-file "~/path/yourimage.png"
          welcome-dashboard-image-width 200
          welcome-dashboard-max-number-of-todos 5
          welcome-dashboard-image-height 169
          welcome-dashboard-title (concat "Welcome " user-full-name))
    :config
    (welcome-dashboard-create-welcome-hook))
  ```
  
  ## Shortcuts
  *M*-number opens project
  *C*-number opens recent files
  *C*-x number opens the todo
