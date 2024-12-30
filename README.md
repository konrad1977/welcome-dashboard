# welcome-dashboard
An alternative dashboard for Emacs. Includes recent projects, recent files and an todos in your most recent project.

 ❤️ [Please sponsor me if you like this package](https://github.com/sponsors/konrad1977)

### How it looks:
!["Dashboard"](https://github.com/konrad1977/welcome-dashboard/blob/main/screenshots/screenshot_1.png)

## Welcome Dashboard Customization

Welcome Dashboard is an Emacs package that provides a customizable dashboard/start screen. Here's how to configure it using `use-package`:

## Available Customization Options

### Display Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `welcome-dashboard-title` | string | "Welcome" | Main dashboard title |
| `welcome-dashboard-use-nerd-icons` | boolean | nil | Enable Nerd Fonts icons |
| `welcome-dashboard-show-separator` | boolean | t | Display separator lines |
| `welcome-dashboard-separator-string` | string | "─" | String used for separators |
| `welcome-dashboard-shortcut-delimiter` | character | ?s | Character used for separation of item and shortcut |
| `welcome-dashboard-show-file-path` | boolean | t | Show complete file paths |

### Layout and Sizing

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `welcome-dashboard-min-left-padding` | integer | 10 | Minimum left padding |
| `welcome-dashboard-shortcut-spacing` | integer | 8 | Space between shortcuts and items |
| `welcome-dashboard-path-max-length` | integer | 80 | Maximum length for file paths |

### Section Titles and Limits

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `welcome-dashboard-projects-title` | string | "Recent projects: [M-number]" | Projects section title |
| `welcome-dashboard-files-title` | string | "Recent files: [C-number]" | Recent files section title |
| `welcome-dashboard-max-number-of-recent-files` | integer | 5 | Maximum recent files shown |
| `welcome-dashboard-max-number-of-todos` | integer | 4 | Maximum todos shown |
| `welcome-dashboard-max-number-of-projects` | integer | 3 | Maximum projects shown |

### Weather Integration

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `welcome-dashboard-latitude` | float | nil | Latitude for weather data |
| `welcome-dashboard-longitude` | float | nil | Longitude for weather data |
| `welcome-dashboard-use-fahrenheit` | boolean | nil | Use Fahrenheit instead of Celsius |

### Image Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `welcome-dashboard-image-file` | string | "" | Path to dashboard image |
| `welcome-dashboard-image-width` | integer | 200 | Image width in pixels |
| `welcome-dashboard-image-height` | integer | 200 | Image height in pixels |

## Example Configuration

Here's a complete example configuration:

```elisp
(use-package welcome-dashboard
  :ensure nil ;; when using local file and not straight nor use-package
  :config
  (setq welcome-dashboard-latitude 56.7365
        welcome-dashboard-longitude 16.2981     ;; latitude and longitude must be set to show weather information
        welcome-dashboard-use-nerd-icons t      ;; Use nerd icons instead of all-the-icons
        welcome-dashboard-path-max-length 75
        welcome-dashboard-show-file-path t      ;; Hide or show filepath
        welcome-dashboard-use-fahrenheit nil    ;; show in celcius or fahrenheit.
        welcome-dashboard-min-left-padding 10
        welcome-dashboard-image-file "~/path/yourimage.png"
        welcome-dashboard-image-width 200
        welcome-dashboard-image-height 169
        welcome-dashboard-max-number-of-todos 5
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
          welcome-dashboard-show-file-path t 
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
  
## Notes

- All settings can be customized through Emacs' customization interface (`M-x customize-group RET welcome-dashboard RET`)
- Weather functionality requires valid latitude and longitude coordinates
- Nerd icons require a Nerd Fonts compatible font to be installed
- Image display requires a valid path to an image file
  
## Shortcuts
M-number opens project
C-number opens recent files
C-x number opens todo with current number
