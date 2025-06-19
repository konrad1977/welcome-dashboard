;;; welcome-dashboard.el --- Simple welcome-dashboard screen -*- lexical-binding: t -*-

;; Welcome-dashboard screen

;; Author: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Maintainer: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Keywords: dashboard
;; Created: 2023
;; Version: 0.3
;; Package-Requires: ((emacs "27.1") (all-the-icons "5.0.0") (async "1.9.7") (nerd-icons "0.0.1"))
;; Homepage: https://github.com/konrad1977/welcome-dashboard

;;; Commentary:

;;; A dashboard for Emacs.

;;; Code:

(require 'json)
(require 'recentf)
(require 'url)

;; Defer loading heavier dependencies until needed
(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function nerd-icons-icon-for-dir "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function nerd-icons-wicon "nerd-icons")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-icon-for-weather "all-the-icons")
(declare-function all-the-icons-octicon "all-the-icons")
(declare-function all-the-icons-octicon-family "all-the-icons")

(defvar welcome-dashboard-mode nil)
(defvar welcome-dashboard-recentfiles '()
  "Recent list.")

(defvar welcome-dashboard-recent-projects '()
  "List of recent projects.")

(defvar welcome-dashboard-todos '()
  "Todos.")

(defconst welcome-dashboard-buffer "*welcome*"
  "Welcome-dashboard buffer name.")

(defvar welcome-dashboard--file-icon-cache (make-hash-table :test 'equal)
  "Cache for file icons.")

(defvar welcome-dashboard--formatted-file-cache (make-hash-table :test 'equal)
  "Cache for formatted file entries.")

(defvar welcome-dashboard--formatted-project-cache (make-hash-table :test 'equal)
  "Cache for formatted project entries.")

(defvar welcome-dashboard--padding-cache nil
  "Cache for padding.")

(defvar welcome-dashboard--last-window-width nil
  "Last window width.")

(defvar welcome-dashboard-last-project-name nil
  "Last project name.")

(defvar welcome-dashboard-temperature nil
  "Temperature.")

(defvar welcome-dashboard-weatherdescription nil
  "Weather description.")

(defvar welcome-dashboard-weathericon nil
  "Weather icon.")

(defvar welcome-dashboard--max-length 0
  "Store the max length of the recent files.")

(defcustom welcome-dashboard-use-nerd-icons nil
  "Use nerd icons."
  :group 'welcome-dashboard
  :type '(boolean))

(defcustom welcome-dashboard-projects-title "Recent projects: [M-number]"
  "Welcome-dashboard title."
  :group 'welcome-dashboard
  :type '(string))

(defcustom welcome-dashboard-files-title "Recent files: [C-number]"
  "Welcome-dashboard recent files title."
  :group 'welcome-dashboard
  :type '(string))

(defcustom welcome-dashboard-separator-string "· · ─────── ·· ─────── · ·"
  "Separator character."
  :group 'welcome-dashboard
  :type '(string))

(defcustom welcome-dashboard-shortcut-delimiter ?\s
  "Character to use for padding (space, dot, underscore etc)."
  :type 'character
  :group 'welcome-dashboard)

(defcustom welcome-dashboard-title "Welcome"
  "Welcome-dashboard title."
  :group 'welcome-dashboard
  :type '(string))

(defcustom welcome-dashboard-show-separator t
  "Show separator in welcome-dashboard."
  :group 'welcome-dashboard
  :type '(boolean))

(defcustom welcome-dashboard-use-fahrenheit nil
  "Show weather temperature in Fahrenheit."
  :group 'welcome-dashboard
  :type '(boolean))

(defcustom welcome-dashboard-show-file-path t
  "Show file path in welcome-dashboard."
  :group 'welcome-dashboard
  :type '(boolean))

(defcustom welcome-dashboard-min-left-padding 10
  "Minimum left padding when resizing window."
  :group 'welcome-dashboard
  :type '(natnum))

(defcustom welcome-dashboard-shortcut-spacing 8
  "Spacing between shortcuts and file."
  :group 'welcome-dashboard
  :type '(natnum))

(defcustom welcome-dashboard-path-max-length 80
  "Latitude for weather information."
  :group 'welcome-dashboard
  :type '(natnum))

(defcustom welcome-dashboard-max-number-of-recent-files 5
  "Maximum number of recent files to show."
  :group 'welcome-dashboard
  :type '(natnum))

(defcustom welcome-dashboard-max-number-of-todos 4
  "Maximum number of todos to show."
  :group 'welcome-dashboard
  :type '(natnum))

(defcustom welcome-dashboard-max-number-of-projects 3
  "Maximum number of projects to show."
 :group 'welcome-dashboard
  :type '(natnum))

(defcustom welcome-dashboard-latitude nil
  "Latitude for weather information."
  :group 'welcome-dashboard
  :type '(float))

(defcustom welcome-dashboard-longitude nil
  "Longitude for weather information in welcome-dashboard package."
  :group 'welcome-dashboard
  :type '(float))

(defcustom welcome-dashboard-image-file ""
  "Image file in welcome-dashboard package."
  :group 'welcome-dashboard
  :type '(file))

(defcustom welcome-dashboard-image-width 200
  "Image width for weather information."
  :group 'welcome-dashboard
  :type '(natnum))

(defcustom welcome-dashboard-image-height 200
  "Image width for weather information."
  :group 'welcome-dashboard
  :type '(natnum))

(defgroup welcome-dashboard nil
  "Welcome-dashboard group."
  :group 'applications)

(defface welcome-dashboard-title-face
  '((t :inherit font-lock-function-name-face :height 1.2 :weight bold))
  "Title face."
  :group 'welcome-dashboard)

(defface welcome-dashboard-subtitle-face
  '((t :inherit font-lock-comment-face))
  "Subtitle face."
  :group 'welcome-dashboard)

(defface welcome-dashboard-separator-face
  '((t :inherit font-lock-comment-face))
  "Subtitle face."
  :group 'welcome-dashboard)

(defface welcome-dashboard-info-face
  '((t :inherit success :height 0.9 :bold t :italic t))
  "Face added to code-usage display."
  :group 'welcome-dashboard)

(defface welcome-dashboard-text-info-face
  '((t :inherit default :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'welcome-dashboard)

(defface welcome-dashboard-path-face
  '((t :inherit font-lock-comment-face :weight normal))
  "Face for the file path."
  :group 'welcome-dashboard)

(defface welcome-dashboard-filename-face
  '((t :inherit default :weight semi-bold))
  "Face for the file name."
  :group 'welcome-dashboard)

(defface welcome-dashboard-time-face
  '((t :inherit mode-line-buffer-id :height 0.9 :weight thin))
  "Face for time."
  :group 'welcome-dashboard)

(defface welcome-dashboard-weather-description-face
  '((t :foreground "#f9e2af" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for weather description."
  :group 'welcome-dashboard)

(defface welcome-dashboard-startup-time-face
  '((t :inherit warning :height 0.9 :weight thin :bold nil :italic nil))
  "Face for startup time."
  :group 'welcome-dashboard)

(defface welcome-dashboard-recent-project-shortcut-face
  '((t :inherit mode-line-buffer-id :weight normal))
  "Face for recent files shortcuts."
  :group 'welcome-dashboard)

(defface welcome-dashboard-recent-file-shortcut-face
  '((t :inherit font-lock-constant-face :weight normal))
  "Face for recent files shortcuts."
  :group 'welcome-dashboard)

(defface welcome-dashboard-shortcut-todo-face
  '((t :inherit font-lock-constant-face :weight normal))
  "Face for todo shortcuts."
  :group 'welcome-dashboard)

(defface welcome-dashboard-todo-type-face
  '((t :foreground "#585b70" :weight normal))
  "Face for todo shortcuts."
  :group 'welcome-dashboard)

(defface welcome-dashboard-weather-icon-face
  '((t :height 0.9))
  "Face for weather icon."
  :group 'welcome-dashboard)

(defface welcome-dashboard-weather-temperature-face
  '((t :foreground "#f38ba8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for temperature."
  :group 'welcome-dashboard)

(defface welcome-dashboard-shortcut-delimiter-face
  '((t :inherit font-lock-comment-face))
  "Face for temperature."
  :group 'welcome-dashboard)

(defface welcome-dashboard-project-face
  '((t :inherit font-lock-doc-face :weight semi-bold))
  "Face for project name."
  :group 'welcome-dashboard)


(defvar welcome-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'welcome-dashboard--open-recent-file)
    (define-key map (kbd "<return>") 'welcome-dashboard--open-recent-file)
    (define-key map (kbd "o") 'welcome-dashboard--open-recent-file)

    (dolist (i (number-sequence 1 welcome-dashboard-max-number-of-projects))
      (define-key map (kbd (concat "M-" (number-to-string i)))
                  `(lambda ()
                     (interactive)
                     (welcome-dashboard--open-project-at-index ,i))))

    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (concat "C-" (number-to-string i)))
                  `(lambda ()
                     (interactive)
                     (welcome-dashboard--open-recent-file-at-index ,i)))

      (define-key map (kbd (concat "C-x " (number-to-string i)))
                           `(lambda ()
                              (interactive)
                              (welcome-dashboard--open-todos-at-index ,i))))

    map)
  "Keymap for `welcome-dashboard-mode'.")

(define-derived-mode welcome-dashboard-mode fundamental-mode "Welcome-dashboard"
  "Major mode for the welcome-dashboard screen."
  :group 'welcome-dashboard
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local global-hl-line-mode nil)
  (setq-local buffer-read-only t)
  (use-local-map welcome-dashboard-mode-map))


(defun welcome-dashboard--ensure-recentf ()
  "Ensure recentf is initialized and loaded."
  (unless recentf-mode
    (recentf-mode 1))
  (unless (bound-and-true-p recentf-list)
    (setq recentf-list nil))
  (when (and (boundp 'recentf-list)
             (not recentf-list))
    (recentf-load-list)))


(defun welcome-dashboard--get-recent-projects ()
  "Get list of recent projects."
  (welcome-dashboard--ensure-recentf)
  (let ((projects '()))
    (dolist (file welcome-dashboard-recentfiles)
      (when-let* ((root (vc-find-root file ".git")))
        (unless (member root projects)
          (push root projects))))
    (seq-take (nreverse projects) welcome-dashboard-max-number-of-projects)))


(defun welcome-dashboard--open-project-at-index (index)
  "Open the project at INDEX."
  (interactive "nIndex: ")
  (let ((projects welcome-dashboard-recent-projects))
    (when (<= 1 index (length projects))
      (let ((project (nth (1- index) projects)))
        (find-file project)))))


(defun welcome-dashboard--weather-icon-from-code (code)
  "Maps a weather (as CODE) to a corresponding string."
  (if welcome-dashboard-use-nerd-icons
      (progn
        (require 'nerd-icons)
        (nerd-icons-wicon
         (pcase code
           (`0 "nf-weather-day_sunny")
           ((or `1 `2 `3) "nf-weather-cloudy")
           ((or `45 `48) "nf-weather-fog")
           ((or `51 `53 `55) "nf-weather-sleet")
           ((or `56 `57) "nf-weather-snow")
           ((or `61 `63 `65) "nf-weather-day_rain_mix")
           ((or `66 `67) "nf-weather-rain-mix")
           ((or `71 `73 `75) "nf-weather-snow")
           (`77 "nf-weather-snow")
           ((or `80 `81 `82) "nf-weather-rain")
           ((or `85 `86) "nf-weather-rain-mix")
           ((or `95 `96 `99) "nf-weather-thunderstorm")
           (_ "Unknown"))))
    (progn
      (require 'all-the-icons)
      (all-the-icons-icon-for-weather
       (pcase code
         (`0 "wi-day-sunny")
         ((or `1 `2 `3) "wi-day-cloudy")
         ((or `45 `48) "wi-day-fog")
         ((or `51 `53 `55) "sleet")
         ((or `56 `57) "wi-snow")
         ((or `61 `63 `65) "wi-day-rain")
         ((or `66 `67) "wi-day-rain-mix")
         ((or `71 `73 `75) "wi-snow")
         (`77 "wi-snow")
         ((or `80 `81 `82) "wi-rain")
         ((or `85 `86) "wi-rain-mix")
         ((or `95 `96 `99) "wi-thunderstorm")
         (_ "Unknown"))))))


(defun welcome-dashboard--weather-code-to-string (code)
  "Maps a weather (as CODE) to a corresponding string."
  (pcase code
    (`0 "Clear sky")
    ((or `1 `2 `3) "Partly cloudy")
    ((or `45 `48) "Fog")
    ((or `51 `53 `55) "Drizzle")
    ((or `56 `57) "Freezing drizzle")
    ((or `61 `63 `65) "Rain")
    ((or `66 `67) "Freezing rain")
    ((or `71 `73 `75) "Snowfall")
    (`77 "Snow grains")
    ((or `80 `81 `82) "Rain showers")
    ((or `85 `86) "Snow showers")
    ((or `95 `96 `99) "Thunderstorm")
    (_ "Unknown")))


(defun welcome-dashboard--insert-centered (text)
  "Insert TEXT at the center of the current line."
  (let ((width (window-width)))
    (insert (make-string (/ (- width (length text)) 2) ?\ ))
    (insert text)
    (insert "\n")))


(defun welcome-dashboard--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (prop-pos (next-single-property-change line-start 'path nil line-end)))
    (when prop-pos
      (let ((file (get-text-property prop-pos 'path)))
        (if (file-exists-p file)
            (find-file file)
          (error "File %s does not exist" file))))))


(defun welcome-dashboard--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files welcome-dashboard-recentfiles))
    (when (<= 1 index (length files))
      (find-file (nth (1- index) files)))))


(defun welcome-dashboard--open-todos-at-index (index)
  "Open the todo at INDEX."
  (interactive "nIndex: ")
  (let ((todos welcome-dashboard-todos))
    (when (<= 1 index (length todos))
      (let* ((todo (nth (1- index) todos))
             (file (nth 0 todo))
             (line (string-to-number (nth 1 todo)))
             (column (string-to-number (nth 2 todo))))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char (1- column))))))


(defun welcome-dashboard--truncate-path-in-middle (path n)
  "Truncate the middle of PATH to length N by removing characters.
And adding an ellipsis."
  (if (<= (length path) n)
      path
    (let* ((left (/ (- n 3) 2))
           (right (- n left 3))
           (head (substring path 0 (+ left 1)))
           (tail (substring path (- (length path) right)))
           (ellipsis "..."))
      (concat head ellipsis tail))))


(defun welcome-dashboard--file-icon (file)
  "Get the icon for (FILE)."
  (or (gethash file welcome-dashboard--file-icon-cache)
      (puthash file
               (if welcome-dashboard-use-nerd-icons
                   (progn
                     (require 'nerd-icons)
                     (propertize (cond ((not (file-exists-p file)) (nerd-icons-mdicon "nf-md-alert_remove" :face '(:inherit nerd-icons-orange)))
                                      ((file-directory-p file) (nerd-icons-icon-for-dir file))
                                      (t (nerd-icons-icon-for-file file)))))
                 (progn
                   (require 'all-the-icons)
                   (propertize (all-the-icons-icon-for-file file :v-adjust -0.05) 'face '(:family "all-the-icons" :height 1.0))))
               welcome-dashboard--file-icon-cache)))

(defun welcome-dashboard--format-file (file)
  "Format a FILE entry for the dashboard, with `path` text property."
  (or (gethash file welcome-dashboard--formatted-file-cache)
      (puthash file
               (let* ((file-name (file-name-nondirectory file))
                      (file-dir (file-name-directory file))
                      (entry-text
                       (if welcome-dashboard-show-file-path
                           (format "%s %s%s"
                                   (welcome-dashboard--file-icon file)
                                   (propertize (welcome-dashboard--truncate-path-in-middle
                                                file-dir
                                                welcome-dashboard-path-max-length)
                                               'face 'welcome-dashboard-path-face)
                                   (propertize file-name
                                               'face 'welcome-dashboard-filename-face))
                         (format "%s %s"
                                 (welcome-dashboard--file-icon file)
                                 (propertize file-name
                                             'face 'welcome-dashboard-filename-face)))))
                 (propertize entry-text 'path file))
               welcome-dashboard--formatted-file-cache)))

(defun welcome-dashboard--format-project (project)
  "Format a PROJECT entry for the dashboard."
  (or (gethash project welcome-dashboard--formatted-project-cache)
      (puthash project
               (let* ((project-name (file-name-nondirectory (directory-file-name project)))
                      (project-line (format "%s %s"
                                           (if welcome-dashboard-use-nerd-icons
                                               (progn
                                                 (require 'nerd-icons)
                                                 (nerd-icons-octicon "nf-oct-repo_forked"))
                                             (progn
                                               (require 'all-the-icons)
                                               (all-the-icons-octicon "nf-oct-repo_forked")))
                                           (propertize project-name 'face 'welcome-dashboard-project-face))))
                 project-line)
               welcome-dashboard--formatted-project-cache)))

(defun welcome-dashboard--insert-recent-projects-and-shortcuts ()
  "Insert the recent projects in the welcome-dashboard buffer."
  (unless welcome-dashboard-recent-projects
    (setq welcome-dashboard-recent-projects (welcome-dashboard--get-recent-projects)))

  (welcome-dashboard--update-max-length)

  (welcome-dashboard--insert-two-columns
   :left-items welcome-dashboard-recent-projects
   :left-title (welcome-dashboard--create-title "Recent" "projects:")
   :left-formatter #'welcome-dashboard--format-project
   :shortcut-modifier "M-"
   :shortcut-face 'welcome-dashboard-recent-project-shortcut-face))
        

(defun welcome-dashboard--insert-recent-files-and-shortcuts ()
  "Insert recent files with icons and shortcuts in the welcome-dashboard buffer."
  (recentf-mode)
  (unless welcome-dashboard-recentfiles
    (setq welcome-dashboard-recentfiles (seq-take recentf-list welcome-dashboard-max-number-of-recent-files)))

  (welcome-dashboard--update-max-length)
  
  (welcome-dashboard--insert-two-columns
   :left-items welcome-dashboard-recentfiles
   :left-title (welcome-dashboard--create-title "Recent" "files:")
   :left-formatter #'welcome-dashboard--format-file
   :shortcut-modifier "C-"
   :shortcut-face 'welcome-dashboard-recent-file-shortcut-face))


(defun welcome-dashboard--insert-todo-info ()
  "Insert misc info in the welcome-dashboard buffer."
  (when (and (> (length welcome-dashboard-todos) 0) (welcome-dashboard--isActive))
    (welcome-dashboard--update-max-length)
    (welcome-dashboard--insert-separator)
    (welcome-dashboard--insert-two-columns
     :left-items (welcome-dashboard--todo-items)
     :left-title (welcome-dashboard--todo-title)
     :left-formatter #'identity
     :shortcut-modifier "C-x"
     :shortcut-face 'welcome-dashboard-shortcut-todo-face)))


(defun welcome-dashboard--insert-misc-info ()
  "Insert misc info in the welcome-dashboard buffer."
  (let ((misc-list (list (welcome-dashboard--weather-info)
                         (welcome-dashboard--package-info)
                         (welcome-dashboard--startup-time))))

        (welcome-dashboard--insert-two-columns
         :left-items misc-list
         :left-title nil
         :left-formatter #'identity
         :shortcut-modifier "  "
         :shortcut-face 'welcome-dashboard-recent-file-shortcut-face)))


(defun welcome-dashboard--todo-icon ()
  "Todo icon."
  (if welcome-dashboard-use-nerd-icons
      (progn
        (require 'nerd-icons)
        (propertize (nerd-icons-octicon "nf-oct-alert")
                    'display '(raise 0)))
    (progn
      (require 'all-the-icons)
      (propertize (all-the-icons-octicon "alert")
                  'face `(:height 1.0)
                  'display '(raise 0)))))


(defun welcome-dashboard--create-title (first second)
  "Create a title with FIRST and SECOND."
  (format "%s %s" (propertize first 'face 'welcome-dashboard-subtitle-face)
          (propertize second 'face 'welcome-dashboard-project-face)))


(defun welcome-dashboard--todo-title ()
  "Insert todo section title."
  (when (> (length welcome-dashboard-todos) 0)
    (format "%s %s!"
            (propertize "You got work todo in" 'face 'welcome-dashboard-subtitle-face)
            (propertize welcome-dashboard-last-project-name 'face 'welcome-dashboard-project-face))))

(defun welcome-dashboard--todo-items ()
  "Insert individual todo items."
  (let ((todo-list '()))
    (dolist (todo welcome-dashboard-todos)
      (let* ((path (nth 0 todo))
             (type (nth 3 todo))
             (text (nth 4 todo))
             (title (format "%s %s %s"
                           (welcome-dashboard--todo-icon)
                           (propertize type 'face 'welcome-dashboard-todo-type-face)
                           (propertize (string-trim-left (welcome-dashboard--truncate-text-right text)) 'face 'welcome-dashboard-filename-face))))
        (push (propertize title 'path path) todo-list)))
    (nreverse todo-list)))


(defun welcome-dashboard--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer."
  (when (equal (buffer-name) welcome-dashboard-buffer)
    (welcome-dashboard--refresh-screen)))

(defun welcome-dashboard--fetch-weather-data (&optional initial)
  "Fetch weather data for the welcome dashboard.
If INITIAL is non-nil, perform an initial fetch."
  ;; Only fetch if we have coordinates
  (when (welcome-dashboard--show-weather-info)
    (let ((url-request-method "GET")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true"
                      welcome-dashboard-latitude welcome-dashboard-longitude)))
      (url-retrieve url
                    (lambda (_)
                      (goto-char (point-min))
                      (re-search-forward "^$")
                      (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
                             (json-obj (json-read-from-string json-data))
                             (current-weather (cdr (assoc 'current_weather json-obj)))
                             (temp (cdr (assoc 'temperature current-weather)))
                             (weather-code (cdr (assoc 'weathercode current-weather)))
                             (weather-icon (welcome-dashboard--weather-icon-from-code weather-code)))
                        (setq welcome-dashboard-weathericon weather-icon)
                        (if welcome-dashboard-use-fahrenheit
                            (setq welcome-dashboard-temperature (format "%.1f" (+ (* temp 1.8) 32)))
                          (setq welcome-dashboard-temperature (format "%.1f" temp)))
                        (setq welcome-dashboard-weatherdescription
                              (format "%s" (welcome-dashboard--weather-code-to-string weather-code)))
                        ;; Only set up the recurring timer after initial fetch
                        (when initial
                          (run-with-timer 900 900 #'welcome-dashboard--fetch-weather-data))
                        (when (welcome-dashboard--isActive)
                          (welcome-dashboard--refresh-screen))))
                    nil
                    t))))


(defun welcome-dashboard-create-welcome-hook ()
  "Setup welcome-dashboard screen."
  (when (< (length command-line-args) 2)
    (remove-hook 'switch-to-buffer #'welcome-dashboard--redisplay-buffer-on-resize)
    (add-hook 'window-configuration-change-hook #'welcome-dashboard--redisplay-buffer-on-resize)
    (add-hook 'emacs-startup-hook (lambda ()
                                   ;; Show dashboard immediately
                                   (welcome-dashboard--refresh-screen)
                                   ;; Defer loading of additional data
                                   (run-with-idle-timer 1.0 nil #'welcome-dashboard--fetch-todos t)
                                   (run-with-idle-timer 2.0 nil #'welcome-dashboard--fetch-weather-data t)))))

(defun welcome-dashboard--truncate-text-right (text)
  "Truncate TEXT at the right to a maximum of 100 characters."
  (if (> (length text) welcome-dashboard-path-max-length)
      (concat (substring text 0 (- welcome-dashboard-path-max-length 3)) "...")
    text))

(defun welcome-dashboard--clock-icon ()
  "Get the clock icon."
  (if welcome-dashboard-use-nerd-icons
      (progn
        (require 'nerd-icons)
        (propertize (nerd-icons-octicon "nf-oct-clock")
                    'display '(raise 0)))
    (progn
      (require 'all-the-icons)
      (propertize (all-the-icons-octicon "clock")
                  'face `(:family ,(all-the-icons-octicon-family) :height 1.0)
                  'display '(raise 0)))))


(defun welcome-dashboard--package-info ()
  "Insert package info."
  (let ((packages (format "%d" (welcome-dashboard--package-length))))
    (format "%s %s %s"
            (welcome-dashboard--package-icon)
            (propertize packages 'face 'welcome-dashboard-info-face 'display '(raise -0.1))
            (propertize "packages loaded" 'face 'welcome-dashboard-text-info-face 'display '(raise -0.1)))))


(defun welcome-dashboard--startup-time()
  "Startup time information."
  (format "%s %s %s %s"
          (welcome-dashboard--clock-icon)
          (propertize "Startup time:" 'face 'welcome-dashboard-text-info-face)
          (propertize (emacs-init-time "%.2f") 'face 'welcome-dashboard-startup-time-face)
          (propertize "seconds" 'face 'welcome-dashboard-text-info-face)))


(defun welcome-dashboard--package-icon ()
  "Get the package icon."
  (if welcome-dashboard-use-nerd-icons
      (progn
        (require 'nerd-icons)
        (propertize (nerd-icons-codicon "nf-cod-package")
                    'display '(raise -0.1)))
    (progn
      (require 'all-the-icons)
      (propertize (all-the-icons-octicon "package")
                  'face `(:family ,(all-the-icons-octicon-family) :height 1.0)
                  'display '(raise -0.1)))))


(defun welcome-dashboard--temperature-symbol ()
  "Get the correct type of temperature symbol."
  (if welcome-dashboard-use-fahrenheit
      "℉"
    "℃"))


(defun welcome-dashboard--show-weather-info ()
  "Check if we latitude and longitude and then show weather info."
  (let ((latitude welcome-dashboard-latitude)
        (longitude welcome-dashboard-longitude))
    (if (and (floatp latitude) (floatp longitude) (> latitude 0.0) (> longitude 0.0))
        t
      nil)))


(defun welcome-dashboard--weather-info ()
  "Insert weather info."
  (when (welcome-dashboard--show-weather-info)
    (if welcome-dashboard-weatherdescription
        (format "%s %s, %s%s"
                (propertize welcome-dashboard-weathericon 'face '(:family "Weather icons" :height 1.0) 'display '(raise 0))
                (propertize welcome-dashboard-weatherdescription 'face 'welcome-dashboard-weather-description-face)
                (propertize welcome-dashboard-temperature 'face 'welcome-dashboard-weather-temperature-face)
                (propertize (welcome-dashboard--temperature-symbol) 'face 'welcome-dashboard-text-info-face))
      (propertize "Loading weather data..." 'face 'welcome-dashboard-weather-temperature-face))))


(defun welcome-dashboard--parse-todo-result (result)
  "Parse the RESULT and create a list of todo items."
  (let ((regex "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):\s?\\W+\\(.*\\):\\(.*\\)$"))
    (save-match-data
      (let (matches todos)
        (while (string-match regex result)
          (setq matches (list (match-string 1 result)
                              (match-string 2 result)
                              (match-string 3 result)
                              (match-string 4 result)
                              (match-string 5 result)))
          (setq result (substring result (match-end 0)))
          (push matches todos))
        (nreverse todos)))))


(cl-defun welcome-dashboard--async-command-to-string (&key command &key callback)
  "Async shell command to JSON run async (as COMMAND)
and parse it json and call (as CALLBACK)."
  (require 'async)
  (async-start
   `(lambda ()
      (shell-command-to-string ,command))
   `(lambda (result)
      (funcall ,callback result))))


(defun welcome-dashboard--last-root ()
  "Get the version control root directory of the most recent file."
  (when (> (length welcome-dashboard-recentfiles) 0)
    (require 'vc)
    (let ((file (car welcome-dashboard-recentfiles)))
      (vc-find-root file ".git"))))


(defun welcome-dashboard--fetch-todos (&optional initial)
  "Fetch todos.  When INITIAL is t, set up recurring updates."
  (when (> welcome-dashboard-max-number-of-todos 0)
    (when (and (executable-find "rg") (welcome-dashboard--last-root))
      (require 'async)
      (require 'vc)
      (let* ((root (welcome-dashboard--last-root))
             (projectname (file-name-nondirectory (directory-file-name root)))
             (command (format "rg -e \"(TODO|FIX|FIXME|PERF|HACK|NOTE):\s+\" --color=never --no-heading --with-filename --line-number --column --sort path %s" root))
             (is-initial initial))  ; Capture the initial value in closure
        (setq welcome-dashboard-last-project-name projectname)
        (welcome-dashboard--async-command-to-string
         :command command
         :callback `(lambda (result)
                     (setq welcome-dashboard-todos
                           (seq-take (welcome-dashboard--parse-todo-result result)
                                   welcome-dashboard-max-number-of-todos))
                     ;; Use the captured is-initial value
                     (when ,is-initial
                       (run-with-timer 300 300 #'welcome-dashboard--fetch-todos))
                     (when (welcome-dashboard--isActive)
                       (welcome-dashboard--refresh-screen))))))))


(defun welcome-dashboard--package-length ()
  "Get the number of installed packages."
  (cond
    ((bound-and-true-p package-alist)
     (length package-activated-list))
    ((boundp 'straight--profile-cache)
     (hash-table-count straight--profile-cache))
    ((boundp 'elpaca--queued)
     (length elpaca--queued))
    (t 0)))


(defun welcome-dashboard--isActive ()
  "Check if buffer is active and visible."
  (or (eq welcome-dashboard-buffer (window-buffer (selected-window)))
      (get-buffer-window welcome-dashboard-buffer 'visible)))


(defun welcome-dashboard--refresh-screen ()
  "Show the welcome-dashboard screen."
  (with-current-buffer (get-buffer-create welcome-dashboard-buffer)
    (let* ((buffer-read-only)
           (left-margin welcome-dashboard-min-left-padding))
      
      ;; Only create image if file exists and is not empty
      (when (and (not (string-empty-p welcome-dashboard-image-file))
                 (file-exists-p welcome-dashboard-image-file))
        (let* ((image (create-image welcome-dashboard-image-file 'png nil 
                                   :width welcome-dashboard-image-width 
                                   :height welcome-dashboard-image-height))
               (size (image-size image))
               (width (car size)))
          (setq left-margin (max welcome-dashboard-min-left-padding 
                                (floor (/ (- (window-width) width) 2))))))
      
      (erase-buffer)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (when (not (welcome-dashboard--isActive))
          (switch-to-buffer welcome-dashboard-buffer))
        (welcome-dashboard--insert-centered (propertize welcome-dashboard-title 'face 'welcome-dashboard-title-face))
        (welcome-dashboard--insert-recent-projects-and-shortcuts)
        (welcome-dashboard--insert-separator)
        (welcome-dashboard--insert-recent-files-and-shortcuts)
        (welcome-dashboard--insert-todo-info)
        (welcome-dashboard--insert-separator)
        (welcome-dashboard--insert-misc-info)
        (insert "\n")
        (welcome-dashboard--insert-centered (propertize (format-time-string "%A, %B %d %R") 'face 'welcome-dashboard-time-face))
        (insert "\n\n")
        
        ;; Only insert image if file exists and is not empty
        (when (and (not (string-empty-p welcome-dashboard-image-file))
                   (file-exists-p welcome-dashboard-image-file))
          (insert (make-string left-margin ?\ ))
          (insert-image (create-image welcome-dashboard-image-file 'png nil 
                                     :width welcome-dashboard-image-width 
                                     :height welcome-dashboard-image-height)))

        (welcome-dashboard-mode)
        (goto-char (point-min))
        (forward-line 3)))))


(cl-defun welcome-dashboard--insert-two-columns (&key left-items
                                                      left-title
                                                      left-formatter
                                                      shortcut-modifier
                                                      shortcut-face)
  "Insert two columns where the right column shows keyboard shortcuts.
LEFT-ITEMS is the list of items to display in the left column.
LEFT-TITLE is a string for the left column header.
LEFT-FORMATTER is a function that takes an item and
returns a formatted string.
SHORTCUT-MODIFIER is a string (e.g. \"M\" or \"C\") for
the keyboard shortcut prefix.
SHORTCUT-FACE is the face for the keyboard shortcuts."

  (let* ((window-width (window-width))
         (count (length left-items))
         (shortcut-width (+ (length shortcut-modifier) 4))  ; +4 for [-n]
         ;; Calculate total width needed
         (total-content-width (+ welcome-dashboard--max-length
                                shortcut-width
                                welcome-dashboard-shortcut-spacing))
         ;; Center everything
         (left-margin (max welcome-dashboard-min-left-padding
                          (/ (- window-width total-content-width) 2))))
    
    ;; Insert title
    (when left-title
      (insert (make-string left-margin ?\s))
      (insert left-title)
      (insert "\n"))
    
    ;; Insert content
    (dotimes (i count)
      ;; Start new line with margin
      (insert (make-string left-margin ?\s))
      ;; Insert left column item
      (when (< i (length left-items))
        (let* ((item (nth i left-items))
               (formatted-item (funcall left-formatter item))
               (shortcut-text (when (not (string-empty-p (string-trim shortcut-modifier)))
                            (propertize (format "[%s%d]" shortcut-modifier (1+ i))
                                       'face shortcut-face)))
               ;; Calculate padding between item and shortcut
               (padding-length (max 0 ; Ensure padding is never negative
                                    (- (+ welcome-dashboard--max-length welcome-dashboard-shortcut-spacing)
                                       (length formatted-item)))))
          ;; Insert item, padding, and shortcut
          (insert formatted-item)
          (when shortcut-text
            (insert (propertize (make-string padding-length welcome-dashboard-shortcut-delimiter) 'face 'welcome-dashboard-shortcut-delimiter-face))
            (insert shortcut-text))))
      (insert "\n"))))


(defun welcome-dashboard--insert-separator ()
  "Insert a separator line."
  (when (and welcome-dashboard-show-separator (stringp welcome-dashboard-separator-string))
    (insert "\n")
    (welcome-dashboard--insert-centered (propertize welcome-dashboard-separator-string 'face 'welcome-dashboard-separator-face))
    (insert "\n")))


(defun welcome-dashboard--calculate-item-length (items formatter)
  "Calculate the maximum length of formatted items.
ITEMS is the list of items to check.
FORMATTER is a function that takes an item and returns a formatted string."
  (if items (apply #'max (mapcar (lambda (item) (length (funcall formatter item))) items)) 0))


(cl-defun welcome-dashboard--calculate-max-length (&key project-items recent-items)
  "Calculate the maximum length of the PROJECT-ITEMS and RECENT-ITEMS."
  (let* ((max-length-projects (welcome-dashboard--calculate-item-length project-items #'welcome-dashboard--format-project))
         (max-length-recent (welcome-dashboard--calculate-item-length recent-items #'welcome-dashboard--format-file)))
    (setq welcome-dashboard--max-length (max max-length-projects max-length-recent))))


(defun welcome-dashboard--update-max-length ()
  "Update the max length of the recent files."
  (recentf-mode)
  (setq welcome-dashboard--max-length (welcome-dashboard--calculate-max-length
                                      :project-items (welcome-dashboard--get-recent-projects)
                                      :recent-items (seq-take recentf-list 9))))

(provide 'welcome-dashboard)
;;; welcome-dashboard.el ends here
