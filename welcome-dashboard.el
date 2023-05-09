;;; welcome-dashboard.el --- Simple welcome-dashboard screen -*- lexical-binding: t -*-

;; Welcome-dashboard screen

;; Authod: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Maintainer: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Created: 2023
;; Package-Version: 0.1
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.1"))
;; Homepage: https://github.com/konrad1977/welcome-dashboard

;;; Commentary:

;;; Minimalistic dashboard for Emacs.

;; code:

(require 'all-the-icons)
(require 'json)
(require 'recentf)
(require 'url)

;;; Code:

(defvar welcome-dashboard-mode nil)
(defvar welcome-dashboard-recentfiles '()
  "Recent list.")

;; (defvar recent-projects '()
;;   "List of recent projects.")

(defvar welcome-dashboard-temperature nil)
(defvar welcome-dashboard-weatherdescription nil)
(defvar welcome-dashboard-weathericon nil)

(defcustom welcome-dashboard-title "Quick access [C-number to open file]"
  "Welcome-dashboard title."
  :group 'welcome-dashboard
  :type '(string))

(defcustom welcome-dashboard-show-weather-info t
  "Show/hide weather info."
  :group 'welcome-dashboard
  :type '(boolean))

(defcustom welcome-dashboard-min-left-padding 10
  "Minimum left padding when resizing window."
  :group 'welcome-dashboard
  :type '(natnum))

(defcustom welcome-dashboard-path-max-length 72
  "Latitude for weather information."
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

(defconst welcome-dashboard-buffer "*welcome*"
  "Welcome-dashboard buffer name.")

(defvar welcome-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'welcome-dashboard--open-recent-file)
    (define-key map (kbd "<return>") 'welcome-dashboard--open-recent-file)
    (define-key map (kbd "o") 'welcome-dashboard--open-recent-file)

    ;; Add shortcuts for file indexes
    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (concat "C-" (number-to-string i)))
        `(lambda ()
           (interactive)
           (welcome-dashboard--open-recent-file-at-index ,i))))
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
  (use-local-map welcome-dashboard-mode-map))

(defface welcome-dashboard-title-face
  '((t :foreground "#87AAF8" :height 1.2 :weight thin))
  "Face added to code-usage display."
  :group 'welcome-dashboard)

(defface welcome-dashboard-info-face
  '((t :foreground "#F66D86" :height 0.9 :bold t :italic t))
  "Face added to code-usage display."
  :group 'welcome-dashboard)

(defface welcome-dashboard-text-info-face
  '((t :foreground "#ADB5D0" :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'welcome-dashboard)

(defface welcome-dashboard-path-face
  '((t :foreground "#63677D" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for the file path."
  :group 'welcome-dashboard)

(defface welcome-dashboard-filename-face
  '((t :weight semi-bold))
  "Face for the file name."
  :group 'welcome-dashboard)

(defface welcome-dashboard-time-face
  '((t :foreground "#a6adc8" :height 0.9 :weight thin))
  "Face for time."
  :group 'welcome-dashboard)

(defface welcome-dashboard-weather-description-face
  '((t :foreground "#f9e2af" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for time."
  :group 'welcome-dashboard)

(defface welcome-dashboard-startup-time-face
  '((t :foreground "#C2A4F8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for time."
  :group 'welcome-dashboard)

(defface welcome-dashboard-shortcut-face
  '((t :foreground "#f9e2af" :height 0.9 :bold t))
  "Face for time."
  :group 'welcome-dashboard)

(defface welcome-dashboard-weather-icon-face
  '((t :height 0.9))
  "Face for time."
  :group 'welcome-dashboard)

(defface welcome-dashboard-weather-temperature-face
  '((t :foreground "#f38ba8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for time."
  :group 'welcome-dashboard)

(defun welcome-dashboard--weather-icon-from-code (code)
  "Maps a weather (as CODE) to a corresponding string."
  (pcase code
    (`0 "wi-day-sunny")
    ((or `1 `2 `3) "wi-day-cloudy")
    ((or `45 `48) "wi-day-fog")
    ((or `51 `53 `55) "wi-sprinkle")
    ((or `56 `57) "wi-snow")
    ((or `61 `63 `65) "wi-day-rain")
    ((or `66 `67) "wi-day-rain-mix")
    ((or `71 `73 `75) "wi-snow")
    (`77 "wi-snow")
    ((or `80 `81 `82) "wi-rain")
    ((or `85 `86) "wi-rain-mix")
    ((or `95 `96 `99) "wi-thunderstorm")
    (_ "Unknown")))

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
    (insert text)))

(defun welcome-dashboard--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (prop-pos (next-single-property-change line-start 'path nil line-end)))
    (when prop-pos
      (let ((file (get-text-property prop-pos 'path)))
        (message file)
        (if (file-exists-p file)
            (find-file file)
          (error "File %s does not exist" file))))))

(defun welcome-dashboard--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files welcome-dashboard-recentfiles))
    (when (<= 1 index (length files))
      (find-file (nth (1- index) files)))))

(defun welcome-dashboard--truncate-path-in-middle (path n)
  "Truncate the middle of PATH to length N by removing characters and adding an ellipsis."
  (if (<= (length path) n)
      path
    (let* ((left (/ (- n 3) 2))
           (right (- n left 3))
           (head (substring path 0 (+ left 1)))
           (tail (substring path (- (length path) right)))
           (ellipsis "..."))
      (concat head ellipsis tail))))

(defun welcome-dashboard--insert-recent-files ()
  "Insert the first 9 recent files with icons in the welcome-dashboard buffer."
  (recentf-mode)
  (insert "\n")
  (let* ((files welcome-dashboard-recentfiles)
         (left-margin (welcome-dashboard--calculate-padding-left)))
    (dolist (file files)
      (let* ((index (cl-position file files :test #'equal))
             (full-path (file-truename file))
             (shortcut (format "%d" (+ index +1)))
             (file-name (file-name-nondirectory file))
             (file-dir (file-name-directory file))
             (title (format "%s %s%s"
                    (propertize (all-the-icons-icon-for-file file :v-adjust -0.05) 'face '(:family "all-the-icons" :height 1.0))
                    (propertize (welcome-dashboard--truncate-path-in-middle file-dir welcome-dashboard-path-max-length) 'face 'welcome-dashboard-path-face)
                    (propertize file-name 'face 'welcome-dashboard-filename-face)))
             (title-with-path (propertize title 'path full-path))
             (title-with-path-and-shortcut (concat title-with-path (propertize (format " [%s]" shortcut) 'face 'welcome-dashboard-shortcut-face))))
        (insert (format "%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut))))))

(defun welcome-dashboard--calculate-padding-left ()
  "Calculate padding for left side."
  (if-let* ((files welcome-dashboard-recentfiles)
         (max-length (apply 'max (mapcar (lambda (path) (length (welcome-dashboard--truncate-path-in-middle path welcome-dashboard-path-max-length))) files)))
         (filenames (mapcar (lambda (path) (file-name-nondirectory path)) files))
         (max-filename-length (/ (apply 'max (mapcar 'length filenames)) 2))
         (left-margin (max (+ welcome-dashboard-min-left-padding max-filename-length) (/ (- (window-width) max-length) 2))))
      (- left-margin max-filename-length)
    welcome-dashboard-min-left-padding))

(defun welcome-dashboard--insert-text (text)
  "Insert (as TEXT)."
  (let ((left-margin (welcome-dashboard--calculate-padding-left)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text))))

(defun welcome-dashboard--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer."
  (when (equal (buffer-name) welcome-dashboard-buffer)
    (welcome-dashboard--refresh-screen)))

(defun welcome-dashboard--fetch-weather-data ()
  "Fetch weather data from API."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true" welcome-dashboard-latitude welcome-dashboard-longitude)))
    (url-retrieve url
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
                           (json-obj (json-read-from-string json-data))
                           (current-weather (cdr (assoc 'current_weather json-obj)))
                           (temp (cdr (assoc 'temperature current-weather)))
                           (weather-code (cdr (assoc 'weathercode current-weather)))
                           (weather-icon (all-the-icons-icon-for-weather
                                          (welcome-dashboard--weather-icon-from-code weather-code))))
                      (setq welcome-dashboard-weathericon weather-icon)
                      (setq welcome-dashboard-temperature (format "%s" temp))
                      (setq welcome-dashboard-weatherdescription (format "%s" (welcome-dashboard--weather-code-to-string weather-code))))
                    (welcome-dashboard--refresh-screen))
                  nil
                  t)))

;;;###autoload
(defun welcome-dashboard-create-welcome-hook ()
  "Setup welcome-dashboard screen."
  (when (< (length command-line-args) 2)
    (add-hook 'switch-to-buffer 'welcome-dashboard--redisplay-buffer-on-resize)
    (add-hook 'window-size-change-functions 'welcome-dashboard--redisplay-buffer-on-resize)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (welcome-dashboard--refresh-screen)
                                    (when welcome-dashboard-show-weather-info
                                      (welcome-dashboard--fetch-weather-data))))))

(defun welcome-dashboard--insert-startup-time ()
  "Insert startup time."
  (welcome-dashboard--insert-text (format "%s %s %s %s"
                                (propertize (all-the-icons-octicon "clock")
                                            'face `(:family ,(all-the-icons-octicon-family) :height 1.0)
                                            'display '(raise 0))
                                (propertize "Startup time:" 'face 'welcome-dashboard-text-info-face)
                                (propertize (emacs-init-time "%.2f") 'face 'welcome-dashboard-startup-time-face)
                                (propertize "seconds" 'face 'welcome-dashboard-text-info-face))))


(defun welcome-dashboard--insert-package-info (packages)
  "Insert package info as (PACKAGES)."
  (welcome-dashboard--insert-text (format "%s %s %s"
                                (propertize (all-the-icons-octicon "package")
                                            'face `(:family ,(all-the-icons-octicon-family) :height 1.0)
                                            'display '(raise 0))
                                (propertize packages 'face 'welcome-dashboard-info-face)
                                (propertize "packages loaded" 'face 'welcome-dashboard-text-info-face))))

(defun welcome-dashboard--insert-weather-info ()
  "Insert weather info."
  (when welcome-dashboard-show-weather-info
    (if welcome-dashboard-weatherdescription
        (welcome-dashboard--insert-text (format "%s %s, %s%s"
                                      (propertize welcome-dashboard-weathericon 'face '(:family "Weather icons" :height 1.0) 'display '(raise 0))
                                      (propertize welcome-dashboard-weatherdescription 'face 'welcome-dashboard-weather-description-face)
                                      (propertize welcome-dashboard-temperature 'face 'welcome-dashboard-weather-temperature-face)
                                      (propertize "℃" 'face 'welcome-dashboard-text-info-face)))
      (welcome-dashboard--insert-text (propertize "Loading weather data..." 'face 'welcome-dashboard-weather-temperature-face)))))

;; (defun insert-recent-projects ()
;;   "Insert recent projects."
;;   (projectile-mode +1)
;;   (setq recent-projects (projectile-relevant-known-projects))
;;   (dolist (project (seq-take recent-projects 3))
;;     (welcome-dashboard--insert-text (projectile-project-name project))))

(defun welcome-dashboard--refresh-screen ()
  "Show the welcome-dashboard screen."
  (setq welcome-dashboard-recentfiles (seq-take recentf-list 9))
  (with-current-buffer (get-buffer-create welcome-dashboard-buffer)
    (let* ((buffer-read-only)
           (image (create-image welcome-dashboard-image-file 'png nil :width welcome-dashboard-image-width :height welcome-dashboard-image-height))
           (size (image-size image))
           (width (car size))
           (left-margin (max welcome-dashboard-min-left-padding (floor (/ (- (window-width) width) 2))))
           (packages (format "%d" (length package-activated-list))))
      (erase-buffer)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (insert "\n")
        (welcome-dashboard--insert-text (propertize welcome-dashboard-title 'face 'welcome-dashboard-title-face))
        (welcome-dashboard--insert-recent-files)
        (setq cursor-type nil)
        (insert "\n\n")

        (welcome-dashboard--insert-startup-time)
        (welcome-dashboard--insert-package-info packages)
        (welcome-dashboard--insert-weather-info)

        (insert "\n\n")
        (insert (make-string left-margin ?\ ))
        (insert-image image)
        (insert "\n\n")
        (welcome-dashboard--insert-centered (propertize (format-time-string "%A, %B %d %H:%M") 'face 'welcome-dashboard-time-face))
        (switch-to-buffer welcome-dashboard-buffer)
        (read-only-mode +1)
        (welcome-dashboard-mode)
        (goto-char (point-min))

        (forward-line 3)))))

(provide 'welcome-dashboard)
;;; welcome-dashboard.el ends here
