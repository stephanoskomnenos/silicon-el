;;; silicon.el --- Generate images from source files. -*- lexical-binding: t -*-

;; Author: Jens Östlund
;; Maintainer: Jens Östlund
;; Homepage: https://github.com/iensu/silicon-el
;; Version: 1
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Convenience package for creating images of the current source buffer
;; using Silicon <https://github.com/Aloxaf/silicon>. It requires you to
;; have the `silicon' command-line tool installed and preferably on your path.
;; You can specify the path to the executable by setting `silicon-executable-path'.

;; The package declares:
;; 1. `silicon-buffer-file-to-png' which tries to create a PNG of the current source code buffer.
;; 2. `silicon-region-to-png' to capture only the selected region.
;; 3. `silicon-region-to-clipboard' and `silicon-buffer-to-clipboard' for clipboard output.

;;; Code:
(eval-when-compile (require 'subr-x))

(defgroup silicon ()
  "Customize group for Silicon."
  :group 'convenience)

(defcustom silicon-executable-path "silicon"
  "Path to silicon executable."
  :type 'string
  :group 'silicon)

(defcustom silicon-default-background-color "#00000000"
  "Default background color of output PNG."
  :type 'string
  :group 'silicon)

(defcustom silicon-completion-function 'ido-completing-read
  "Function to use for completion.

Function needs to have a signature similar to `ido-completing-read', for example `ivy-completing-read'."
  :type 'function
  :group 'silicon)

(defcustom silicon-default-theme nil
  "Set default theme for generated PNG. Inspect `silicon-available-themes' for available themes."
  :type 'string
  :group 'silicon)

(defcustom silicon-show-line-numbers nil
  "Add line numbers to resulting PNG by default."
  :type 'boolean
  :group 'silicon)

(defcustom silicon-show-window-controls nil
  "Add window controls to the resulting PNG by default."
  :type 'boolean
  :group 'silicon)

(defcustom silicon-rounded-corners t
  "Rounded corners on code window."
  :type 'boolean
  :group 'silicon)

(defcustom silicon-window-shadow nil
  "Specifies the window shadow.

The value is a plist with keys `:blur-radius', `:color', `:offset-x' and/or `:offset-y'."
  :type '(plist :options ((:blur-radius integer)
                          (:color string)
                          (:offset-x integer)
                          (:offset-y integer)))
  :group 'silicon)

(defcustom silicon-default-save-directory nil
  "Default directory to save silicon screenshots.
If nil (default), save next to the current buffer's file.
If a path (e.g., \"~/Pictures/\"), save all screenshots there."
  :type '(choice (const :tag "Next to file (default)" nil)
                 (directory :tag "Custom directory"))
  :group 'silicon)

(defvar -silicon--background-color-history '())
(defvar -silicon--cmd-options-history '())

(defvar silicon-available-themes nil
  "List of available silicon themes.")

;; Try to populate `silicon-available-themes'
(when (commandp silicon-executable-path)
  (setq-default silicon-available-themes
                (split-string (shell-command-to-string (format "%s --list-themes" silicon-executable-path))
                              "[\r\n]+"
                              'omit-nulls
                              "\s+")))


(defun -silicon--build-command-opts-string (&rest args)
  "Generate a silicon command options string.

Supported options are `:line-numbers', `:window-controls', `:background-color', `:rounded-corners', `shadow', `:theme', `:highlight-lines' and `:language'"
  (let* ((show-line-numbers (or (plist-get args :line-numbers) silicon-show-line-numbers))
         (show-window-controls (or (plist-get args :window-controls) silicon-show-window-controls))
         (rounded-corners (or (plist-get args :rounded-corners) silicon-rounded-corners))
         (background-color (or (plist-get args :background-color) silicon-default-background-color))
         (theme (or (plist-get args :theme) silicon-default-theme))
         (highlight-lines (plist-get args :highlight-lines))
         (language (plist-get args :language))

         (shadow (or (plist-get args :shadow) silicon-window-shadow))
         (shadow-blur-radius (plist-get shadow :blur-radius))
         (shadow-color (plist-get shadow :color))
         (shadow-offset-x (plist-get shadow :offset-x))
         (shadow-offset-y (plist-get shadow :offset-y))

         (opts `(,(when (not show-line-numbers) "--no-line-number")
                 ,(when (not show-window-controls) "--no-window-controls")
                 ,(when (not rounded-corners) "--no-round-corner")
                 ,(format "--background '%s'" background-color)
                 ,(when theme (format "--theme '%s'" theme))
                 ,(when highlight-lines (format "--highlight-lines '%s'" highlight-lines))
                 ,(when language (format "--language '%s'" language))
                 ,(when shadow-blur-radius (format "--shadow-blur-radius %d" shadow-blur-radius))
                 ,(when shadow-color (format "--shadow-color '%s'" shadow-color))
                 ,(when shadow-offset-x (format "--shadow-offset-x %d" shadow-offset-x))
                 ,(when shadow-offset-y (format "--shadow-offset-y %d" shadow-offset-y)))))

    (string-join (seq-remove #'null opts) " ")))

(defun -silicon--build-command (file-path options &optional prompt-for-output-file)
  "Build the silicon command string for FILE input."
  (let* ((default-base-name (file-name-base file-path))
         (default-original-dir (file-name-directory file-path))
         (save-dir (if silicon-default-save-directory
                       (expand-file-name silicon-default-save-directory)
                     default-original-dir))
         (default-output-path (expand-file-name (concat default-base-name ".png") save-dir))
         (output-path (if prompt-for-output-file
                          (read-file-name "Output file: "
                                          save-dir ; Prompt starts in our desired dir
                                          default-output-path) ; Suggests this file name
                        default-output-path))) ; Default save path
    (string-join `(,silicon-executable-path
                   ,options
                   ,(format "--output '%s'" output-path)
                   ,file-path)
                 " ")))

(defun -silicon--build-stdin-command (options &optional prompt-for-output-file)
  "Build the silicon command string for STDIN input.
Reads from stdin because no file-path is provided."
  (let* ((default-base-name (if buffer-file-name
                                (file-name-base buffer-file-name)
                              "snippet"))
         (default-original-dir (if buffer-file-name
                                   (file-name-directory buffer-file-name)
                                 default-directory))
         (save-dir (if silicon-default-save-directory
                       (expand-file-name silicon-default-save-directory)
                     default-original-dir))
         (default-output-path (expand-file-name (concat default-base-name ".png") save-dir))
         (output-path (if prompt-for-output-file
                          (read-file-name "Output file: "
                                          save-dir ; Prompt starts here
                                          default-output-path) ; Suggests this name
                        default-output-path))) ; Default save path

    (string-join `(,silicon-executable-path
                    ,options
                    ,(format "--output '%s'" output-path))
                   " ")))

;;;###autoload
(defun silicon-set-default-theme ()
  "Set the default silicon theme. This command allows you to select from the list of available themes."
  (interactive)
  (defvar silicon-available-themes)
  (setq silicon-default-theme (funcall silicon-completion-function
                                       "Select theme: "
                                       silicon-available-themes
                                       nil
                                       (not (null silicon-available-themes))
                                       silicon-default-theme)))

(defun -silicon--get-options-string (universal-arg &optional language)
  "Get the silicon options string based on UNIVERSAL-ARG and LANGUAGE.
This handles the C-u and C-u C-u prompts."
  (let ((is-edit (= universal-arg 16))
        (is-prompt (= universal-arg 4))
        (default-opts (-silicon--build-command-opts-string :language language)))
    (cond (is-edit
           (read-string "Options: "
                        default-opts
                        '-silicon--cmd-options-history
                        default-opts))

          (is-prompt
           (let ((theme
                  (funcall silicon-completion-function
                           "Theme: "
                           silicon-available-themes
                           nil
                           (not (null silicon-available-themes))
                           silicon-default-theme))
                 (background-color
                  (read-string "Background color: "
                               silicon-default-background-color
                               '-silicon--background-color-history
                               silicon-default-background-color))
                 (highlight-lines (read-string "Highlight lines: " nil nil nil))
                 (show-line-numbers (yes-or-no-p "Add line numbers? "))
                 (show-window-controls (yes-or-no-p "Add window controls? "))
                 (rounded-corners (yes-or-no-p "Rounded corners? ")))
             (-silicon--build-command-opts-string :theme theme
                                                  :background-color background-color
                                                  :highlight-lines (if (string= "" highlight-lines) nil highlight-lines)
                                                  :line-numbers show-line-numbers
                                                  :window-controls show-window-controls
                                                  :rounded-corners rounded-corners
                                                  :language language)))
          
          (t default-opts))))

(defun -silicon--run (universal-arg source-type dest-type &optional begin end)
  "Core executor for all silicon commands.
UNIVERSAL-ARG controls prompts.
SOURCE-TYPE is 'buffer or 'region.
DEST-TYPE is 'file or 'clipboard.
BEGIN and END are used for 'region."
  (interactive)
  (if (not (commandp silicon-executable-path))
      (error "Could not find `silicon' executable, try setting `silicon-executable-path'."))

  (let* ((fname (buffer-file-name))
         (language (when (eq source-type 'region)
                     (file-name-extension fname)))
         (options-string (-silicon--get-options-string universal-arg language))
         ;; Prompt for output file *only* if C-u or C-u C-u was pressed
         (prompt-for-file (or (= universal-arg 4) (= universal-arg 16)))
         (command-string nil))

    (cond
     ;; --- Buffer Source ---
     ((eq source-type 'buffer)
      (if-let* ((file-name (buffer-file-name))
                (file-path (expand-file-name file-name)))
          (cond
           ;; Case 1: Buffer -> File
           ((eq dest-type 'file)
            (setq command-string (-silicon--build-command file-path options-string prompt-for-file)))
           ;; Case 2: Buffer -> Clipboard
           ((eq dest-type 'clipboard)
            (setq command-string (string-join `(,silicon-executable-path
                                                ,options-string
                                                "--to-clipboard"
                                                ,file-path)
                                              " "))))
        (error "Current buffer is not associated with any file.")))

     ;; --- Region Source ---
     ((eq source-type 'region)
      (let* ((region-text (buffer-substring-no-properties begin end)))
        (cond
         ;; Case 3: Region -> File
         ((eq dest-type 'file)
          (let* ((base-silicon-command (-silicon--build-stdin-command options-string prompt-for-file)))
            (setq command-string (concat (format "echo %s" (shell-quote-argument region-text))
                                         " | "
                                         base-silicon-command))))
         ;; Case 4: Region -> Clipboard
         ((eq dest-type 'clipboard)
          (let* ((base-silicon-command (string-join `(,silicon-executable-path
                                                      ,options-string
                                                      "--to-clipboard")
                                                    " ")))
            (setq command-string (concat (format "echo %s" (shell-quote-argument region-text))
                                         " | "
                                         base-silicon-command))))))))
    
    (when command-string
      (compile command-string))))

;;;###autoload
(defun silicon-buffer-file-to-png (universal-arg)
  "Generate a PNG of the current buffer file and save it.
With C-u, prompts for options.
With C-u C-u, prompts for raw options string."
  (interactive "p")
  (-silicon--run universal-arg 'buffer 'file))

;;;###autoload
(defun silicon-buffer-to-clipboard (universal-arg)
  "Generate a PNG of the current buffer file and copy it to clipboard.
With C-u, prompts for options.
With C-u C-u, prompts for raw options string."
  (interactive "p")
  (-silicon--run universal-arg 'buffer 'clipboard))

;;;###autoload
(defun silicon-region-to-png (begin end universal-arg)
  "Generate a PNG of the current region and save it.
With C-u, prompts for options.
With C-u C-u, prompts for raw options string."
  (interactive "r\np")
  (-silicon--run universal-arg 'region 'file begin end))

;;;###autoload
(defun silicon-region-to-clipboard (begin end universal-arg)
  "Generate a PNG of the current region and copy it to clipboard.
With C-u, prompts for options.
With C-u C-u, prompts for raw options string."
  (interactive "r\np")
  (-silicon--run universal-arg 'region 'clipboard begin end))


(provide 'silicon)
;;; silicon.el ends here
