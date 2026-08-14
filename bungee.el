;;; bungee.el --- Point history bungee cord  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Bungee records where the point is before you jump somewhere else, and
;; lets you return to a saved position afterwards.  It is most useful
;; when following definitions: jumping to a definition loses your
;; place, and Bungee gives it back.
;;
;; Positions are saved automatically before the commands listed in
;; `bungee-save-before-functions' run.  By default those are
;; `xref-find-definitions', `xref-find-references', and `goto-line'.
;; Call `bungee-add' (M-x bungee-add) to save the current position
;; manually, and `bungee-history' (M-x bungee-history) to jump to one
;; of the saved positions.
;;
;; Saved points are tracked with markers, so they follow edits in the
;; buffer.  When the buffer is killed, its position and line are
;; stored in the history item; jumping to such an item reopens the
;; file (or recreates a non-file buffer) and restores point there.

;;; Code:

(defvar bungee--history nil
  "List of saved points, in order from newest to oldest.

Each element is a plist; see `bungee--history-item'.")

(defvar bungee--history-max-length 100
  "Maximum number of items to keep in `bungee--history'.")

(defvar bungee--save-before-advised nil
  "Functions we have advised via `bungee-set-hooks'.")

;; `vertico-sort-function' is defined in vertico.el.  Declare it here
;; so `bungee-history' can bind it without requiring Vertico.
(defvar vertico-sort-function nil)

(defvar bungee-save-before-functions
  '(xref-find-definitions
    xref-find-references
    goto-line)
  "Functions advised to save the current position before running.

These default to commands that move the user away from the current
line (e.g., jumping to a definition).  Set this and call
`bungee-set-hooks' to (re)apply the advice:

  (setq bungee-save-before-functions
        \\='(xref-find-definitions imenu))
  (bungee-set-hooks)")

;;; Advice

(defun bungee--save-before-advice (&rest _)
  "Call `bungee-add' to save the current position.

Used as `:before' advice on the functions in
`bungee-save-before-functions'.  Ignores the arguments, since
`:before' advice is invoked with the advised function's arguments."
  (bungee-add))

(defun bungee--advise (fn)
  "Attach `bungee--save-before-advice' to FN.

FN is recorded in `bungee--save-before-advised' so that
`bungee-set-hooks' can remove the advice if FN is later dropped
from `bungee-save-before-functions'."
  (unless (advice-member-p #'bungee--save-before-advice fn)
    (advice-add fn :before #'bungee--save-before-advice))
  (unless (memq fn bungee--save-before-advised)
    (push fn bungee--save-before-advised)))

(defun bungee-set-hooks ()
  "Synchronize advice with `bungee-save-before-functions'.

Adds `bungee--save-before-advice' to every listed function and
removes it from functions that were advised but are no longer
listed.  Functions that are still autoload stubs are deferred via
`with-eval-after-load' until their library loads, because advising
an interactive autoload stub corrupts its argument handling:
`call-interactively' would invoke it with zero arguments."
  (interactive)
  (dolist (fn bungee--save-before-advised)
    (unless (memq fn bungee-save-before-functions)
      (advice-remove fn #'bungee--save-before-advice)))
  (setq bungee--save-before-advised nil)
  (dolist (fn bungee-save-before-functions)
    (let ((def (and (fboundp fn) (symbol-function fn))))
      (if (autoloadp def)
          (with-eval-after-load (nth 1 def)
            (when (memq fn bungee-save-before-functions)
              (bungee--advise fn)))
        (bungee--advise fn)))))

;;; History items

(defun bungee--history-item ()
  "Capture the current position as a history item.

Records the current buffer, a marker and position at point, the
line number, the trimmed text of the current line, and the file
path (if any).  The line's text is fontified first so it is
retained accurately when the buffer dies."
  (let* ((buffer (current-buffer))
         (pt (point))
         (bol (line-beginning-position))
         (eol (line-end-position)))
    (when (bound-and-true-p font-lock-mode)
      (font-lock-ensure bol eol))
    (list :buffer buffer
          :buffer-name (buffer-name buffer)
          :marker (copy-marker pt t)
          :position pt
          :line (line-number-at-pos bol)
          :content (string-trim (buffer-substring bol eol))
          :path (buffer-file-name buffer))))

(defun bungee--history-item-position (item)
  "Return ITEM's current position.

Tracks edits via the item's marker, falling back to the stored
`:position' when the buffer is dead or the marker no longer
belongs to it."
  (let ((buffer (plist-get item :buffer))
        (marker (plist-get item :marker)))
    (if (and marker
             (buffer-live-p buffer)
             (eq (marker-buffer marker) buffer))
        (marker-position marker)
      (plist-get item :position))))

(defun bungee--history-item-label (item)
  "Return the display label for ITEM.

The label is \"NAME:LINE - CONTENT\", where NAME is the live buffer
name or the file name when the buffer is dead."
  (let* ((buffer (plist-get item :buffer))
         (path (plist-get item :path))
         (live (buffer-live-p buffer))
         (line (if live
                   (with-current-buffer buffer
                     (line-number-at-pos
                      (or (bungee--history-item-position item) 1)))
                 (plist-get item :line)))
         (name (if live
                   (buffer-name buffer)
                 (or (and path (file-name-nondirectory path))
                     (plist-get item :buffer-name)
                     (prin1-to-string buffer)))))
    (format "%s:%d - %s"
            name
            (or line 0)
            (or (plist-get item :content) ""))))

(defun bungee--history-alist ()
  "Return `bungee--history' as an alist of (label . item).

Labels are prefixed with a zero-padded index so any alphabetical
sorter keeps the newest item first."
  (let* ((count (length bungee--history))
         (width (length (number-to-string (max 1 count))))
         (fmt (format "%%0%dd %%s" width))
         (index 0)
         (result nil))
    (dolist (item bungee--history)
      (setq index (1+ index))
      (push (cons (format fmt index (bungee--history-item-label item)) item)
            result))
    (nreverse result)))

;;; Buffer lifecycle

(defun bungee--update-on-buffer-kill ()
  "Persist positions before the current buffer is killed.

Runs on `kill-buffer-hook'.  For every history item whose buffer
is being killed, stores the marker's current position and line
back into the item, since the marker becomes detached when the
buffer dies."
  (let ((buffer (current-buffer)))
    (dolist (item bungee--history)
      (let ((marker (plist-get item :marker)))
        (when (and (eq (plist-get item :buffer) buffer)
                   (markerp marker))
          (let ((pos (marker-position marker)))
            (when pos
              (plist-put item :position pos)
              (plist-put item :line (line-number-at-pos pos)))))))))

(add-hook 'kill-buffer-hook #'bungee--update-on-buffer-kill)

(defun bungee--reactivate-on-file-open ()
  "Re-attach markers when a tracked file is (re)opened.

Runs on `find-file-hook'.  For every history item whose buffer is
dead but whose stored path matches the just-opened file, re-points
the item's buffer and marker at the new buffer so it keeps
tracking edits (also used when a file is reopened outside of
`bungee-history')."
  (let* ((file (buffer-file-name (current-buffer)))
         (file-exp (and file (expand-file-name file))))
    (when file-exp
      (dolist (item bungee--history)
        (let* ((buffer (plist-get item :buffer))
               (marker (plist-get item :marker))
               (path (plist-get item :path)))
          (when (and path
                     marker
                     (not (buffer-live-p buffer))
                     (string= (expand-file-name path) file-exp))
            (plist-put item :buffer (current-buffer))
            (set-marker marker
                        (min (or (plist-get item :position) (point-min))
                             (point-max))
                        (current-buffer))))))))

(add-hook 'find-file-hook #'bungee--reactivate-on-file-open)

;;; Commands

;;;###autoload
(defun bungee-add ()
  "Save the current position to `bungee--history'.

Drops any existing item for the same buffer and position, then
trims the history to `bungee--history-max-length' items."
  (interactive)
  (let* ((item (bungee--history-item))
         (buffer (plist-get item :buffer))
         (pos (plist-get item :position)))
    (setq bungee--history
          (cons item
                (seq-remove
                 (lambda (old)
                   (and (eq (plist-get old :buffer) buffer)
                        (eq (bungee--history-item-position old) pos)))
                 bungee--history)))
    (when (> (length bungee--history) bungee--history-max-length)
      (setq bungee--history
            (seq-take bungee--history bungee--history-max-length)))))

;;;###autoload
(defun bungee-history ()
  "Jump to a saved position from `bungee--history'.

Prompts for a history item via `completing-read', with Vertico's
sorting disabled so the newest item stays first.  If the item's
buffer has been killed, its file is reopened (or a placeholder
buffer is created), then switches to the buffer, moves point to
the saved position, and recenters."
  (interactive)
  (unless bungee--history
    (user-error "No history items saved yet"))
  (let* ((alist (bungee--history-alist))
         (old-sort (symbol-value 'vertico-sort-function))
         (choice (progn
                   (when (bound-and-true-p vertico-mode)
                     (setq vertico-sort-function #'identity))
                   (unwind-protect
                       (completing-read "Bungee to: " alist nil t)
                     (setq vertico-sort-function old-sort)))))
    (when choice
      (let* ((item (cdr (assoc choice alist)))
             (buffer (plist-get item :buffer))
             (position (bungee--history-item-position item)))
        (unless (buffer-live-p buffer)
          (let ((path (plist-get item :path)))
            (setq buffer
                  (if (and path (file-exists-p path))
                      (find-file-noselect path)
                    (get-buffer-create
                     (or (and path (file-name-nondirectory path))
                         (plist-get item :buffer-name)
                         (buffer-name buffer)
                         "*bungee*"))))
            (when (markerp (plist-get item :marker))
              (plist-put item :buffer buffer)
              (set-marker (plist-get item :marker)
                          (min (or position (point-min))
                               (with-current-buffer buffer (point-max)))
                          buffer))))
        (switch-to-buffer buffer)
        (goto-char (min (or position (point-min)) (point-max)))
        (recenter)))))

(bungee-set-hooks)

(provide 'bungee)

;;; bungee.el ends here
