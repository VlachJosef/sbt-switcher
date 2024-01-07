;;; sbt-switcher.el --- Easy switching between running sbt processes

;; Copyright (c) 2021 Josef Vlach

;; Homepage: https://github.com/VlachJosef/sbt-switcher
;; Package-Version:  0.1

;;; Commentary:
;;
;;; Code:

(require 'sbt-mode-buffer)

(defun sbt-switcher:mode-buffers ()
  (cl-loop for buffer being the elements of (buffer-list)
           when (with-current-buffer buffer
                  (sbt:mode-p))
           collect buffer into mode-buffers
           finally return mode-buffers))

(defun sbt-switcher:frontend-p (buffer-name)
  (string-match "-frontend" buffer-name))

(defun sbt-switcher:sbt-project-name (buffer-name)
  (string-match "/\\([^/]*\\)/>" buffer-name)
  (match-string 1 buffer-name))

(defun sbt-switcher:sbt-path-and-project-name (buffer-name)
  (string-match "\\(*sbt\\*<.*\\)/\\(.*\\)/>" buffer-name)
  (cons (match-string 1 buffer-name) (match-string 2 buffer-name)))

(defun sbt-switcher:sbt-complement-buffer-name (buffer-name project-name)
  (if (sbt-switcher:frontend-p project-name)
      (replace-regexp-in-string "-frontend" "" buffer-name)
    (replace-regexp-in-string project-name (format "%s-frontend" project-name) buffer-name)))

(defun sbt-switcher:sbts-for-project (buffer)
  (let* ((buffer-name (buffer-name buffer))
         (project-name (sbt-switcher:sbt-project-name buffer-name))
         (complement-buffer-name (sbt-switcher:sbt-complement-buffer-name buffer-name project-name))
         (buffer (seq-find  (lambda (buffer) (equal (buffer-name buffer) complement-buffer-name)) (sbt-switcher:mode-buffers))))
    (if buffer
        (switch-to-buffer-other-window buffer)
      (let* ((project-path (sbt-switcher:sbt-path-and-project-name buffer-name))
             (complement-buffers (seq-filter
                                  (lambda (buf) (and (string-prefix-p (car project-path) (buffer-name buf))
                                                (not (string-prefix-p (concat (car project-path) "/" (cdr project-path)) (buffer-name buf)))))
                                  (sbt-switcher:mode-buffers))))
        (pcase (length complement-buffers)
          (0 (message "No complement buffer found for %s" project-path))
          (1 (switch-to-buffer-other-window (car complement-buffers)))
          (_ (message "More than one complement buffers for %s found: %s" project-path complement-buffers)
             (switch-to-buffer-other-window (car complement-buffers))))))))

(defun sbt-switcher:sbt ()
  "Find all sbt processes and switch to best candidate buffer or if no best candidate can be decided let user select."
  (interactive)
  (let* ((all-buffers (sbt-switcher:mode-buffers))
         (invoked-from-directory (expand-file-name default-directory))
         (best-candidate (seq-find (lambda (buf)
                                     (with-current-buffer buf
                                       (string-prefix-p (expand-file-name default-directory) invoked-from-directory)))
                                   all-buffers))
         (other-candidates (seq-filter (lambda (buf)
                                         (with-current-buffer buf
                                           (string-prefix-p invoked-from-directory (expand-file-name default-directory))))
                                       all-buffers)))

    (if (and best-candidate
             (not (equal (current-buffer) best-candidate)))
        (switch-to-buffer-other-window best-candidate)
      (pcase (length other-candidates)
        (0 (if (null all-buffers)
               (message "No SBT buffer found." )
             (sbt-switcher:select-from all-buffers)))
        (1 (let ((only-candidate (car other-candidates)))
             (if (not (equal (current-buffer) only-candidate))
                 (switch-to-buffer-other-window only-candidate)
               (sbt-switcher:sbts-for-project only-candidate))))
        (_ (sbt-switcher:select-from other-candidates))))))

(defun sbt-switcher:select-from (all-buffers)
  (let ((buffers (mapcar 'buffer-name all-buffers)))
    (switch-to-buffer-other-window
     (completing-read "SBT buffer: " buffers nil t))))

(provide 'sbt-switcher)
;;; sbt-switcher.el ends here
