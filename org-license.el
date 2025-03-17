(defun org-license--fetch-spdx-data (&optional force-refresh)
  (when (or force-refresh (null org-license--spdx-data))
    (condition-case err
        (if (and (not force-refresh) 
                 (file-exists-p org-license-cache-file)
                 (file-newer-than-file-p org-license-cache-file
                                         (time-subtract (current-time)
                                                        (days-to-time 7))))
            (setq org-license--spdx-data (json-read-file org-license-cache-file))
          (let* ((url "https://spdx.org/licenses/licenses.json")
                 (response (url-retrieve-synchronously url t t 10)))
            (if response
                (with-current-buffer response
                  (goto-char url-http-end-of-headers)
                  (let ((content (json-parse-buffer :object-type 'alist)))
                    (when (and (alist-get 'licenses content)
                               (cl-every #'identity 
                                         (mapcar (lambda (l) 
                                                   (and (alist-get 'id l)
                                                        (alist-get 'name l))) 
                                                 (alist-get 'licenses content))))
                      (setq org-license--spdx-data (alist-get 'licenses content))
                      (make-directory (file-name-directory org-license-cache-file) t)
                      (with-temp-file org-license-cache-file
                        (insert (json-encode org-license--spdx-data))))))
              (error "Échec de la connexion à l'API SPDX"))))
      (error 
       (message "Erreur SPDX: %s - Utilisation du cache" (error-message-string err))
       (setq org-license--spdx-data (ignore-errors 
                                      (json-read-file org-license-cache-file)))))))

(defun org-license--validate-spdx-id (spdx-id)
  (unless (cl-find-if (lambda (license) 
                        (string= (alist-get 'id license) spdx-id))
                      org-license--spdx-data)
    (error "Identifiant SPDX invalide: %s" spdx-id)))

(defun org-license--resolve-composite (spdx-expr)
  (mapconcat
   (lambda (part)
     (if-let ((template (org-license--resolve-template part)))
         template
       (error "Licence non supportée: %s" part)))
   (split-string spdx-expr "\\s-+\\(AND\\|OR\\)\\s-+")
   "\n\n"))

(defun org-license-insert-header ()
  (when (and org-license-mode
             (not (org-license--header-exists-p)))
    (let ((header (org-license-generate-header)))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^#\\+end_export$" nil t)
            (insert "\n\n" header)
          (goto-char (point-max))
          (insert "\n" header))))))

(defun org-license--header-exists-p ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (regexp-quote (org-license-generate-header)) nil t)))

(require 'ert)

(ert-deftest test-license-metadata-extraction ()
  (with-temp-buffer
    (insert "#+author: Test User\n#+date: 2024\n#+license: MIT")
    (org-mode)
    (should (equal (plist-get (org-license-get-metadata) :author) "Test User")))

  (ert-deftest test-spdx-validation ()
    (should-error (org-license--validate-spdx-id "INVALID-LICENSE")))

  (add-hook 'org-mode-hook
            (lambda () (when (locate-dominating-file default-directory ".git")
                         (org-license-mode 1))))

  (defun org-license-refresh-cache ()
    (interactive)
    (setq org-license--spdx-data nil)
    (org-license--fetch-spdx-data t)
    (message "Cache SPDX actualisé avec succès!"))

  (defvar org-license-log-buffer "*org-license-log*")

  (defun org-license--log (message &rest args)
    (when (get-buffer org-license-log-buffer)
      (with-current-buffer (get-buffer-create org-license-log-buffer)
        (insert (format "[%s] %s\n" 
                        (format-time-string "%Y-%m-%d %H:%M") 
                        (apply #'format message args)))))))

(provide 'org-license)

