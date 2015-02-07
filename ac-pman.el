;;; ac-java.el --- auto complete source for java import statements

;; Author: Matthias 
;; Version: 1.0.0
;; Keywords: java, classpath, pman, import
;; Package-Requires: ((auto-complete "1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;; Functions

(make-variable-buffer-local
 (defvar ac-classpath-cache nil
   "Cache for buffer local classpath."))

(defun ac-classpath ()
  (when (not ac-classpath-cache)
    (setq ac-classpath-cache (pman-expand-classpath pman-project)))
  ac-classpath-cache)

(defvar ac-source-classpath
  '((candidates . ac-classpath)
    (prefix . "^import \\(.*\\)")
    (symbol . "t")))

(provide 'ac-pman)

;;; ac-pman.el ends here
