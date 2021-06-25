;;; nearby.el --- Find nearby emacs user  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 LiuBo

;; Author: LiuBo <https://github.com/404cn>
;; Created: Jun 25, 2021
;; Version: 0.1.0
;; Homepage: https://github.com/404cn/nearby.el

;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;;; Find nearby emacs user

;;; Code:

(defgroup nearby nil
  "Find nearby people in emacs!"
  :group 'nearby)

(defcustom nearby-name nil
  "Your name showed in nearby mode."
  :group 'nearby
  :type 'string)

(defcustom nearby-latitude nil
  "Your latitude."
  :grouop 'nearby
  :type 'integer)

(defcustom nearby-longitude nil
  "Your longitude"
  :group 'nearby
  :type 'integer)

(defun nearby-buffer ()
  (get-buffer-create "*nearby*"))

(defun nearby-mode ()
  "Major mode for listing nearby people."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'nearby-mode
        mode-name "nearby"
        truncate-lines t
        buffer-read-only t)
  ;; TODO request to server for nearby uesr
  )

;;;###autoload
(defun nearby ()
  "Enter nearby."
  (interactive)
  (switch-to-buffer (nearby-buffer))
  (unless (eq major-mode 'nearby-mode)
    (nearby-mode)))

(provide 'nearby)
