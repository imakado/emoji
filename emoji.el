;;; emoji.el - show emoji on Emacs, complete emoji with anythig.

;; Copyright (c) 2009 by KAYAC Inc.

;; Author: IMAKADO <ken.imakado@gmail.com>
;; Keywords: cellphone,japanese

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Prefix: emoji-

;;; Installation

;; If you can read japanese,
;; see tomoya's great entry for more infomation
;; http://d.hatena.ne.jp/tomoya/20090706/1246874191


;; 1. checkout from github
;; git clone git://github.com/imakado/emoji.git /path/to/emoji

;; 2. set load-path and require(load) library.
;; (add-to-list 'load-path  "/path/to/emoji")
;; (require 'emoji)

;; Commands:
;; emoji completion
;; `emoji-complete-pictogram'

;; show emoji(pictogram)
;; `emoji-show-docomo'
;; `emoji-show-ezweb'
;; `emoji-show-softbank'

;; `emoji-replace-image-to-unicode'

(require 'cl)
(require 'anything)

(defvar emoji-start "&#x")
(make-variable-buffer-local 'emoji-start)
(defvar emoji-end ";")
(make-variable-buffer-local 'emoji-end)

(defun emoji-convert-from-docomo-unihex (carrier unihex)
  (assoc-default carrier
                 (assoc-default unihex emoji-unihex-convert-map)))

(defun emoji-get-image-dir (E-or-I-or-V)
  (expand-file-name
   (concat (file-name-directory (locate-library "emoji")) "image/" E-or-I-or-V "/")))

(defun emoji-filter (regexp callback)
  (save-excursion
    (loop initially (goto-char (point-min))
          while (re-search-forward regexp nil t)
          do (ignore-errors (funcall callback)))))

(defun emoji-docomo-unicode-regexp ()
  (concat (regexp-quote emoji-start)
          "\\(" "[a-zA-Z0-9]\\{4\\}" "\\)"
          (regexp-quote emoji-end)))

(defun emoji-get-carrier-emoji-img (carrier-dirname s)
  (create-image
   (concat (emoji-get-image-dir carrier-dirname) s ".gif")))

(defun emoji-get-emoji-image (carrier unihex)
  "return image"
  (let ((carrier-dirname (cond
                          ((string= carrier "ezweb") "E")
                          ((string= carrier "softbank") "V")
                          ((string= carrier "docomo") "I")
                          (t (error "unknown carrier %s" carrier)))))
    (emoji-get-carrier-emoji-img carrier-dirname unihex)))

(defun emoji-display-images-buffer (carrier)
  (emoji-filter
   (emoji-docomo-unicode-regexp)
   (lambda ()
    (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (let* ((unihex (emoji-convert-from-docomo-unihex carrier (match-string 1)))
                    (img (emoji-get-emoji-image carrier unihex)))
               (when (and img start end)
                 (put-text-property start end 'display img)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pictogram Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emoji-get-carrier-unihex-image-map (carrier-dirname)
  (let ((images
         (directory-files (emoji-get-image-dir carrier-dirname) t "^[^.]" t)))
    (loop for img in images
          for unihex = (file-name-sans-extension (file-name-nondirectory img))
          collect `(,unihex . ,(create-image img 'gif)))))

(defvar emoji-docomo-unihex-image-map
  (emoji-get-carrier-unihex-image-map "I"))

(defvar emoji-ezweb-unihex-image-map
  (emoji-get-carrier-unihex-image-map "E"))

(defvar emoji-softbank-unihex-image-map
  (emoji-get-carrier-unihex-image-map "V"))

;; unihex docomo ez soft name
;; enter -> (concat emoji-start unihex emoji-end)
(defun emoji-make-cands-string ()
  (let ((unihex-list (mapcar 'car emoji-docomo-unihex-image-map))
        (make-img-str (lambda (s image)
                        (put-text-property 0 (length s) 'display image s)
                        s)))
    (loop for unihex in unihex-list
          for docomo-image-str = (funcall make-img-str " " (assoc-default unihex emoji-docomo-unihex-image-map))
          for ezweb-image-str = (funcall make-img-str " " (assoc-default (emoji-convert-from-docomo-unihex "ezweb" unihex)
                                                                         emoji-ezweb-unihex-image-map))
          for softbank-image-str = (funcall make-img-str " " (assoc-default (emoji-convert-from-docomo-unihex "softbank" unihex)
                                                                            emoji-softbank-unihex-image-map))
          for title = (assoc-default unihex emoji-description-map)
          collect (mapconcat 'identity (list unihex docomo-image-str ezweb-image-str softbank-image-str title) " "))))

(defvar anything-c-source-insert-docomo-unihex
  `((name . "Pictogram")
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (insert (mapconcat 'identity (emoji-make-cands-string) "\n")))))
    (candidates-in-buffer)
    (get-line . (lambda (start end)
                  (buffer-substring start end)))
    (display-to-real .  (lambda (c)
                          (replace-regexp-in-string "^[A-Z0-9]+\\(.*\\)$" "" c nil nil 1)))
    (action . (("Insert" .
                (lambda (c)
                  (insert emoji-start c emoji-end)))))
    (migemo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emoji-show-docomo ()
  (interactive)
  (emoji-display-images-buffer "docomo"))

(defun emoji-show-ezweb ()
  (interactive)
  (emoji-display-images-buffer "ezweb"))

(defun emoji-show-softbank ()
  (interactive)
  (emoji-display-images-buffer "softbank"))

(defun emoji-replace-image-to-unicode ()
  (interactive)
  (emoji-filter
   (emoji-docomo-unicode-regexp)
   (lambda ()
    (let* ((start (match-beginning 0))
           (end (match-end 0)))
       (put-text-property start end 'display 'string)))))

(defun emoji-complete-pictogram ()
  (interactive)
  (anything 'anything-c-source-insert-docomo-unihex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convert map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emoji-convert-from-docomo-unihex-script (carrier unihex)
  (let ((encoding (concat "x-utf8-" carrier)))
  (shell-command-to-string
   (concat
    "perl -MEncode -MEncode::JP::Mobile -e \"printf '%X', unpack 'U*', decode '"
    encoding
    "', encode('"
    encoding
    "', chr ( hex '"
    unihex
    "' ));\""))))

;; generated by follows.

;; (defvar emoji-unihex-convert-map nil)
;; (let ((ret nil)
;;       (docomo-unihex-list (mapcar
;;                     (lambda (s) (replace-regexp-in-string "\\..*$" "" s))
;;                     (directory-files (emoji-get-image-dir "I")))))
;;   (dolist (docomo-unihex docomo-unihex-list ret)
;;     (let ((softbank-unihex (emoji-convert-from-docomo-unihex-script "softbank" docomo-unihex))
;;           (ezweb-unihex (emoji-convert-from-docomo-unihex-script "ezweb" docomo-unihex)))
;;       (add-to-list 'emoji-unihex-convert-map
;;                    `(,docomo-unihex . (("docomo" . ,docomo-unihex)
;;                                        ("ezweb" . ,ezweb-unihex)
;;                                        ("softbank" . ,softbank-unihex)))))))
;; (require 'pp)
;; (insert "(defvar emoji-unihex-convert-map\n" "'"(pp-to-string emoji-unihex-convert-map) ")")


(defvar emoji-unihex-convert-map
'(("E757"
  ("docomo" . "E757")
  ("ezweb" . "F0F5")
  ("softbank" . "E107"))
 ("E756"
  ("docomo" . "E756")
  ("ezweb" . "EF9A")
  ("softbank" . "E044"))
 ("E755"
  ("docomo" . "E755")
  ("ezweb" . "EFB7")
  ("softbank" . "E10B"))
 ("E754"
  ("docomo" . "E754")
  ("ezweb" . "EFB1")
  ("softbank" . "E01A"))
 ("E753"
  ("docomo" . "E753")
  ("ezweb" . "ED85")
  ("softbank" . "E404"))
 ("E752"
  ("docomo" . "E752")
  ("ezweb" . "ECA1")
  ("softbank" . "E056"))
 ("E751"
  ("docomo" . "E751")
  ("ezweb" . "EF72")
  ("softbank" . "E019"))
 ("E750"
  ("docomo" . "E750")
  ("ezweb" . "EFB5")
  ("softbank" . "E055"))
 ("E74F"
  ("docomo" . "E74F")
  ("ezweb" . "EFB9")
  ("softbank" . "E523"))
 ("E74E"
  ("docomo" . "E74E")
  ("ezweb" . "ED83")
  ("softbank" . "3F"))
 ("E74D"
  ("docomo" . "E74D")
  ("ezweb" . "EC83")
  ("softbank" . "E339"))
 ("E74C"
  ("docomo" . "E74C")
  ("ezweb" . "F0D1")
  ("softbank" . "E340"))
 ("E74B"
  ("docomo" . "E74B")
  ("ezweb" . "EC6A")
  ("softbank" . "E30B"))
 ("E74A"
  ("docomo" . "E74A")
  ("ezweb" . "EFA9")
  ("softbank" . "E046"))
 ("E749"
  ("docomo" . "E749")
  ("ezweb" . "EFAE")
  ("softbank" . "E342"))
 ("E748"
  ("docomo" . "E748")
  ("ezweb" . "EFA3")
  ("softbank" . "E030"))
 ("E747"
  ("docomo" . "E747")
  ("ezweb" . "EFA7")
  ("softbank" . "E118"))
 ("E746"
  ("docomo" . "E746")
  ("ezweb" . "ED82")
  ("softbank" . "E110"))
 ("E745"
  ("docomo" . "E745")
  ("ezweb" . "EC8D")
  ("softbank" . "E345"))
 ("E744"
  ("docomo" . "E744")
  ("ezweb" . "ECF6")
  ("softbank" . "3F"))
 ("E743"
  ("docomo" . "E743")
  ("ezweb" . "EFBD")
  ("softbank" . "E304"))
 ("E742"
  ("docomo" . "E742")
  ("ezweb" . "EFAB")
  ("softbank" . "3F"))
 ("E741"
  ("docomo" . "E741")
  ("ezweb" . "EFEC")
  ("softbank" . "E110"))
 ("E740"
  ("docomo" . "E740")
  ("ezweb" . "F0ED")
  ("softbank" . "E03B"))
 ("E73F"
  ("docomo" . "E73F")
  ("ezweb" . "ED81")
  ("softbank" . "E43E"))
 ("E73E"
  ("docomo" . "E73E")
  ("ezweb" . "EC53")
  ("softbank" . "E157"))
 ("E73D"
  ("docomo" . "E73D")
  ("ezweb" . "ED80")
  ("softbank" . "3F"))
 ("E73C"
  ("docomo" . "E73C")
  ("ezweb" . "ED7E")
  ("softbank" . "3F"))
 ("E73B"
  ("docomo" . "E73B")
  ("ezweb" . "EC5C")
  ("softbank" . "E22A"))
 ("E73A"
  ("docomo" . "E73A")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E739"
  ("docomo" . "E739")
  ("ezweb" . "EC5D")
  ("softbank" . "E22B"))
 ("E738"
  ("docomo" . "E738")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E737"
  ("docomo" . "E737")
  ("ezweb" . "EF59")
  ("softbank" . "E252"))
 ("E736"
  ("docomo" . "E736")
  ("ezweb" . "F075")
  ("softbank" . "E24F"))
 ("E735"
  ("docomo" . "E735")
  ("ezweb" . "ED7D")
  ("softbank" . "3F"))
 ("E734"
  ("docomo" . "E734")
  ("ezweb" . "EFCA")
  ("softbank" . "E315"))
 ("E733"
  ("docomo" . "E733")
  ("ezweb" . "EF43")
  ("softbank" . "E115"))
 ("E732"
  ("docomo" . "E732")
  ("ezweb" . "F06A")
  ("softbank" . "E537"))
 ("E731"
  ("docomo" . "E731")
  ("ezweb" . "F074")
  ("softbank" . "E24E"))
 ("E730"
  ("docomo" . "E730")
  ("ezweb" . "EF78")
  ("softbank" . "3F"))
 ("E72F"
  ("docomo" . "E72F")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E72E"
  ("docomo" . "E72E")
  ("ezweb" . "ED6D")
  ("softbank" . "E413"))
 ("E72D"
  ("docomo" . "E72D")
  ("ezweb" . "EF4B")
  ("softbank" . "E411"))
 ("E72C"
  ("docomo" . "E72C")
  ("ezweb" . "EC93")
  ("softbank" . "E402"))
 ("E72B"
  ("docomo" . "E72B")
  ("ezweb" . "EC96")
  ("softbank" . "E406"))
 ("E72A"
  ("docomo" . "E72A")
  ("ezweb" . "EC99")
  ("softbank" . "E40A"))
 ("E729"
  ("docomo" . "E729")
  ("ezweb" . "F0F3")
  ("softbank" . "E405"))
 ("E728"
  ("docomo" . "E728")
  ("ezweb" . "EFC0")
  ("softbank" . "E105"))
 ("E727"
  ("docomo" . "E727")
  ("ezweb" . "EFD2")
  ("softbank" . "E00E"))
 ("E726"
  ("docomo" . "E726")
  ("ezweb" . "F0F4")
  ("softbank" . "E106"))
 ("E725"
  ("docomo" . "E725")
  ("ezweb" . "EC9D")
  ("softbank" . "E40E"))
 ("E724"
  ("docomo" . "E724")
  ("ezweb" . "ED61")
  ("softbank" . "E416"))
 ("E723"
  ("docomo" . "E723")
  ("ezweb" . "F0F6")
  ("softbank" . "E108"))
 ("E722"
  ("docomo" . "E722")
  ("ezweb" . "EF49")
  ("softbank" . "E415"))
 ("E721"
  ("docomo" . "E721")
  ("ezweb" . "EC99")
  ("softbank" . "E40A"))
 ("E720"
  ("docomo" . "E720")
  ("ezweb" . "EC94")
  ("softbank" . "E403"))
 ("E71F"
  ("docomo" . "E71F")
  ("ezweb" . "F097")
  ("softbank" . "3F"))
 ("E71E"
  ("docomo" . "E71E")
  ("ezweb" . "EC82")
  ("softbank" . "E338"))
 ("E71D"
  ("docomo" . "E71D")
  ("ezweb" . "EF87")
  ("softbank" . "E136"))
 ("E71C"
  ("docomo" . "E71C")
  ("ezweb" . "EF54")
  ("softbank" . "3F"))
 ("E71B"
  ("docomo" . "E71B")
  ("ezweb" . "EFED")
  ("softbank" . "E034"))
 ("E71A"
  ("docomo" . "E71A")
  ("ezweb" . "F0F9")
  ("softbank" . "E10E"))
 ("E719"
  ("docomo" . "E719")
  ("ezweb" . "EF79")
  ("softbank" . "E301"))
 ("E718"
  ("docomo" . "E718")
  ("ezweb" . "F0A4")
  ("softbank" . "3F"))
 ("E717"
  ("docomo" . "E717")
  ("ezweb" . "ED7C")
  ("softbank" . "E103"))
 ("E716"
  ("docomo" . "E716")
  ("ezweb" . "F0E8")
  ("softbank" . "E00C"))
 ("E715"
  ("docomo" . "E715")
  ("ezweb" . "EFA0")
  ("softbank" . "E12F"))
 ("E714"
  ("docomo" . "E714")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E713"
  ("docomo" . "E713")
  ("ezweb" . "EFEB")
  ("softbank" . "E325"))
 ("E712"
  ("docomo" . "E712")
  ("ezweb" . "EF91")
  ("softbank" . "3F"))
 ("E711"
  ("docomo" . "E711")
  ("ezweb" . "ED7B")
  ("softbank" . "3F"))
 ("E710"
  ("docomo" . "E710")
  ("ezweb" . "EFE2")
  ("softbank" . "E31C"))
 ("E70F"
  ("docomo" . "E70F")
  ("ezweb" . "EFDD")
  ("softbank" . "3F"))
 ("E70E"
  ("docomo" . "E70E")
  ("ezweb" . "F0E6")
  ("softbank" . "E006"))
 ("E70D"
  ("docomo" . "E70D")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E70C"
  ("docomo" . "E70C")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E70B"
  ("docomo" . "E70B")
  ("ezweb" . "F0CA")
  ("softbank" . "E24D"))
 ("E70A"
  ("docomo" . "E70A")
  ("ezweb" . "ECF2")
  ("softbank" . "3F"))
 ("E709"
  ("docomo" . "E709")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E708"
  ("docomo" . "E708")
  ("ezweb" . "EFCD")
  ("softbank" . "E330"))
 ("E707"
  ("docomo" . "E707")
  ("ezweb" . "EFBF")
  ("softbank" . "E331"))
 ("E706"
  ("docomo" . "E706")
  ("ezweb" . "F0CE")
  ("softbank" . "E331"))
 ("E705"
  ("docomo" . "E705")
  ("ezweb" . "F0CD")
  ("softbank" . "3F"))
 ("E704"
  ("docomo" . "E704")
  ("ezweb" . "ECF1")
  ("softbank" . "3F"))
 ("E703"
  ("docomo" . "E703")
  ("ezweb" . "ECF0")
  ("softbank" . "3F"))
 ("E702"
  ("docomo" . "E702")
  ("ezweb" . "EF5A")
  ("softbank" . "E021"))
 ("E701"
  ("docomo" . "E701")
  ("ezweb" . "EF4D")
  ("softbank" . "E13C"))
 ("E700"
  ("docomo" . "E700")
  ("ezweb" . "ECEF")
  ("softbank" . "E238"))
 ("E6FF"
  ("docomo" . "E6FF")
  ("ezweb" . "EFDE")
  ("softbank" . "E326"))
 ("E6FE"
  ("docomo" . "E6FE")
  ("ezweb" . "EF52")
  ("softbank" . "E311"))
 ("E6FD"
  ("docomo" . "E6FD")
  ("ezweb" . "EFCC")
  ("softbank" . "E00D"))
 ("E6FC"
  ("docomo" . "E6FC")
  ("ezweb" . "EFBE")
  ("softbank" . "E334"))
 ("E6FB"
  ("docomo" . "E6FB")
  ("ezweb" . "EF4E")
  ("softbank" . "E10F"))
 ("E6FA"
  ("docomo" . "E6FA")
  ("ezweb" . "EC7E")
  ("softbank" . "E32E"))
 ("E6F9"
  ("docomo" . "E6F9")
  ("ezweb" . "EFC4")
  ("softbank" . "E003"))
 ("E6F8"
  ("docomo" . "E6F8")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6F7"
  ("docomo" . "E6F7")
  ("ezweb" . "EF95")
  ("softbank" . "E123"))
 ("E6F6"
  ("docomo" . "E6F6")
  ("ezweb" . "F0EE")
  ("softbank" . "E03E"))
 ("E6F5"
  ("docomo" . "E6F5")
  ("ezweb" . "ECEE")
  ("softbank" . "E236"))
 ("E6F4"
  ("docomo" . "E6F4")
  ("ezweb" . "F0CB")
  ("softbank" . "E406"))
 ("E6F3"
  ("docomo" . "E6F3")
  ("ezweb" . "EC97")
  ("softbank" . "E407"))
 ("E6F2"
  ("docomo" . "E6F2")
  ("ezweb" . "EC94")
  ("softbank" . "E058"))
 ("E6F1"
  ("docomo" . "E6F1")
  ("ezweb" . "EF4A")
  ("softbank" . "E059"))
 ("E6F0"
  ("docomo" . "E6F0")
  ("ezweb" . "EF49")
  ("softbank" . "E057"))
 ("E6EF"
  ("docomo" . "E6EF")
  ("ezweb" . "EF50")
  ("softbank" . "E327"))
 ("E6EE"
  ("docomo" . "E6EE")
  ("ezweb" . "EF4F")
  ("softbank" . "E023"))
 ("E6ED"
  ("docomo" . "E6ED")
  ("ezweb" . "ED79")
  ("softbank" . "E327"))
 ("E6EC"
  ("docomo" . "E6EC")
  ("ezweb" . "F0B2")
  ("softbank" . "E022"))
 ("E6EB"
  ("docomo" . "E6EB")
  ("ezweb" . "F0C9")
  ("softbank" . "E225"))
 ("E6EA"
  ("docomo" . "E6EA")
  ("ezweb" . "F046")
  ("softbank" . "E224"))
 ("E6E9"
  ("docomo" . "E6E9")
  ("ezweb" . "F045")
  ("softbank" . "E223"))
 ("E6E8"
  ("docomo" . "E6E8")
  ("ezweb" . "F044")
  ("softbank" . "E222"))
 ("E6E7"
  ("docomo" . "E6E7")
  ("ezweb" . "F043")
  ("softbank" . "E221"))
 ("E6E6"
  ("docomo" . "E6E6")
  ("ezweb" . "F042")
  ("softbank" . "E220"))
 ("E6E5"
  ("docomo" . "E6E5")
  ("ezweb" . "F041")
  ("softbank" . "E21F"))
 ("E6E4"
  ("docomo" . "E6E4")
  ("ezweb" . "F040")
  ("softbank" . "E21E"))
 ("E6E3"
  ("docomo" . "E6E3")
  ("ezweb" . "EFFC")
  ("softbank" . "E21D"))
 ("E6E2"
  ("docomo" . "E6E2")
  ("ezweb" . "EFFB")
  ("softbank" . "E21C"))
 ("E6E1"
  ("docomo" . "E6E1")
  ("ezweb" . "F048")
  ("softbank" . "3F"))
 ("E6E0"
  ("docomo" . "E6E0")
  ("ezweb" . "ED89")
  ("softbank" . "E210"))
 ("E6DF"
  ("docomo" . "E6DF")
  ("ezweb" . "3F")
  ("softbank" . "E211"))
 ("E6DE"
  ("docomo" . "E6DE")
  ("ezweb" . "ECED")
  ("softbank" . "3F"))
 ("E6DD"
  ("docomo" . "E6DD")
  ("ezweb" . "F0E5")
  ("softbank" . "E212"))
 ("E6DC"
  ("docomo" . "E6DC")
  ("ezweb" . "EFF1")
  ("softbank" . "E114"))
 ("E6DB"
  ("docomo" . "E6DB")
  ("ezweb" . "F0C8")
  ("softbank" . "3F"))
 ("E6DA"
  ("docomo" . "E6DA")
  ("ezweb" . "F079")
  ("softbank" . "3F"))
 ("E6D9"
  ("docomo" . "E6D9")
  ("ezweb" . "EFF2")
  ("softbank" . "E03F"))
 ("E6D8"
  ("docomo" . "E6D8")
  ("ezweb" . "EC5B")
  ("softbank" . "E229"))
 ("E6D7"
  ("docomo" . "E6D7")
  ("ezweb" . "F095")
  ("softbank" . "3F"))
 ("E6D6"
  ("docomo" . "E6D6")
  ("ezweb" . "F09A")
  ("softbank" . "3F"))
 ("E6D5"
  ("docomo" . "E6D5")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6D4"
  ("docomo" . "E6D4")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6D3"
  ("docomo" . "E6D3")
  ("ezweb" . "EFFA")
  ("softbank" . "E103"))
 ("E6D2"
  ("docomo" . "E6D2")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6D1"
  ("docomo" . "E6D1")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6D0"
  ("docomo" . "E6D0")
  ("ezweb" . "EFF9")
  ("softbank" . "E00B"))
 ("E6CF"
  ("docomo" . "E6CF")
  ("ezweb" . "ED66")
  ("softbank" . "E103"))
 ("E6CE"
  ("docomo" . "E6CE")
  ("ezweb" . "F0DF")
  ("softbank" . "E104"))
 ("E6BA"
  ("docomo" . "E6BA")
  ("ezweb" . "F0B1")
  ("softbank" . "E02D"))
 ("E6B9"
  ("docomo" . "E6B9")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6B8"
  ("docomo" . "E6B8")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6B7"
  ("docomo" . "E6B7")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6B3"
  ("docomo" . "E6B3")
  ("ezweb" . "ECC5")
  ("softbank" . "E44B"))
 ("E6B2"
  ("docomo" . "E6B2")
  ("ezweb" . "3F")
  ("softbank" . "E11F"))
 ("E6B1"
  ("docomo" . "E6B1")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6AE"
  ("docomo" . "E6AE")
  ("ezweb" . "F0DA")
  ("softbank" . "3F"))
 ("E6AD"
  ("docomo" . "E6AD")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E6AC"
  ("docomo" . "E6AC")
  ("ezweb" . "EF97")
  ("softbank" . "E324"))
 ("E6A5"
  ("docomo" . "E6A5")
  ("ezweb" . "F072")
  ("softbank" . "E239"))
 ("E6A4"
  ("docomo" . "E6A4")
  ("ezweb" . "EFA2")
  ("softbank" . "E033"))
 ("E6A3"
  ("docomo" . "E6A3")
  ("ezweb" . "EF8D")
  ("softbank" . "E01C"))
 ("E6A2"
  ("docomo" . "E6A2")
  ("ezweb" . "EFB4")
  ("softbank" . "E04F"))
 ("E6A1"
  ("docomo" . "E6A1")
  ("ezweb" . "EFBA")
  ("softbank" . "E052"))
 ("E6A0"
  ("docomo" . "E6A0")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E69F"
  ("docomo" . "E69F")
  ("ezweb" . "EF5E")
  ("softbank" . "E04C"))
 ("E69E"
  ("docomo" . "E69E")
  ("ezweb" . "F0C7")
  ("softbank" . "E04C"))
 ("E69D"
  ("docomo" . "E69D")
  ("ezweb" . "F0C6")
  ("softbank" . "E04C"))
 ("E69C"
  ("docomo" . "E69C")
  ("ezweb" . "F0C5")
  ("softbank" . "3F"))
 ("E69B"
  ("docomo" . "E69B")
  ("ezweb" . "EF57")
  ("softbank" . "E20A"))
 ("E69A"
  ("docomo" . "E69A")
  ("ezweb" . "EFD7")
  ("softbank" . "3F"))
 ("E699"
  ("docomo" . "E699")
  ("ezweb" . "ECEC")
  ("softbank" . "E007"))
 ("E698"
  ("docomo" . "E698")
  ("ezweb" . "ECEB")
  ("softbank" . "E536"))
 ("E697"
  ("docomo" . "E697")
  ("ezweb" . "F068")
  ("softbank" . "E237"))
 ("E696"
  ("docomo" . "E696")
  ("ezweb" . "F069")
  ("softbank" . "E238"))
 ("E695"
  ("docomo" . "E695")
  ("ezweb" . "F0C4")
  ("softbank" . "E012"))
 ("E694"
  ("docomo" . "E694")
  ("ezweb" . "F0C3")
  ("softbank" . "E011"))
 ("E693"
  ("docomo" . "E693")
  ("ezweb" . "ED88")
  ("softbank" . "E010"))
 ("E692"
  ("docomo" . "E692")
  ("ezweb" . "F0C2")
  ("softbank" . "E41B"))
 ("E691"
  ("docomo" . "E691")
  ("ezweb" . "F0C1")
  ("softbank" . "E419"))
 ("E690"
  ("docomo" . "E690")
  ("ezweb" . "F0C0")
  ("softbank" . "E20F"))
 ("E68F"
  ("docomo" . "E68F")
  ("ezweb" . "F0BF")
  ("softbank" . "E20D"))
 ("E68E"
  ("docomo" . "E68E")
  ("ezweb" . "F0BE")
  ("softbank" . "E20E"))
 ("E68D"
  ("docomo" . "E68D")
  ("ezweb" . "EC78")
  ("softbank" . "E20C"))
 ("E68C"
  ("docomo" . "E68C")
  ("ezweb" . "EFE5")
  ("softbank" . "E126"))
 ("E68B"
  ("docomo" . "E68B")
  ("ezweb" . "EF9F")
  ("softbank" . "3F"))
 ("E68A"
  ("docomo" . "E68A")
  ("ezweb" . "EFDB")
  ("softbank" . "E12A"))
 ("E689"
  ("docomo" . "E689")
  ("ezweb" . "EC65")
  ("softbank" . "E301"))
 ("E688"
  ("docomo" . "E688")
  ("ezweb" . "F0A5")
  ("softbank" . "E00A"))
 ("E687"
  ("docomo" . "E687")
  ("ezweb" . "F0B3")
  ("softbank" . "E009"))
 ("E686"
  ("docomo" . "E686")
  ("ezweb" . "F0BD")
  ("softbank" . "E34B"))
 ("E685"
  ("docomo" . "E685")
  ("ezweb" . "EFA8")
  ("softbank" . "E112"))
 ("E684"
  ("docomo" . "E684")
  ("ezweb" . "F0BC")
  ("softbank" . "E314"))
 ("E683"
  ("docomo" . "E683")
  ("ezweb" . "EF77")
  ("softbank" . "E148"))
 ("E682"
  ("docomo" . "E682")
  ("ezweb" . "EF74")
  ("softbank" . "E323"))
 ("E681"
  ("docomo" . "E681")
  ("ezweb" . "EFEE")
  ("softbank" . "E008"))
 ("E680"
  ("docomo" . "E680")
  ("ezweb" . "EF56")
  ("softbank" . "E208"))
 ("E67F"
  ("docomo" . "E67F")
  ("ezweb" . "EF55")
  ("softbank" . "E30E"))
 ("E67E"
  ("docomo" . "E67E")
  ("ezweb" . "EF76")
  ("softbank" . "E125"))
 ("E67D"
  ("docomo" . "E67D")
  ("ezweb" . "F0BB")
  ("softbank" . "3F"))
 ("E67C"
  ("docomo" . "E67C")
  ("ezweb" . "ECC9")
  ("softbank" . "E503"))
 ("E67B"
  ("docomo" . "E67B")
  ("ezweb" . "F0B9")
  ("softbank" . "E502"))
 ("E67A"
  ("docomo" . "E67A")
  ("ezweb" . "EFE1")
  ("softbank" . "E30A"))
 ("E679"
  ("docomo" . "E679")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E678"
  ("docomo" . "E678")
  ("ezweb" . "F071")
  ("softbank" . "E236"))
 ("E677"
  ("docomo" . "E677")
  ("ezweb" . "EFF0")
  ("softbank" . "E03D"))
 ("E676"
  ("docomo" . "E676")
  ("ezweb" . "EFDC")
  ("softbank" . "E03C"))
 ("E675"
  ("docomo" . "E675")
  ("ezweb" . "EFEF")
  ("softbank" . "E313"))
 ("E674"
  ("docomo" . "E674")
  ("ezweb" . "EFF3")
  ("softbank" . "E13E"))
 ("E673"
  ("docomo" . "E673")
  ("ezweb" . "EFAF")
  ("softbank" . "E120"))
 ("E672"
  ("docomo" . "E672")
  ("ezweb" . "EF9C")
  ("softbank" . "E047"))
 ("E671"
  ("docomo" . "E671")
  ("ezweb" . "EF9B")
  ("softbank" . "E044"))
 ("E670"
  ("docomo" . "E670")
  ("ezweb" . "F0B4")
  ("softbank" . "E045"))
 ("E66F"
  ("docomo" . "E66F")
  ("ezweb" . "EF85")
  ("softbank" . "E043"))
 ("E66E"
  ("docomo" . "E66E")
  ("ezweb" . "EF7D")
  ("softbank" . "E151"))
 ("E66D"
  ("docomo" . "E66D")
  ("ezweb" . "EF42")
  ("softbank" . "E14E"))
 ("E66C"
  ("docomo" . "E66C")
  ("ezweb" . "EF7E")
  ("softbank" . "E14F"))
 ("E66B"
  ("docomo" . "E66B")
  ("ezweb" . "F08E")
  ("softbank" . "E03A"))
 ("E66A"
  ("docomo" . "E66A")
  ("ezweb" . "EF7C")
  ("softbank" . "E156"))
 ("E669"
  ("docomo" . "E669")
  ("ezweb" . "EC54")
  ("softbank" . "E158"))
 ("E668"
  ("docomo" . "E668")
  ("ezweb" . "EF7B")
  ("softbank" . "E154"))
 ("E667"
  ("docomo" . "E667")
  ("ezweb" . "EF83")
  ("softbank" . "E14D"))
 ("E666"
  ("docomo" . "E666")
  ("ezweb" . "EC52")
  ("softbank" . "E155"))
 ("E665"
  ("docomo" . "E665")
  ("ezweb" . "EC51")
  ("softbank" . "E153"))
 ("E664"
  ("docomo" . "E664")
  ("ezweb" . "EF86")
  ("softbank" . "E038"))
 ("E663"
  ("docomo" . "E663")
  ("ezweb" . "EF84")
  ("softbank" . "E036"))
 ("E662"
  ("docomo" . "E662")
  ("ezweb" . "EF8C")
  ("softbank" . "E01D"))
 ("E661"
  ("docomo" . "E661")
  ("ezweb" . "EC55")
  ("softbank" . "E202"))
 ("E660"
  ("docomo" . "E660")
  ("ezweb" . "EF88")
  ("softbank" . "E159"))
 ("E65F"
  ("docomo" . "E65F")
  ("ezweb" . "EF8A")
  ("softbank" . "E42E"))
 ("E65E"
  ("docomo" . "E65E")
  ("ezweb" . "EF8A")
  ("softbank" . "E01B"))
 ("E65D"
  ("docomo" . "E65D")
  ("ezweb" . "EF89")
  ("softbank" . "E435"))
 ("E65C"
  ("docomo" . "E65C")
  ("ezweb" . "F0EC")
  ("softbank" . "E434"))
 ("E65B"
  ("docomo" . "E65B")
  ("ezweb" . "EF8E")
  ("softbank" . "E01E"))
 ("E65A"
  ("docomo" . "E65A")
  ("ezweb" . "F0B8")
  ("softbank" . "3F"))
 ("E659"
  ("docomo" . "E659")
  ("ezweb" . "EF92")
  ("softbank" . "E132"))
 ("E658"
  ("docomo" . "E658")
  ("ezweb" . "F0B7")
  ("softbank" . "E42A"))
 ("E657"
  ("docomo" . "E657")
  ("ezweb" . "EC80")
  ("softbank" . "E013"))
 ("E656"
  ("docomo" . "E656")
  ("ezweb" . "EF8F")
  ("softbank" . "E018"))
 ("E655"
  ("docomo" . "E655")
  ("ezweb" . "EF90")
  ("softbank" . "E015"))
 ("E654"
  ("docomo" . "E654")
  ("ezweb" . "F0B6")
  ("softbank" . "E014"))
 ("E653"
  ("docomo" . "E653")
  ("ezweb" . "EF93")
  ("softbank" . "E016"))
 ("E652"
  ("docomo" . "E652")
  ("ezweb" . "3F")
  ("softbank" . "3F"))
 ("E651"
  ("docomo" . "E651")
  ("ezweb" . "EF72")
  ("softbank" . "E24A"))
 ("E650"
  ("docomo" . "E650")
  ("ezweb" . "EF71")
  ("softbank" . "E249"))
 ("E64F"
  ("docomo" . "E64F")
  ("ezweb" . "EF70")
  ("softbank" . "E248"))
 ("E64E"
  ("docomo" . "E64E")
  ("ezweb" . "EF6F")
  ("softbank" . "E247"))
 ("E64D"
  ("docomo" . "E64D")
  ("ezweb" . "EF6E")
  ("softbank" . "E246"))
 ("E64C"
  ("docomo" . "E64C")
  ("ezweb" . "EF6D")
  ("softbank" . "E245"))
 ("E64B"
  ("docomo" . "E64B")
  ("ezweb" . "EF6C")
  ("softbank" . "E244"))
 ("E64A"
  ("docomo" . "E64A")
  ("ezweb" . "EF6B")
  ("softbank" . "E243"))
 ("E649"
  ("docomo" . "E649")
  ("ezweb" . "EF6A")
  ("softbank" . "E242"))
 ("E648"
  ("docomo" . "E648")
  ("ezweb" . "EF69")
  ("softbank" . "E241"))
 ("E647"
  ("docomo" . "E647")
  ("ezweb" . "EF68")
  ("softbank" . "E240"))
 ("E646"
  ("docomo" . "E646")
  ("ezweb" . "EF67")
  ("softbank" . "E23F"))
 ("E645"
  ("docomo" . "E645")
  ("ezweb" . "ECBC")
  ("softbank" . "E43C"))
 ("E644"
  ("docomo" . "E644")
  ("ezweb" . "F0B5")
  ("softbank" . "3F"))
 ("E643"
  ("docomo" . "E643")
  ("ezweb" . "EF41")
  ("softbank" . "E443"))
 ("E642"
  ("docomo" . "E642")
  ("ezweb" . "EF5F")
  ("softbank" . "E13D"))
 ("E641"
  ("docomo" . "E641")
  ("ezweb" . "EF5D")
  ("softbank" . "E048"))
 ("E640"
  ("docomo" . "E640")
  ("ezweb" . "EF64")
  ("softbank" . "E04B"))
 ("E63F"
  ("docomo" . "E63F")
  ("ezweb" . "EF65")
  ("softbank" . "E049"))
 ("E63E"
  ("docomo" . "E63E")
  ("ezweb" . "EF60")
  ("softbank" . "E04A"))
 (""
  ("docomo" . "")
  ("ezweb" . "0")
  ("softbank" . "0")))
)

;; (defvar emoji-description-map)

;; (loop for (unihex title) in
;; (loop for s in (split-string 
;;                 (with-current-buffer "*perl output*"
;;                   (buffer-substring-no-properties (point-min) (point-max)))
;;                 "\n"
;;                 t)
;;       collect (split-string s "::" t))
;;collect `(,unihex . ,title))


(defvar emoji-description-map
  '(("E63E" . "晴れ")
    ("E63F" . "曇り")
    ("E640" . "雨")
    ("E641" . "雪")
    ("E642" . "雷")
    ("E643" . "台風")
    ("E644" . "霧")
    ("E645" . "小雨")
    ("E646" . "牡羊座")
    ("E647" . "牡牛座")
    ("E648" . "双子座")
    ("E649" . "蟹座")
    ("E64A" . "獅子座")
    ("E64B" . "乙女座")
    ("E64C" . "天秤座")
    ("E64D" . "蠍座")
    ("E64E" . "射手座")
    ("E64F" . "山羊座")
    ("E650" . "水瓶座")
    ("E651" . "魚座")
    ("E652" . "スポーツ")
    ("E653" . "野球")
    ("E654" . "ゴルフ")
    ("E655" . "テニス")
    ("E656" . "サッカー")
    ("E657" . "スキー")
    ("E658" . "バスケットボール")
    ("E659" . "モータースポーツ")
    ("E65A" . "ポケットベル")
    ("E65B" . "電車")
    ("E65C" . "地下鉄")
    ("E65D" . "新幹線")
    ("E65E" . "車（セダン）")
    ("E65F" . "車（ＲＶ）")
    ("E660" . "バス")
    ("E661" . "船")
    ("E662" . "飛行機")
    ("E663" . "家")
    ("E664" . "ビル")
    ("E665" . "郵便局")
    ("E666" . "病院")
    ("E667" . "銀行")
    ("E668" . "ＡＴＭ")
    ("E669" . "ホテル")
    ("E66A" . "コンビニ")
    ("E66B" . "ガソリンスタンド")
    ("E66C" . "駐車場")
    ("E66D" . "信号")
    ("E66E" . "トイレ")
    ("E66F" . "レストラン")
    ("E670" . "喫茶店")
    ("E671" . "バー")
    ("E672" . "ビール")
    ("E673" . "ファーストフード")
    ("E674" . "ブティック")
    ("E675" . "美容院")
    ("E676" . "カラオケ")
    ("E677" . "映画")
    ("E678" . "右斜め上")
    ("E679" . "遊園地")
    ("E67A" . "音楽")
    ("E67B" . "アート")
    ("E67C" . "演劇")
    ("E67D" . "イベント")
    ("E67E" . "チケット")
    ("E67F" . "喫煙")
    ("E680" . "禁煙")
    ("E681" . "カメラ")
    ("E682" . "カバン")
    ("E683" . "本")
    ("E684" . "リボン")
    ("E685" . "プレゼント")
    ("E686" . "バースデー")
    ("E687" . "電話")
    ("E688" . "携帯電話")
    ("E689" . "メモ")
    ("E68A" . "ＴＶ")
    ("E68B" . "ゲーム")
    ("E68C" . "ＣＤ")
    ("E68D" . "ハート")
    ("E68E" . "スペード")
    ("E68F" . "ダイヤ")
    ("E690" . "クラブ")
    ("E691" . "目")
    ("E692" . "耳")
    ("E693" . "手（グー）")
    ("E694" . "手（チョキ）")
    ("E695" . "手（パー）")
    ("E696" . "右斜め下")
    ("E697" . "左斜め上")
    ("E698" . "足")
    ("E699" . "くつ")
    ("E69A" . "眼鏡")
    ("E69B" . "車椅子")
    ("E69C" . "新月")
    ("E69D" . "やや欠け月")
    ("E69E" . "半月")
    ("E69F" . "三日月")
    ("E6A0" . "満月")
    ("E6A1" . "犬")
    ("E6A2" . "猫")
    ("E6A3" . "リゾート")
    ("E6A4" . "クリスマス")
    ("E6A5" . "左斜め下")
    ("E6CE" . "phoneto")
    ("E6CF" . "mailto")
    ("E6D0" . "faxto")
    ("E6D1" . "iモード")
    ("E6D2" . "iモード（枠付き）")
    ("E6D3" . "メール")
    ("E6D4" . "ドコモ提供")
    ("E6D5" . "ドコモポイント")
    ("E6D6" . "有料")
    ("E6D7" . "無料")
    ("E6D8" . "ID")
    ("E6D9" . "パスワード")
    ("E6DA" . "次項有")
    ("E6DB" . "クリア")
    ("E6DC" . "サーチ（調べる）")
    ("E6DD" . "ＮＥＷ")
    ("E6DE" . "位置情報")
    ("E6DF" . "フリーダイヤル")
    ("E6E0" . "シャープダイヤル")
    ("E6E1" . "モバＱ")
    ("E6E2" . "1")
    ("E6E3" . "2")
    ("E6E4" . "3")
    ("E6E5" . "4")
    ("E6E6" . "5")
    ("E6E7" . "6")
    ("E6E8" . "7")
    ("E6E9" . "8")
    ("E6EA" . "9")
    ("E6EB" . "0")
    ("E70B" . "決定")
    ("E6EC" . "黒ハート")
    ("E6ED" . "揺れるハート")
    ("E6EE" . "失恋")
    ("E6EF" . "ハートたち（複数ハート）")
    ("E6F0" . "わーい（嬉しい顔）")
    ("E6F1" . "ちっ（怒った顔）")
    ("E6F2" . "がく〜（落胆した顔）")
    ("E6F3" . "もうやだ〜（悲しい顔）")
    ("E6F4" . "ふらふら")
    ("E6F5" . "グッド（上向き矢印）")
    ("E6F6" . "るんるん")
    ("E6F7" . "いい気分（温泉）")
    ("E6F8" . "かわいい")
    ("E6F9" . "キスマーク")
    ("E6FA" . "ぴかぴか（新しい）")
    ("E6FB" . "ひらめき")
    ("E6FC" . "むかっ（怒り）")
    ("E6FD" . "パンチ")
    ("E6FE" . "爆弾")
    ("E6FF" . "ムード")
    ("E700" . "バッド（下向き矢印）")
    ("E701" . "眠い(睡眠)")
    ("E702" . "exclamation")
    ("E703" . "exclamation&question")
    ("E704" . "exclamation×2")
    ("E705" . "どんっ（衝撃）")
    ("E706" . "あせあせ（飛び散る汗）")
    ("E707" . "たらーっ（汗）")
    ("E708" . "ダッシュ（走り出すさま）")
    ("E709" . "ー（長音記号１）")
    ("E70A" . "ー（長音記号２）")
    ("E6AC" . "カチンコ")
    ("E6AD" . "ふくろ")
    ("E6AE" . "ペン")
    ("E6B1" . "人影")
    ("E6B2" . "いす")
    ("E6B3" . "夜")
    ("E6B7" . "soon")
    ("E6B8" . "on")
    ("E6B9" . "end")
    ("E6BA" . "時計")
    ("E70C" . "iアプリ")
    ("E70D" . "iアプリ（枠付き）")
    ("E70E" . "Tシャツ（ボーダー）")
    ("E70F" . "がま口財布")
    ("E710" . "化粧")
    ("E711" . "ジーンズ")
    ("E712" . "スノボ")
    ("E713" . "チャペル")
    ("E714" . "ドア")
    ("E715" . "ドル袋")
    ("E716" . "パソコン")
    ("E717" . "ラブレター")
    ("E718" . "レンチ")
    ("E719" . "鉛筆")
    ("E71A" . "王冠")
    ("E71B" . "指輪")
    ("E71C" . "砂時計")
    ("E71D" . "自転車")
    ("E71E" . "湯のみ")
    ("E71F" . "腕時計")
    ("E720" . "考えてる顔")
    ("E721" . "ほっとした顔")
    ("E722" . "冷や汗")
    ("E723" . "冷や汗2")
    ("E724" . "ぷっくっくな顔")
    ("E725" . "ボケーっとした顔")
    ("E726" . "目がハート")
    ("E727" . "指でOK")
    ("E728" . "あっかんべー")
    ("E729" . "ウィンク")
    ("E72A" . "うれしい顔")
    ("E72B" . "がまん顔")
    ("E72C" . "猫2")
    ("E72D" . "泣き顔")
    ("E72E" . "涙")
    ("E72F" . "NG")
    ("E730" . "クリップ")
    ("E731" . "コピーライト")
    ("E732" . "トレードマーク")
    ("E733" . "走る人")
    ("E734" . "マル秘")
    ("E735" . "リサイクル")
    ("E736" . "レジスタードトレードマーク")
    ("E737" . "危険・警告")
    ("E738" . "禁止")
    ("E739" . "空室・空席・空車")
    ("E73A" . "合格マーク")
    ("E73B" . "満室・満席・満車")
    ("E73C" . "矢印左右")
    ("E73D" . "矢印上下")
    ("E73E" . "学校")
    ("E73F" . "波")
    ("E740" . "富士山")
    ("E741" . "クローバー")
    ("E742" . "さくらんぼ")
    ("E743" . "チューリップ")
    ("E744" . "バナナ")
    ("E745" . "りんご")
    ("E746" . "芽")
    ("E747" . "もみじ")
    ("E748" . "桜")
    ("E749" . "おにぎり")
    ("E74A" . "ショートケーキ")
    ("E74B" . "とっくり（おちょこ付き）")
    ("E74C" . "どんぶり")
    ("E74D" . "パン")
    ("E74E" . "かたつむり")
    ("E74F" . "ひよこ")
    ("E750" . "ペンギン")
    ("E751" . "魚")
    ("E752" . "うまい！")
    ("E753" . "ウッシッシ")
    ("E754" . "ウマ")
    ("E755" . "ブタ")
    ("E756" . "ワイングラス")
    ("E757" . "げっそり")))

(provide 'emoji)
;; emoji.el ends here.
