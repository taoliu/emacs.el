;;; pubfind.el --- Client to search/get BibTeX entries in PubMed from
;;; the internet

;; Copyright (C) 2005,2006 by Tao Liu
;; Author: Tao Liu <vladimir.liu@gmail.com>
;; Created: Jan 18, 2005
;; Url: http://www.freewebs.com/liut/src/pubfind.el
;; RCS $Id: pubfind.el,v 1.2 2006/04/27 02:41:46 fred Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;; This packages provides an Emacs Lisp interface to
;; searching/obtaining BibTeX entry from the online source.
;; Currently concerned sources are:
;;
;; o NCBI entrez  <http://www.ncbi.nlm.nih.gov/entrez/>
;; o HubMed       <http://www.hubmed.org/>
;;
;; On invocation, the function pubfind-get-by-query prompt the user
;; for a journal, volume and page, then return the corresponding
;; BibTeX entry. While pubfind-get-by-id fetches the BibteX entry
;; according to a given pubmed id. The result will be a bibtex-mode
;; buffer with the results parsed out.

;;; Dependency:
;; If you are using emacs21, you should install w3-url package. This
;; package is included in emacs22 and xemacs21, so you need not to
;; install it if either of them is your current emacsen.

;;; Usage:
;;
;; byte-compile-file and put it in your load-path. Add this line in
;; your initialization file (e.g. ~/.emacs):
;;
;; (add-to-list 'load-path "/path/to/elisp/")
;; (load "pubfind.el")
;;
;; Then M-x pubfind-get-by-id/pubfind-get-by-query invokes the
;; interface.

;;; Acknowledge:
;; This work is inspired by Nevin Kapur's `bibfind.el'. which you can
;; download from here:
;; http://www.cs.caltech.edu/~nkapur/emacs/bibfind.el


;;; Code:

(require 'url)
(require 'bibtex)

(defconst pubfind-version "1.04"
  "pubfind version.")

(defvar pubfind-bibtex-buffer "*Pubfind bibTeX Results*"
  "Buffer for Pubfind bibTeX Results.") 

(defvar pubfind-use-font-lock t
  "Whether to fontify the results.")

(defconst pubfind-journal-sources 
  '("Science" "Cell"
    "Nature" "Nat Rev Genet" "Nat Genet" "Nat Biotech" 
    "Bioinformatics" "Nucl Acids Res" "Genome Res"
    "PNAS" "PLoS Biol" "PLoS Comput Biol" "BMC Bioinformatics"
    )
  "List of common journal abbreviations to search:
Science                              Science
Cell                                 Cell
Nature                               Nature
Nat Rev Genet                        Nature Review Genetics
Nat Genet                            Nature Genetics
Nat Biotechnol                       Nature Biotechnology
Bioinformatics                       Bioinformatics
BMC Bioinformatics                   BMC Bioinformatics
Nucl Acids Res                       Nucleic Acids Research
PNAS                                 Proceedings of the National Academy of Sciences of the United States of America
Genome Res                           Genome Research
PLoS Biol                            PLoS Biology
PLoS Comput Biol                     PLoS Computational Biology
")

;;;###autoload
(defun pubfind-get-by-id ()
  "Get bibTeX entry interactively.

User must give a pubmed id. Then this function will download the
bibtex entry from HubMed.
"
  (interactive)
  (let* ((pubmed-id (read-string "pubmed-id: "))
	 (completion-ignore-case t))
    (cond
     ((string-match "^[ \t]*$" pubmed-id)
      (error 
       "pubfind error: pubmed id must be given."))
     ((pubfind-get-run pubmed-id)))))
				 
;;;###autoload
(defun pubfind-get-by-query ()
  "Search then get entry interactively.

User must give the journal name, volume number and page number
for the article. This function will first query the PubMed for
pubmed id, then download the bibtex entry from HubMed.
"
  (interactive)
  (let* ((journal (completing-read "Journal (TAB for some completion): " 
				   (mapcar 'list pubfind-journal-sources)))
	 (volume  (read-string "Volume: "))
	 (page    (read-string "Page: "))
	 (completion-igore-case t))
    (cond 
     ((or 
       (string-match "^[ \t]*$" journal)
       (string-match "^[ \t]*$" volume)
       (string-match "^[ \t]*$" page))
      (error
       "pubsearch error: Neither journal, volume nor page should be blank."))
     ((pubfind-search-run journal volume page)))))

;; Talk to HubMed/PubMed.
(defun pubfind-get-run ( pubmed-id )
  "Run the HubMed client."
  (pop-to-buffer pubfind-bibtex-buffer)
  (erase-buffer)
  (if (string= pubmed-id "0")
      (insert "Sorry, no matches")
    (progn
      (pubfind-get-fetch pubmed-id)
      (pubfind-treat-buffer pubfind-bibtex-buffer))))

(defun pubfind-search-run ( journal volume page)
  "Run the NCBI search client."
  (pop-to-buffer pubfind-bibtex-buffer)
  (erase-buffer)
  (pubfind-search-query journal volume page)
  (pubfind-search-wash))

;; Clients
(defun pubfind-get-fetch ( pubmed-id )
  "Fetch data and print it in the current buffer."
  (let* ((url (concat
	      "http://www.hubmed.org/export/bibtex.cgi?"
	      "uids=" pubmed-id)))
    (url-insert-file-contents url)))

(defun pubfind-search-query ( journal volume page)
  "Query and print in the current buffer."
  (let* ((url (pubfind-format 
	       (concat
		"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmax=1&"
		"term="
		journal "[ta]"
		volume "[vi]"
		page "[pg]"))))
    (url-insert-file-contents url)))

;; Wash functions

(defun pubfind-search-wash ()
  "Wash the search result."
  (goto-char (point-min))
  (let ((case-fold-search t))
    ;; I only keep the first hit.
    (if (re-search-forward "<Id>\\([0-9]+\\)</Id>" nil t)
	(let* ((searched-id (match-string 1)))
	  (pubfind-get-run searched-id))
      (let* ((searched-id "0"))
	(pubfind-get-run searched-id)))))

;; Utility functions

(defun pubfind-format (url)
  "Replace ' ' with '%20'"
  (while (string-match "[ \t]+" url)
    (setq url (replace-match "%20" nil t url)))
  (eval url))
	 
(defun pubfind-treat-buffer (buffer)
  "Treat buffer with bib entries."
  (with-current-buffer buffer
    (bibtex-mode)
    (when pubfind-use-font-lock
      (font-lock-fontify-buffer))
    (goto-char (point-min))
    (setq buffer-file-name nil)))

;;; pubfind.el ends here
