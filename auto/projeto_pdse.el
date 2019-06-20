(TeX-add-style-hook
 "projeto_pdse"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("abntex2" "a4paper" "12pt" "openright" "oneside" "german" "french" "english" "brazil" "article")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "brazil") ("inputenc" "utf8") ("abntex2cite" "alf") ("geometry" "a4paper" "left=3cm" "right=2cm" "top=3cm" "bottom=2cm")))
   (TeX-run-style-hooks
    "latex2e"
    "abntex2"
    "abntex212"
    "babel"
    "graphicx"
    "inputenc"
    "wrapfig"
    "lscape"
    "rotating"
    "epstopdf"
    "abntex2cite"
    "geometry"
    "indentfirst"
    "longtable")
   (LaTeX-add-labels
    "cronograma")
   (LaTeX-add-bibliographies
    "BIBDOUTORADO"
    " abnt-options"))
 :latex)

