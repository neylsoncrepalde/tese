(TeX-add-style-hook
 "project_lazega"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("abntex2" "a4paper" "12pt" "openright" "oneside" "german" "french" "brazil" "english")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "brazil") ("inputenc" "utf8") ("abntex2cite" "alf" "abnt-and-type=&") ("geometry" "a4paper" "left=2cm" "right=2cm" "top=2cm" "bottom=2cm")))
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
    "tipo-de-estudo"
    "metodos-usados"
    "white_1.2"
    "cost"
    "satisfaction"
    "S-theta"
    "exponents-abcd"
    "market-plane"
    "repertory-effect-correction"
    "satisfaction-corrected"
    "biencourt-quality"
    "biencourt-derivation"
    "regression"
    "N-com-pesos"
    "parameters"
    "eloire-abcd"
    "indicadores"
    "goals"
    "repertoire-peryear"
    "repertoire-perseries"
    "concert-consumption-models"
    "occupancy-table")
   (LaTeX-add-environments
    "hip")
   (LaTeX-add-bibliographies
    "BIBDOUTORADO"
    " abnt-options"))
 :latex)

