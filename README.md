# kbkb

Static site of linked TeX files, found in `doc/`.

## DONE
- Convert directory of Latex into HTML + PDF
- Image/Tikz rendering
- Table of contents rendering
- Collapsible subsections
- Up / Prev / Next HTML buttons
- Preview on hover
- keyword parsing at the top (ORD, TAGS, TEX) would be good. Any more?
    - ORD is used to order the files/directories within a folder. 1-2-3-3.5-4, etc.
    - TAGS *could* be like "def" / "exercise"
    - TEX *could* specify nondefault things to compile LaTeX with
## TODO
- color code background based on tag
- Cache hash of tex before computing pdf, check before recomputing
- move buttons to a header
- List of other pages that link to the current page (and for each subsection?
  would need to make that togglable)
- 'Home page' (with overall ToC, search bar, references, ABOUT, filters)
    - search
    - filter by definition / proposition
- comment system + postgres
