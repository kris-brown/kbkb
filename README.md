# KBKB: Knowledge Base

A static site generated from a directory of TeX files. Currently hosted [here](https://web.stanford.edu/~ksb/phil/doc/phil.html).

## HOW TO USE

- Edit TeX files, found in `doc/`.
- `stack build --exec "kbkb <FLAGS>"` to compile and run
  - flag `gen` populates `site/`
  - flag `sync` ssh copies `site/` to where the website is hosted
  - flag `pdf` will generate pdfs (currently broken)
  - flag `clear` will wipe out the local and remote `site/` folders

### Caveats
- Don't use unicode, apostrophes, periods, slashes in titles of files (so do
  `Exercise 1-1`, rather than `Exercise 1.1`)
- Leading digits of file titles are stripped (these are just used to order)

## DONE
- Convert directory of Latex into HTML + PDF
- Image/Tikz diagram, table of contents, footnotes, bibliography rendering
- Collapsible subsections
- Up / Prev / Next / Random / Home HTML buttons
- Preview links on hover
- Flashcard mode
- List of other pages that link to the current page
  - Go back to the specific part that linked it
  - Optionally include a description of the context of the link
- Metadata parsing (file named `0` in a given folder)
    - Specify title, unique identifier, and tag.
    - Tags like "Def" / "Exercise" used to color the section
- Different interconvertable representations of the KB
  1. Directories of TeX files (good for writing long form)
  2. Normalized database (good for editing connections)
  3. Denormalized database (good for querying)

## TODO
- Parse markdown OR LaTeX
- Speed up html generation by reusing the html of subsections
  - trouble with footnotes/citations, though
- LaTeX errors are silent: prefer to fail noisily rather than just have blank
  space in final result
- Content: Brandom lectures, math textbooks
- Make uids case-insensitive
- Compute pdfs (will need different ToLatex function)
- search feature (possibly in <nav>)
    - filter by definition / proposition, etc.
- comment system (POST request online) and download comments to local filesystem
- mouseover delay 1 second (so it doesn't pop up immediately) and better
  placement of the pop up.
- Make the LaTeX compilation more parameterized (possibly with a TEX metadatum)
- Internal link fix currently requires us to have all three components.

## Inspirations
- [Workflowy](https://www.workflowy.com/features/) / [Nested](https://orteil.dashnet.org/nested)
- [Stacks project](https://stacks.math.columbia.edu/)
- [Maddox's personal website](https://maddo.xxx/)