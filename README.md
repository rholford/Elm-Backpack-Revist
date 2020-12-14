# Elm-Backpack-Revist
Revisit of DT project.

Original Code: Group 15, Fall 2019.

Initial Commit: Clone of old code, version submitted via avenue.

Compile with ```elm make src/Main.elm```, install dependencies with ```elm install <mising pacakge>```

## Dependencies/Versions
```json
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "MacCASOutreach/graphicsvg": "7.1.0",
            "elm/browser": "1.0.2",
            "elm/core": "1.0.2",
            "elm/html": "1.0.0",
            "elm-community/list-extra": "8.2.4",
            "elm-community/maybe-extra": "5.1.0",
            "justgage/tachyons-elm": "4.1.3",
            "tortus/elm-array-2d": "2.1.2"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/svg": "1.0.1",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
```

## Current functionality
The prototype appears to be able to algorithmically solve user inputted problems. (Changes below)

Remove button removes last item entered correctly.

Previous/Next now works. (Step by step)

Enable/Disable Buttons based on usability.

Removal button now can change position of removal.

Solve/steps bug fixed.

Added visible step counter.

Output is now an html table, and not a string. Fixes structuring/alignment for double digit and above outputs.

Step counter changes to display maximum value (ie solution) once completed.

Table actually looks like a table, now.

File cleanup.

Minor tweaks to various CSS related visual bits.

Added new buttons, first step and back.

Added colouring to table steps. Some text/format tweaks.

## Ideas?

Find the back/solve again bug.

Is more interactivity practical? 

NOTE: IF WHEN COMPILING IT GIVES VERSION ERROR, EITHER UPDATE OR BOOTLEG IT BY GOING INTO THE .json AND MODIFYING THE VERSION

