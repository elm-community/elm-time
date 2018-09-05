# Example Page Demonstrating Error Rendering

In this demonstration, you modify an **ISO8601** string in a text input
and see the parsed result.  If the string has no parsing or validation
errors, you see a grid of the parsed result.  Otherwise, an "Elm-style"
error message is shown, followed by the **parser**'s error structure
that the error renderer needed in order to interpret to generate
the error message.

## Installation

### Prerequisites

| **Name** | **Version** | **Installation** |
|---|---|---|
| elm | \>=&nbsp;0.19.0 | **[Install](https://guide.elm-lang.org/install.html)** |
| node | v8.9.3 | **[node dist](https://nodejs.org/dist/v8.9.3/)** |
| npm | 5.8.0 | Included as part of **node** install above |
| elm-live | \>=&nbsp;v2.7.5 | `npm install -g elm-live` |

### Install and Run

For this you need to clone `elm-community/elm-time` from GitHub
into your directory of other Elm projects as follows:

1. `cd` into your Elm projects' folder into which you want to clone
   `elm-time`.

1. Perform the following commands:
    ```bash
    git clone git@github.com:elm-community/elm-time.git
    cd elm-time
    elm-make --warn
    cd examples/with-parser-error-renderer
    elm-live --output=elm.js Main.elm --open --warn
    ```
1. Browse http://localhost:8000 and observe that the initial **ISO8601**
   string is parsed successfully into a row of labeled boxes.
1. In the input text edit control with the light-gray background,
   change the **ISO8601** string to make the parsing fail.
1. Hit the `Enter` key and observe the error message and parsing
   error structure in red.

Try different **ISO8601** string values and see what this page does
with them.
