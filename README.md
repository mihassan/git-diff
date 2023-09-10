# GitDiff

A simple haskell tool to parse the output of `git diff` command to JSON format.

## Usage

1. Install haskell tool stack using [ghcup](https://www.haskell.org/ghcup/).
2. Either run directly via `stack run` or install it via `stack install` as `GitDiff-exe`.
3. Pipe the output via this tool `git diff | GitDiff-exe`.

## Example

Consider the following output from `git diff`:

```
diff --git a/file1.txt b/file1.txt
index 168ed52..e41a4f5 100644
--- a/file1.txt
+++ b/file1.txt
@@ -1,6 +1,6 @@
 This is a sample file.
-This line is changed.
+These lines are part of the original content.
 Add a new line here.
 Another change in content.
-Additional lines in the middle.
+More lines in the original file.
 New content at the end of the file.
```

When run this output via `GitDiff-exe` we see the following JSON:

```
[
  {
    "chunks": [
      {
        "fromLineRange": {
          "from": 1,
          "length": 6
        },
        "lineDiffs": [
          {
            "ContextLine": "This is a sample file."
          },
          {
            "RemovedLine": "This line is changed."
          },
          {
            "AddedLine": "These lines are part of the original content."
          },
          {
            "ContextLine": "Add a new line here."
          },
          {
            "ContextLine": "Another change in content."
          },
          {
            "RemovedLine": "Additional lines in the middle."
          },
          {
            "AddedLine": "More lines in the original file."
          },
          {
            "ContextLine": "New content at the end of the file."
          }
        ],
        "toLineRange": {
          "from": 1,
          "length": 6
        }
      }
    ],
    "fromFile": "file1.txt",
    "headers": [
      "index 168ed52..e41a4f5 100644"
    ],
    "toFile": "file1.txt"
  }
]

```

## Limitations
1. This code does not handle many `git diff` output variations.
1. The header section is not parsed, rather just kept as a list ot strings.
1. It can not handle diff between more than 2 files.

