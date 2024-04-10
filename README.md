# Radish

Radish is a modern concurrency-focused shell built with gleam and rust

Example code: (not enough has been implemented yet for this to work)

```sh
def find-lines [to-find] [
    # set line-conts to one line from stdin
    set line-conts (read)
    if (!= $line-conts -EOF) [ # if $line-conts is not the end of stdin, keep going
        if (string.contains $line-conts $to-find) [
            echo $line-conts
        ]
        # recursively loop through the function
        find-lines $to-find
    ]
]

# for each file that matches the glob **.{y,to}ml (every .toml or .yml file in the next directories) run everything in the brackets
# (glob "*.toml") -> ["gleam.toml", "stuff.toml", "askdjlfjkl.toml"]
list.map (glob "**.{y,to}ml") [x] [
    # spawn a seperate asynchronous process for reading each file
    & (| (cat x) (find-lines "cucumber"))
]
```
