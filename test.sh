#!/bin/bash
cabal new-build || exit 1
EXE="$(cabal list-bin prob)"

fail=0
for f in ./examples/*.txt; do
    if [ -f "$f.out" ]; then
        if diff -q "$f.out" <(/usr/bin/time -p -o "$f.time" "$EXE" "$f" 2>&1); then
            echo -n PASS "$f" " "
            awk '/real/ { print $2 }' "$f.time"
        else
            echo FAIL "$f"
            fail=1
        fi
        rm -f "$f.time"
    fi
done

exit $fail
