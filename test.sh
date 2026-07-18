#!/bin/bash
cabal new-build || exit 1

fail=0
for f in ./examples/*.txt; do
    if [ -f "$f.out" ]; then
        if diff -q "$f.out" <(cabal new-run prob -- "$f" 2>&1); then
            echo PASS "$f"
        else
            echo FAIL "$f"
            fail=1
        fi
    fi
done

exit $fail
