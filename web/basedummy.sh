#! /bin/bash

sed -i -E "s/caml_failwith(\n[ ]+)?\(\"Base(.*)\"\)/return 1/g" ../_build/default/web/mttweb.js
