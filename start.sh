#!/bin/sh

erl +A 4 +K true -name lilly@localhost -pa ./ebin -pa ./deps/*/ebin -boot start_sasl -s toolbar
