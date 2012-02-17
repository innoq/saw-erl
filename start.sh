#!/bin/sh

erl +A 4 +K true -name sawl@localhost -pa ./ebin -pa ./deps/*/ebin -boot start_sasl -s toolbar -s reloader 
