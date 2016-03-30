#! /bin/bash
set -e

# Setup the build directory
mkdir -p build/
rm -f build/*
cp srcs/* build


# Run the commands:
hdt drop
hdt repl "justo" "GoodBye" --noapply
hdt apply --acceptall
hdt repl "Bye" "Night" --noapply
hdt apply --acceptall
hdt repl "Aenean" "woohoo" --noapply
hdt apply --acceptall


#hdt format
#hdt apply
#hdt repl "Nemo" "World"
#hdt drop

echo "Finished OK"
