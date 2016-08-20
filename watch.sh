./build.sh
fswatch -o Main.elm | xargs -n1 ./build.sh
