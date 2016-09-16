./build.sh
fswatch -o src/ build.sh | xargs -n1 ./build.sh
