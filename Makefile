default: main.js

main.js: src/*
	elm make src/Main.elm --output output/index.html
