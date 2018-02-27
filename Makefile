all: main.js

main.js: *.elm
	elm-make Main.elm --output=main.js
