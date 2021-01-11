default: main.js

main.js: src/*
	elm make src/Main.elm --output output/main.js

deb: output/*
	rm debian/var/www/*
	cp output/* debian/var/www/
	dpkg-deb --build debian .
