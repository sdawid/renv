
build/e: renv.rkt
	if [ ! -d build ]; then mkdir build; fi
	raco exe --orig-exe -o build/e renv.rkt

init:
	raco pkg install --skip-installed rash

clean:
	rm -rf build
