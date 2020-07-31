.SILENT: default run clean


default:
	csc src/main.ss -o bok

run: default
	./bok

clean:
	rm bok
