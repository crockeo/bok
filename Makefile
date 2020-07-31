.SILENT: default run clean


default:
	csc src/*.ss -o bok

run: default
	./bok

clean:
	rm bok
