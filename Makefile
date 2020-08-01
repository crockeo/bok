.SILENT: default run clean


default:
	csc src/main.ss -o bok

run:
	csi -s src/main.ss

clean:
	rm bok
