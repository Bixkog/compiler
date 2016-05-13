
compile:
	swipl -s algol.pl -t main > out

tohex:
	 ./padhex out | xxd -p -r > out.sextium